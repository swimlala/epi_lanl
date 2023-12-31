CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2022-01-23T14:19:26Z creation; 2022-09-06T18:25:48Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.6   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    8   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  8   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  8�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  9   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        9x   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    9�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    9�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  9�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    9�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    9�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  9�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  :   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  :T   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    :�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   axis      T      
resolution        >�E�r�_K   
_FillValue        A.�~            :�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    :�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        ?F�k"kmj   
_FillValue        A.�~            :�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   	valid_min         �V�        	valid_max         @V�        axis      Y      
_FillValue        @�i�            :�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    	valid_min         �f�        	valid_max         @f�        axis      X      
_FillValue        @�i�            :�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    :�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    :�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    :�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    ;    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        =    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  dh   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �H   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  Ҩ   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 9   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � @�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � `h   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � hH   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �|   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �8   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �@   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �`   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �h   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20220123141926  20220907192128  5905791 5905791 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               k   kAA  AOAO7825_008765_107                 7825_008765_107                 2C  2C  DD  SOLO_II                         SOLO_II                         8765                            8765                            V2.6; SBE602 06Mar19            V2.6; SBE602 06Mar19            853 853 @ٴb��@ٴb��11  @ٴ��Mj@ٴ��Mj@4�r\=�x@4�r\=�x�e)�(��$�e)�(��$11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?u?��H@:�H@}p�@�  @�  @޸RAG�AG�A   A+�A@  A`��A�Q�A�Q�A�  A�  A�  A�  A߮A�\)A��BQ�B(�B  B   B'�
B/�
B7�
B@  BH(�BP  BW�
B_�
Bg�
Bp  Bx  B�  B�(�B�{B�{B�{B��B��B�  B��B�  B��
B�  B�(�B�  B�(�B�{B��
B��B�  B��
B��B�{B�  B�{B�{B�  B�{B�{B�  B�  B��B�  C 
=C
=C  C{C
=C
  C�C  C
=C  C�C��C�C
=C�C��C   C"
=C$
=C&
=C(  C)��C,  C.  C/��C2
=C4  C5��C8
=C9��C;��C=��C?��CA��CD  CF  CH
=CJ
=CL  CN  CO��CQ��CT
=CU��CW�HCY�C[��C^  C`  Ca��Cd  Cf{Ch�Cj
=Ck��Cn  Cp
=Cr  Cs��Cu��Cw�Cy��C{��C~  C�  C�  C�  C�C�C�  C�  C�  C�C�C�
=C�C�C�C�  C���C��C���C�C���C���C�  C�  C�  C�  C���C���C���C���C���C�  C�C�C�C�C���C���C�  C�C�  C�C�  C�  C�  C�C�
=C�C���C���C���C�C�C�  C�C�C�  C�C�  C�  C�  C�  C���C�  C���C���C���C�  C���C��C���C�C�  C���C���C�C�C���C���C���C���C�C�C���C���C���C�  C�  C�  C�  C���C�C�C���C�  C�  C�  C�  C�  C�
=C�  C���C�C�C�C���C�  C�\C�C�C�C�C�C�C�C���C���C���C���C�  C���C�C�  C���C�  C���C���C���C�D   D � D  D}qD�qD}qD�qD��D�D}qD  D��D�D�D  D� D�Dz�D	  D	��D
  D
}qD
�qD}qD�qD}qD�qD}qD  D� D  D��D�D� D  D}qD�qD}qD  D��DD��D�D��D  D� D�qD}qD  D}qD��D� DD��D  D� D�D��D�D}qD  D�D�D}qD�qD � D!  D!��D"D"� D#  D#z�D#��D$}qD%  D%�D&�D&}qD'  D'� D(  D(� D(�qD)}qD)�qD*��D+�D+��D,�D,��D-�D-}qD-��D.}qD/�D/� D0  D0}qD0�qD1}qD1��D2z�D2��D3z�D3��D4z�D4��D5}qD5�qD6� D7�D7�D8�D8�D9  D9��D:  D:z�D:�qD;� D<  D<}qD<��D=� D>D>}qD?  D?�D@�D@}qD@�qDA� DB�DB�DC�DC��DDDD� DD�qDEz�DE�qDF� DF��DG}qDG�qDHz�DH��DI}qDI�qDJ��DK  DK� DL  DL� DM�DM}qDN  DN}qDN�qDO}qDO�qDP� DQDQ�DRDR��DS�DS}qDT  DT��DT�qDUz�DU�qDV}qDV�qDWz�DW�qDX}qDX��DY}qDZ  DZ}qDZ�qD[� D\  D\}qD\�qD]��D^  D^� D_�D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df��Dg�Dg}qDg�qDh� Di  Di��Dj�Dj� Dk�Dk� Dl  Dl� Dl�qDm}qDn  Dn� Dn�qDo��Dp�Dp��Dq�Dq}qDq�qDr}qDr�qDsz�Ds��Dt� Du  Du� Du�qDv}qDv�qDw� Dx�Dx�DyDy� Dz  Dz� D{  D{��D|  D|}qD|��D}� D~D~� D  D� D�  D�AHD�� D���D���D�>�D�� D��HD��D�AHD�}qD��)D���D�AHD�� D��HD�HD�AHD��HD�� D�  D�AHD��HD���D�  D�B�D��HD�� D���D�>�D�}qD�� D��D�@ D�� D�� D�  D�@ D��HD��HD�HD�@ D�� D�� D�HD�@ D�|)D���D�  D�>�D�� D��HD�  D�@ D�� D�� D�  D�@ D�� D�� D���D�>�D��HD�� D��)D�=qD�~�D��HD��D�@ D�~�D��HD�HD�AHD�~�D���D�HD�B�D���D��HD�HD�>�D�~�D�� D�  D�@ D�� D���D�HD�AHD�~�D�� D���D�>�D�� D�� D�  D�>�D�}qD�� D�HD�>�D�� D�� D�  D�@ D�� D�� D���D�<)D�}qD���D�  D�@ D�� D���D��qD�@ D�~�D��qD�  D�>�D�~�D�� D�HD�AHD�� D���D���D�@ D�� D�� D�  D�@ D��HD�� D�  D�>�D�~�D��HD�HD�>�D�~�D��HD�HD�AHD���D��HD�  D�@ D�� D��HD��D�@ D�~�D��qD�  D�AHD�� D���D��qD�@ D��HD�� D�HD�B�D��HD��HD��D�AHD�� D���D���D�@ D��HD�� D�  D�>�D�~�D���D�HD�B�D�~�D���D�  D�AHD���D�� D�  D�AHD��HD��HD�  D�@ D�� D�� D�HD�C�D��HD��HD�  D�AHD���D��HD�  D�@ D�� D�� D���D�>�D�}qD��qD�  D�>�D�|)D���D�HD�AHD��HD��HD���D�@ D�� D��qD��qD�>�D��HD��HD��qD�>�D�~�D���D���D�@ D�� D�� D�  D�AHD��HD���D��)D�=qD�~�D�� D��qD�<)D��HD�D��qD�>�D��HD���D�HD�@ D�� D�� D�  D�@ D�~�D���D�  D�>�D�� D��HD�HD�B�D�~�D¾�D�  D�@ DÀ DýqD���D�>�D�~�D�� D���D�=qD�}qDŽqD��qD�AHDƁHD��HD�HD�@ DǁHD�� D���D�@ D�~�D�� D��D�@ D�~�Dɾ�D���D�>�Dʀ D�� D��qD�>�DˁHD��HD�HD�>�D�~�D��HD�HD�AHD̀ D;�D�  D�AHD�~�D�� D��D�B�DρHD�� D�  D�@ DЁHD�� D���D�@ Dр D��HD�  D�@ DҁHD��HD�HD�>�DӀ D�� D�  D�@ DԁHD�� D���D�>�DՀ Dվ�D�  D�AHD�~�D��HD��D�AHD׀ D�� D�HD�AHD؀ Dؾ�D���D�@ DفHD�� D�  D�>�D�~�D�� D�  D�>�DہHD��HD�  D�@ D܀ Dܾ�D���D�@ D݀ Dݾ�D�HD�>�D�~�D޾�D�  D�@ D�~�D߾�D�  D�>�D�� D��HD�HD�@ D�~�D�� D�  D�@ D� D��HD�HD�>�D� D�� D�  D�>�D� D�� D�HD�@ D�~�D��HD�HD�@ D�~�D澸D���D�@ D�~�D羸D��qD�@ D�HD��HD�HD�@ D�~�D龸D��D�AHD�~�D꾸D�HD�B�D낏D��HD�  D�>�D�~�D�� D�HD�AHD�HD��HD�HD�>�D�}qD�� D�  D�=qD�~�D�� D�HD�AHD�� D�� D���D�@ D�HD��HD�HD�@ D� D�� D���D�>�D� D��HD�HD�AHD�HD���D���D�AHD�� D�� D�  D�@ D�� D�� D�  D�AHD�� D�� D�  D�@ D�� D��qD�  D�B�D��HD��HD�  D�@ D�p�?�?��?k�?��?�Q�?��@   @��@#�
@.{@G�@Q�@n{@xQ�@���@���@��H@��
@�{@�Q�@\@���@�z�@�G�@�ff@�@�(�Az�A�A{A�A�A(�A!G�A'
=A+�A0��A4z�A;�A?\)AEAI��AP  AS33AY��A]p�Ac�
Ag�Al��Ar�\AuA}p�A�Q�A�(�A�A���A��HA�p�A���A��\A�A�\)A��\A�z�A��A�=qA�z�A�
=A���A��
A�ffA���A�33A��RA�  A��A��A�Q�A��HA���AǮA��A���A�
=A��A��
A�\)A���A�z�A�{A�G�A�A�A���A��HA�{A�A��HA�z�A�Q�A��A�z�A�
=B ��B=qB
=B��B��B\)B(�B	B
�RB  Bp�BffB  B��B�\B\)B��B�B�B��B�B\)B(�B�B�RB Q�B!��B"�RB$(�B%�B&�HB'�B)G�B*=qB+�B,��B.=qB/�B0��B2=qB3
=B4��B5B733B8��B9��B;33B<(�B=�B>�HB@Q�BA��BB�HBDQ�BEG�BG
=BH  BI��BJ�HBK�
BMp�BN�\BP(�BQG�BR�\BT  BT��BV�RBW�BYG�BZffB[�B]�B^{B_�B`��Bb{Bc\)Bd��Bf{Bg
=Bh��Bi��Bk\)BlQ�BmBo
=BpQ�BqBr�RBtz�Bu��Bv�HBxz�ByG�B{
=B|Q�B}��B
=B�{B��HB��B�(�B�
=B��B�ffB���B��B�ffB���B��
B�Q�B��B�B�ffB�G�B�B���B�G�B��B���B�33B�(�B��RB�p�B�=qB��RB���B�Q�B���B�B�=qB�33B��
B�Q�B�G�B��
B��\B�\)B��B���B�p�B�{B���B��B�ffB�
=B��B���B�33B�  B��RB�\)B�Q�B��HB��B�z�B�
=B�  B��\B��B�(�B��HB��
B�Q�B�G�B�  B��\B��B�(�B��HB�B�Q�B�33B��
B��\B�p�B�  B���B��B�Q�B�
=B�  B��\B�p�B�(�B��HB�B�ffB��B�{B£�B�p�B�ffB���B��
BƸRB�G�B�=qB�
=Bə�B�z�B�G�B��B��HBͮB�=qB�33B��BУ�Bљ�B�=qB�
=B�  Bԣ�B�\)B�ffB��HB��
Bأ�B�G�B�=qB���Bۙ�B܏\B�\)B��B���B߅B�Q�B��B�B��B�p�B�{B���B�B�Q�B�\)B�  B�RB�B�Q�B��B�{B��B�p�B�Q�B���B�B�ffB�33B�B�RB�B�  B���B��B�Q�B�G�B��B��\B��B�{B���B�B�ffB�
=B�  B��\B�\)C �C \)C �
C=qC�C��CffC��C{Cz�CC33C��C�HCG�C�RC  Cp�C�HC(�C�\C  CG�CC	�C	p�C	�C
G�C
��C
=Cz�C�
C(�C�C  C\)C��C33C�C��CffC�RC(�C��C�CG�CC�Cp�C�CG�C��C
=Cz�C��C(�C��C�CQ�CC�Cp�C�HC33Cz�C�C33Cp�C�
C{CG�C��C�
C
=CQ�C�\C�C�HC33CQ�Cz�C��C  C�C\)C��C��C��C33Cp�C�\C�
C�C33CffC�C��C 
=C Q�C p�C ��C �HC!
=C!33C!p�C!�RC!�
C"
=C"\)C"z�C"��C"��C#(�C#G�C#�C#C#�HC$�C$ffC$�\C$�RC%  C%=qC%\)C%��C%�HC&  C&=qC&�C&��C&�
C'(�C'Q�C'z�C'��C'��C(�C(ffC(��C(C)
=C)G�C)p�C)��C)�C*�C*=qC*�C*C*�C+{C+ffC+��C+�RC,  C,=qC,ffC,�\C,�HC-{C-33C-p�C-C-�HC.
=C.\)C.�\C.�RC.�C/=qC/ffC/�\C/�
C0
=C033C0p�C0�RC0�HC1�C1ffC1�\C1C2
=C2G�C2p�C2��C2��C333C3\)C3�\C3�
C4�C4G�C4z�C4�RC5  C5(�C5\)C5�C5�
C6
=C6G�C6�\C6�RC6�C733C7p�C7��C7��C8
=C8\)C8�\C8�C8�C9=qC9p�C9��C9�
C:�C:\)C:�C:�RC;
=C;G�C;z�C;��C;�HC<(�C<p�C<��C<��C={C=\)C=�\C=�RC>
=C>Q�C>z�C>�RC?
=C?G�C?p�C?�RC@
=C@G�C@z�C@�RCA
=CAQ�CA�CA�RCB{CBQ�CBz�CB�RCC{CCQ�CCz�CCCD{CD\)CD��CDCE
=CE\)CE��CE�
CF  CFQ�CF��CF�HCG
=CGG�CG��CG�CH�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                      ?u?��H@:�H@}p�@�  @�  @޸RAG�AG�A   A+�A@  A`��A�Q�A�Q�A�  A�  A�  A�  A߮A�\)A��BQ�B(�B  B   B'�
B/�
B7�
B@  BH(�BP  BW�
B_�
Bg�
Bp  Bx  B�  B�(�B�{B�{B�{B��B��B�  B��B�  B��
B�  B�(�B�  B�(�B�{B��
B��B�  B��
B��B�{B�  B�{B�{B�  B�{B�{B�  B�  B��B�  C 
=C
=C  C{C
=C
  C�C  C
=C  C�C��C�C
=C�C��C   C"
=C$
=C&
=C(  C)��C,  C.  C/��C2
=C4  C5��C8
=C9��C;��C=��C?��CA��CD  CF  CH
=CJ
=CL  CN  CO��CQ��CT
=CU��CW�HCY�C[��C^  C`  Ca��Cd  Cf{Ch�Cj
=Ck��Cn  Cp
=Cr  Cs��Cu��Cw�Cy��C{��C~  C�  C�  C�  C�C�C�  C�  C�  C�C�C�
=C�C�C�C�  C���C��C���C�C���C���C�  C�  C�  C�  C���C���C���C���C���C�  C�C�C�C�C���C���C�  C�C�  C�C�  C�  C�  C�C�
=C�C���C���C���C�C�C�  C�C�C�  C�C�  C�  C�  C�  C���C�  C���C���C���C�  C���C��C���C�C�  C���C���C�C�C���C���C���C���C�C�C���C���C���C�  C�  C�  C�  C���C�C�C���C�  C�  C�  C�  C�  C�
=C�  C���C�C�C�C���C�  C�\C�C�C�C�C�C�C�C���C���C���C���C�  C���C�C�  C���C�  C���C���C���C�D   D � D  D}qD�qD}qD�qD��D�D}qD  D��D�D�D  D� D�Dz�D	  D	��D
  D
}qD
�qD}qD�qD}qD�qD}qD  D� D  D��D�D� D  D}qD�qD}qD  D��DD��D�D��D  D� D�qD}qD  D}qD��D� DD��D  D� D�D��D�D}qD  D�D�D}qD�qD � D!  D!��D"D"� D#  D#z�D#��D$}qD%  D%�D&�D&}qD'  D'� D(  D(� D(�qD)}qD)�qD*��D+�D+��D,�D,��D-�D-}qD-��D.}qD/�D/� D0  D0}qD0�qD1}qD1��D2z�D2��D3z�D3��D4z�D4��D5}qD5�qD6� D7�D7�D8�D8�D9  D9��D:  D:z�D:�qD;� D<  D<}qD<��D=� D>D>}qD?  D?�D@�D@}qD@�qDA� DB�DB�DC�DC��DDDD� DD�qDEz�DE�qDF� DF��DG}qDG�qDHz�DH��DI}qDI�qDJ��DK  DK� DL  DL� DM�DM}qDN  DN}qDN�qDO}qDO�qDP� DQDQ�DRDR��DS�DS}qDT  DT��DT�qDUz�DU�qDV}qDV�qDWz�DW�qDX}qDX��DY}qDZ  DZ}qDZ�qD[� D\  D\}qD\�qD]��D^  D^� D_�D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df��Dg�Dg}qDg�qDh� Di  Di��Dj�Dj� Dk�Dk� Dl  Dl� Dl�qDm}qDn  Dn� Dn�qDo��Dp�Dp��Dq�Dq}qDq�qDr}qDr�qDsz�Ds��Dt� Du  Du� Du�qDv}qDv�qDw� Dx�Dx�DyDy� Dz  Dz� D{  D{��D|  D|}qD|��D}� D~D~� D  D� D�  D�AHD�� D���D���D�>�D�� D��HD��D�AHD�}qD��)D���D�AHD�� D��HD�HD�AHD��HD�� D�  D�AHD��HD���D�  D�B�D��HD�� D���D�>�D�}qD�� D��D�@ D�� D�� D�  D�@ D��HD��HD�HD�@ D�� D�� D�HD�@ D�|)D���D�  D�>�D�� D��HD�  D�@ D�� D�� D�  D�@ D�� D�� D���D�>�D��HD�� D��)D�=qD�~�D��HD��D�@ D�~�D��HD�HD�AHD�~�D���D�HD�B�D���D��HD�HD�>�D�~�D�� D�  D�@ D�� D���D�HD�AHD�~�D�� D���D�>�D�� D�� D�  D�>�D�}qD�� D�HD�>�D�� D�� D�  D�@ D�� D�� D���D�<)D�}qD���D�  D�@ D�� D���D��qD�@ D�~�D��qD�  D�>�D�~�D�� D�HD�AHD�� D���D���D�@ D�� D�� D�  D�@ D��HD�� D�  D�>�D�~�D��HD�HD�>�D�~�D��HD�HD�AHD���D��HD�  D�@ D�� D��HD��D�@ D�~�D��qD�  D�AHD�� D���D��qD�@ D��HD�� D�HD�B�D��HD��HD��D�AHD�� D���D���D�@ D��HD�� D�  D�>�D�~�D���D�HD�B�D�~�D���D�  D�AHD���D�� D�  D�AHD��HD��HD�  D�@ D�� D�� D�HD�C�D��HD��HD�  D�AHD���D��HD�  D�@ D�� D�� D���D�>�D�}qD��qD�  D�>�D�|)D���D�HD�AHD��HD��HD���D�@ D�� D��qD��qD�>�D��HD��HD��qD�>�D�~�D���D���D�@ D�� D�� D�  D�AHD��HD���D��)D�=qD�~�D�� D��qD�<)D��HD�D��qD�>�D��HD���D�HD�@ D�� D�� D�  D�@ D�~�D���D�  D�>�D�� D��HD�HD�B�D�~�D¾�D�  D�@ DÀ DýqD���D�>�D�~�D�� D���D�=qD�}qDŽqD��qD�AHDƁHD��HD�HD�@ DǁHD�� D���D�@ D�~�D�� D��D�@ D�~�Dɾ�D���D�>�Dʀ D�� D��qD�>�DˁHD��HD�HD�>�D�~�D��HD�HD�AHD̀ D;�D�  D�AHD�~�D�� D��D�B�DρHD�� D�  D�@ DЁHD�� D���D�@ Dр D��HD�  D�@ DҁHD��HD�HD�>�DӀ D�� D�  D�@ DԁHD�� D���D�>�DՀ Dվ�D�  D�AHD�~�D��HD��D�AHD׀ D�� D�HD�AHD؀ Dؾ�D���D�@ DفHD�� D�  D�>�D�~�D�� D�  D�>�DہHD��HD�  D�@ D܀ Dܾ�D���D�@ D݀ Dݾ�D�HD�>�D�~�D޾�D�  D�@ D�~�D߾�D�  D�>�D�� D��HD�HD�@ D�~�D�� D�  D�@ D� D��HD�HD�>�D� D�� D�  D�>�D� D�� D�HD�@ D�~�D��HD�HD�@ D�~�D澸D���D�@ D�~�D羸D��qD�@ D�HD��HD�HD�@ D�~�D龸D��D�AHD�~�D꾸D�HD�B�D낏D��HD�  D�>�D�~�D�� D�HD�AHD�HD��HD�HD�>�D�}qD�� D�  D�=qD�~�D�� D�HD�AHD�� D�� D���D�@ D�HD��HD�HD�@ D� D�� D���D�>�D� D��HD�HD�AHD�HD���D���D�AHD�� D�� D�  D�@ D�� D�� D�  D�AHD�� D�� D�  D�@ D�� D��qD�  D�B�D��HD��HD�  D�@ G�O�?�?��?k�?��?�Q�?��@   @��@#�
@.{@G�@Q�@n{@xQ�@���@���@��H@��
@�{@�Q�@\@���@�z�@�G�@�ff@�@�(�Az�A�A{A�A�A(�A!G�A'
=A+�A0��A4z�A;�A?\)AEAI��AP  AS33AY��A]p�Ac�
Ag�Al��Ar�\AuA}p�A�Q�A�(�A�A���A��HA�p�A���A��\A�A�\)A��\A�z�A��A�=qA�z�A�
=A���A��
A�ffA���A�33A��RA�  A��A��A�Q�A��HA���AǮA��A���A�
=A��A��
A�\)A���A�z�A�{A�G�A�A�A���A��HA�{A�A��HA�z�A�Q�A��A�z�A�
=B ��B=qB
=B��B��B\)B(�B	B
�RB  Bp�BffB  B��B�\B\)B��B�B�B��B�B\)B(�B�B�RB Q�B!��B"�RB$(�B%�B&�HB'�B)G�B*=qB+�B,��B.=qB/�B0��B2=qB3
=B4��B5B733B8��B9��B;33B<(�B=�B>�HB@Q�BA��BB�HBDQ�BEG�BG
=BH  BI��BJ�HBK�
BMp�BN�\BP(�BQG�BR�\BT  BT��BV�RBW�BYG�BZffB[�B]�B^{B_�B`��Bb{Bc\)Bd��Bf{Bg
=Bh��Bi��Bk\)BlQ�BmBo
=BpQ�BqBr�RBtz�Bu��Bv�HBxz�ByG�B{
=B|Q�B}��B
=B�{B��HB��B�(�B�
=B��B�ffB���B��B�ffB���B��
B�Q�B��B�B�ffB�G�B�B���B�G�B��B���B�33B�(�B��RB�p�B�=qB��RB���B�Q�B���B�B�=qB�33B��
B�Q�B�G�B��
B��\B�\)B��B���B�p�B�{B���B��B�ffB�
=B��B���B�33B�  B��RB�\)B�Q�B��HB��B�z�B�
=B�  B��\B��B�(�B��HB��
B�Q�B�G�B�  B��\B��B�(�B��HB�B�Q�B�33B��
B��\B�p�B�  B���B��B�Q�B�
=B�  B��\B�p�B�(�B��HB�B�ffB��B�{B£�B�p�B�ffB���B��
BƸRB�G�B�=qB�
=Bə�B�z�B�G�B��B��HBͮB�=qB�33B��BУ�Bљ�B�=qB�
=B�  Bԣ�B�\)B�ffB��HB��
Bأ�B�G�B�=qB���Bۙ�B܏\B�\)B��B���B߅B�Q�B��B�B��B�p�B�{B���B�B�Q�B�\)B�  B�RB�B�Q�B��B�{B��B�p�B�Q�B���B�B�ffB�33B�B�RB�B�  B���B��B�Q�B�G�B��B��\B��B�{B���B�B�ffB�
=B�  B��\B�\)C �C \)C �
C=qC�C��CffC��C{Cz�CC33C��C�HCG�C�RC  Cp�C�HC(�C�\C  CG�CC	�C	p�C	�C
G�C
��C
=Cz�C�
C(�C�C  C\)C��C33C�C��CffC�RC(�C��C�CG�CC�Cp�C�CG�C��C
=Cz�C��C(�C��C�CQ�CC�Cp�C�HC33Cz�C�C33Cp�C�
C{CG�C��C�
C
=CQ�C�\C�C�HC33CQ�Cz�C��C  C�C\)C��C��C��C33Cp�C�\C�
C�C33CffC�C��C 
=C Q�C p�C ��C �HC!
=C!33C!p�C!�RC!�
C"
=C"\)C"z�C"��C"��C#(�C#G�C#�C#C#�HC$�C$ffC$�\C$�RC%  C%=qC%\)C%��C%�HC&  C&=qC&�C&��C&�
C'(�C'Q�C'z�C'��C'��C(�C(ffC(��C(C)
=C)G�C)p�C)��C)�C*�C*=qC*�C*C*�C+{C+ffC+��C+�RC,  C,=qC,ffC,�\C,�HC-{C-33C-p�C-C-�HC.
=C.\)C.�\C.�RC.�C/=qC/ffC/�\C/�
C0
=C033C0p�C0�RC0�HC1�C1ffC1�\C1C2
=C2G�C2p�C2��C2��C333C3\)C3�\C3�
C4�C4G�C4z�C4�RC5  C5(�C5\)C5�C5�
C6
=C6G�C6�\C6�RC6�C733C7p�C7��C7��C8
=C8\)C8�\C8�C8�C9=qC9p�C9��C9�
C:�C:\)C:�C:�RC;
=C;G�C;z�C;��C;�HC<(�C<p�C<��C<��C={C=\)C=�\C=�RC>
=C>Q�C>z�C>�RC?
=C?G�C?p�C?�RC@
=C@G�C@z�C@�RCA
=CAQ�CA�CA�RCB{CBQ�CBz�CB�RCC{CCQ�CCz�CCCD{CD\)CD��CDCE
=CE\)CE��CE�
CF  CFQ�CF��CF�HCG
=CGG�CG��CG�CH�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                      @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�r�A�n�A�n�A�n�A�r�A�x�A�z�A�z�A�|�A�x�A�x�A�x�A�x�A�l�A�I�A�7LA�
=Aʺ^A��Aɩ�AɋDA�hsA�{AȬA�\)A�"�A��A�z�A�;dA��A�{A�  A��TA�ƨA�bNA�p�A�7LA�XA��A��yA�oA��HA�A��#A�VA��`A�bNA��TA�33A�O�A��A��A��+A�&�A�I�A���A���A��HA��yA�|�A�1'A���A��-A��^A�=qA�E�A�A�A�+A�ĜA��yA���A���A�dZA�`BA���A���A�9XA���A�=qA��A��9A��A�7LA���A�p�A�XA�{A��jA���A��A���A�%A�  A��A�n�A�S�A�Q�A��`A��\A��+A�-A�=qA�
=At�A~^5A|��A{K�Az�/AyAxM�Aw\)AvA�AudZAq�;Ao�-Aox�Ao
=An��AmdZAl�+AkO�Aj�Aj�DAh��Ad��AaC�A[C�AU�7AT�9AT=qAS�ARȴARVAQ��AP�AN9XAL�HAK�AK?}AJA�AH��AEVAC�AB�AB�jABv�AA�;AA�AA��AA�AAS�A?�-A;t�A9O�A8�+A8=qA7K�A5�wA4z�A3\)A29XA1�A0��A/�PA.�yA-�
A-C�A,ffA+��A*{A(M�A'�hA'l�A'%A&VA%��A%K�A$��A$A#t�A"��A"  A!��A!t�A �jAE�A�A%A�FAȴA�TA�HA;dA�AbA|�A�9AffA9XA�A�#A��A�Al�A7LA�A�yAȴAn�AbA�A�+A��Al�A�A%A
=qA	oA^5A��A��A��AAJA&�A-A�
A��A�7A&�A ~�@��h@�J@��@���@���@�r�@�Z@�A�@��;@�@�S�@���@���@�V@�@��`@�r�@��@��`@�ff@��/@��@�F@�l�@�K�@��@�+@�^@�Q�@��y@�-@��@�r�@���@ߍP@���@��@�o@١�@��@���@ج@أ�@ץ�@֗�@�-@�`B@���@ԣ�@� �@ҸR@��@��@�@�G�@�\)@�=q@�/@ǍP@��@�l�@�x�@�G�@�?}@�bN@�Z@�1'@��P@�"�@���@�M�@��@��@��`@�b@�+@���@��\@�@�%@�bN@��@��P@�+@��y@��\@�p�@�j@���@�"�@���@���@���@�5?@��h@���@���@�(�@���@�K�@���@���@�v�@�n�@�ff@�M�@�-@�{@���@��j@�9X@��m@�ƨ@���@�t�@�K�@�"�@���@�v�@�M�@�$�@�$�@�{@�x�@�hs@�?}@���@���@�A�@�  @���@�l�@���@�{@��@���@���@�x�@�?}@��`@�1@�;d@�
=@���@���@��+@�n�@�V@�E�@�=q@�-@��@��@��7@�hs@�`B@�`B@�O�@�/@���@���@��@��@�Q�@� �@�ƨ@�o@���@�^5@�ff@�E�@�{@�J@��@��T@���@���@��h@�`B@�G�@�&�@�Ĝ@��@�"�@�@��H@�;d@�S�@�\)@�\)@�dZ@�\)@�S�@�K�@�;d@�33@�+@���@���@�v�@�^5@�E�@�=q@�$�@�@��@���@�@���@���@��@�`B@�&�@���@��9@�j@�9X@�S�@�-@���@�/@��9@�1'@��m@���@���@��7@�/@��`@���@��D@�bN@��
@��@�t�@�t�@�l�@�S�@�33@�"�@��@�
=@�@���@��y@�-@��7@��@�x�@�p�@�hs@�p�@�hs@��@��`@��@�A�@���@���@�
=@���@�J@���@��T@���@���@�O�@��@��@�V@���@���@�Ĝ@�j@��m@�K�@��+@��@���@���@���@��h@��h@��h@��7@�X@���@���@��@��/@��9@���@��@�j@�b@�t�@�K�@�+@�
=@���@���@�v�@�-@�{@�@�?}@���@�1'@��@~��@~@|��@|I�@|1@|1@{�F@{33@zJ@x��@w�;@w\)@v�y@v�R@vff@v@u`B@t��@t�j@t�@st�@so@r��@rJ@pĜ@pb@o�w@o|�@oK�@o+@o
=@n�+@n$�@n$�@n@m��@m@m�-@m��@m�h@m/@l�D@lz�@l9X@k��@k��@kt�@kC�@k"�@ko@j�@j��@j=q@iX@hQ�@gl�@f��@f$�@f{@f@f5?@e�@e�h@e�@eO�@d�@d�/@d�/@d�/@d�/@d�j@dZ@c�m@c�
@c��@c�
@c��@ct�@cdZ@cS�@cC�@co@b�@b��@b�!@b^5@b-@b�@a��@aG�@`�`@`�@`1'@_�;@_|�@^��@^V@]��@\�@\��@\I�@\�@[��@[��@[@Z��@Z^5@Z-@Y��@Y��@YX@Y7L@X��@XĜ@XĜ@X��@XbN@XQ�@X1'@Xb@W�@W�w@W|�@W+@V�y@V��@Vv�@Vff@VV@V$�@Up�@T��@T�/@T�j@T��@T�D@TZ@T�@S��@Rn�@Qhs@Q�@P�`@P�u@P1'@Pb@O�@O��@O|�@O;d@O;d@O;d@O;d@O+@O
=@N�y@Nv�@M�@M@Mp�@L�@K@J��@J�H@J��@J��@J��@J��@J��@J��@J��@J�\@J~�@J=q@JJ@I�@I�#@I�^@I��@I7L@I�@H�`@H��@H�@HQ�@H1'@G�@G�w@Gl�@F��@F�y@F�@F�@F�@F�R@F$�@F5?@F$�@E�@E�T@E@E��@E/@D�@D1@C�F@C�@C"�@B��@A��@AX@A7L@A�@@r�@>��@>�+@>v�@>ff@>$�@=�-@=/@=V@<�j@<j@<I�@;�
@;��@;t�@;C�@;o@:��@:n�@:�@97L@8bN@8 �@7�;@7;d@7
=@6�y@6�y@6�@6�@6�R@6��@6��@5�-@4�j@4z�@4Z@49X@4(�@4�@3�m@3�F@3��@3��@3t�@2�H@2J@1��@1�@1�^@1�7@1X@1X@1X@1X@1X@1%@0�`@0��@0�@0�@0Q�@01'@0A�@0A�@01'@0b@/�;@/|�@.��@.ff@.V@.V@.E�@.$�@-@-��@-O�@,��@,�@,z�@,j@,I�@,1@+o@)�^@)&�@(��@(��@(��@(Q�@(b@'��@'|�@'l�@'\)@'K�@';d@'�@&�+@%�@%/@%V@$j@$1@#�F@#t�@#S�@#33@"�@"�\@"J@!X@!&�@ Ĝ@��@\)@;d@+@�@��@ȴ@��@��@v�@5?@{@�-@/@��@�@z�@Z@Z@9X@(�@1@1@1@1@1@��@ƨ@t�@�@n�@^5@-@�@�u@1'@b@  @  @�w@�y@�R@��@�+@�+@�+@�T@p�@/@V@�j@I�@(�@ƨ@t�@C�@~�@=q@�@J@��@�@��@hs@7L@%@�`@��@Ĝ@��@Ĝ@�9@�9@��@��@�9@�@Q�@1'@�@��@\)@
=@ȴ@v�@V@$�@@��@��@p�@O�@?}@/@/@�@�@V@��@�@��@j@��@t�@33@o@o@@@@
�@
�@
�@
�@
�@
�@
�@
�H@
��@
��@
�\@
M�@
-@
-@
J@	�7@	X@	X@	G�@	�@�9@�@bN@Q�@A�@ �@b@  @��@�wA�hsA�z�A�p�A�t�A�p�A�t�A�n�A�hsA�dZA�r�A�p�A�r�A�p�A�l�A�n�A�n�A�r�A�r�A�v�A�v�A�|�A�r�A�|�A�x�A�z�A�z�A�z�A�|�A�x�A�|�A�z�A�~�A�x�A�z�A�t�A�|�A�v�A�|�A�t�A�x�A�r�A�z�A�v�A�x�A�x�A�z�A�|�A�v�A�x�A�t�A�t�A�n�A�r�A�p�A�XA�S�A�M�A�K�A�9XA�I�A�G�A�K�A�A�A�A�A�?}A�7LA�+A��A�$�A�oA�VA�1A�%A��A��HA��/A���Aʺ^Aʰ!Aʧ�A�n�A�G�A� �A�{A�VA�A��A�ȴAɮAɥ�Aɡ�Aə�Aɝ�AɓuAɓuAɍPAɍPAɇ+A�~�A�|�A�v�A�v�A�l�A�l�A�`BA�K�A�33A�/A�"�A�oA�%A���A���A��#A���Aȡ�Aȝ�Aȏ\Aȇ+A�~�A�n�A�dZA�O�A�I�A�=qA�9XA�1'A�$�A�"�A��A�oA�A���A��A��TA�ȴAǮAǧ�AǙ�AǏ\A�|�A�r�A�XA�ZA�S�A�O�A�?}A�33A�/A�$�A��A��A�{A��A�oA��A�oA��A��A��A��A�1A�
=A�A�%A�A���A���A��A��A��mA��`A��/A���A���A���A���A���A�ȴA���AƬAƩ�AƋDA�hsA�XA�G�A��A��A��HA�ĜA�|�A�+A���A�1'Aé�A�?}A���A��`A�ƨA©�APA�p�A�bNA�I�A���A���A�A�A���A��RA���A���A��DA�t�A�G�A��A�\)A���A�JA�|�A�{A���A�^5A�%A�hsA�A���A���A�z�A�/A��A��HA��-A�ffA�K�A��A��A�1A���A���A��uA��\A�p�A�?}A�5?A�7LA�=qA��A�%A���A���A���A���A��7A�jA�K�A��A�{A�JA��A�ƨA��RA��A��+A�VA�/A�  A��^A��PA�ZA�A�A�A�A�A�A�5?A�5?A�+A� �A�%A��/A���A�I�A���A���A�7LA�%A���A��uA�ffA�+A��wA�~�A�I�A�  A���A�hsA���A��jA�"�A��uA��#A�ƨA���A���A�p�A�VA�&�A��A��FA��uA�dZA�$�A���A��mA��
A���A�A�A�-A���A��A���A���A��A�~�A�p�A�l�A�XA�C�A�9XA�/A�{A�
=A��yA���A���A�?}A�bA�  A��/A���A�x�A�S�A�oA��A�ȴA��PA�?}A�7LA�G�A�;dA�=qA�C�A�A�A�=qA�I�A�C�A�E�A�O�A�O�A�;dA�G�A�=qA�/A�/A�1'A� �A�-A�+A��A�1A�  A��RA�~�A�&�A��A�ZA��yA��PA�?}A�JA��mA��RA��\A�bNA���A��A� �A��
A�x�A�
=A��;A���A�n�A��A��A��FA���A�jA�9XA� �A�
=A��#A�5?A�$�A�oA�VA���A��9A�ffA�Q�A�7LA�ƨA�dZA��;A��A�p�A�"�A���A�`BA�M�A�O�A�K�A�?}A�;dA�7LA�/A�1A���A���A���A�x�A�A��A��wA�x�A�^5A�M�A�?}A���A��yA��^A���A��PA�z�A�hsA�=qA��A��A�ƨA��!A���A���A���A���A��uA��uA���A���A��uA��uA���A�l�A�`BA�ffA�ffA�\)A�^5A�bNA�XA�ZA�\)A�XA�ZA�\)A�\)A�\)A�I�A�G�A�VA�=qA�?}A�+A��A�oA�A��A��`A��HA�ȴA��A���A���A��wA��jA��9A��FA���A��7A�9XA�bA��A��RA��!A��A���A���A��DA�x�A�jA�\)A�C�A�&�A�JA��;A���A�ȴA�A��FA��!A��!A���A���A���A���A���A�|�A�r�A�K�A� �A�%A���A��HA���A���A���A��wA��jA��^A���A�VA��A��mA��A���A��!A�bNA�"�A�JA��/A�XA�1'A� �A��A�1'A��A�A��A��uA�dZA�E�A�-A��A���A���A��A���A� �A���A��yA��/A���A��^A���A��hA�z�A�dZA�`BA�Q�A�$�A��#A��-A�t�A�5?A��A�A���A���A��A�bNA�=qA�A��;A�ƨA���A�t�A�ZA�+A�(�A�+A��A��A���A��\A�t�A�5?A�JA��HA��RA��\A��A�r�A�9XA�
=A��!A��PA�^5A�5?A��`A�x�A��A��A��A�bNA�I�A�;dA�-A��A�A��AƨA��AA�A��A��Ax�A`BAK�A"�A~��A~�DA~r�A~ffA~ZA~M�A~=qA~{A}�TA}��A}p�A}�A|�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                      A�r�A�n�A�n�A�n�A�r�A�x�A�z�A�z�A�|�A�x�A�x�A�x�A�x�A�l�A�I�A�7LA�
=Aʺ^A��Aɩ�AɋDA�hsA�{AȬA�\)A�"�A��A�z�A�;dA��A�{A�  A��TA�ƨA�bNA�p�A�7LA�XA��A��yA�oA��HA�A��#A�VA��`A�bNA��TA�33A�O�A��A��A��+A�&�A�I�A���A���A��HA��yA�|�A�1'A���A��-A��^A�=qA�E�A�A�A�+A�ĜA��yA���A���A�dZA�`BA���A���A�9XA���A�=qA��A��9A��A�7LA���A�p�A�XA�{A��jA���A��A���A�%A�  A��A�n�A�S�A�Q�A��`A��\A��+A�-A�=qA�
=At�A~^5A|��A{K�Az�/AyAxM�Aw\)AvA�AudZAq�;Ao�-Aox�Ao
=An��AmdZAl�+AkO�Aj�Aj�DAh��Ad��AaC�A[C�AU�7AT�9AT=qAS�ARȴARVAQ��AP�AN9XAL�HAK�AK?}AJA�AH��AEVAC�AB�AB�jABv�AA�;AA�AA��AA�AAS�A?�-A;t�A9O�A8�+A8=qA7K�A5�wA4z�A3\)A29XA1�A0��A/�PA.�yA-�
A-C�A,ffA+��A*{A(M�A'�hA'l�A'%A&VA%��A%K�A$��A$A#t�A"��A"  A!��A!t�A �jAE�A�A%A�FAȴA�TA�HA;dA�AbA|�A�9AffA9XA�A�#A��A�Al�A7LA�A�yAȴAn�AbA�A�+A��Al�A�A%A
=qA	oA^5A��A��A��AAJA&�A-A�
A��A�7A&�A ~�@��h@�J@��@���@���@�r�@�Z@�A�@��;@�@�S�@���@���@�V@�@��`@�r�@��@��`@�ff@��/@��@�F@�l�@�K�@��@�+@�^@�Q�@��y@�-@��@�r�@���@ߍP@���@��@�o@١�@��@���@ج@أ�@ץ�@֗�@�-@�`B@���@ԣ�@� �@ҸR@��@��@�@�G�@�\)@�=q@�/@ǍP@��@�l�@�x�@�G�@�?}@�bN@�Z@�1'@��P@�"�@���@�M�@��@��@��`@�b@�+@���@��\@�@�%@�bN@��@��P@�+@��y@��\@�p�@�j@���@�"�@���@���@���@�5?@��h@���@���@�(�@���@�K�@���@���@�v�@�n�@�ff@�M�@�-@�{@���@��j@�9X@��m@�ƨ@���@�t�@�K�@�"�@���@�v�@�M�@�$�@�$�@�{@�x�@�hs@�?}@���@���@�A�@�  @���@�l�@���@�{@��@���@���@�x�@�?}@��`@�1@�;d@�
=@���@���@��+@�n�@�V@�E�@�=q@�-@��@��@��7@�hs@�`B@�`B@�O�@�/@���@���@��@��@�Q�@� �@�ƨ@�o@���@�^5@�ff@�E�@�{@�J@��@��T@���@���@��h@�`B@�G�@�&�@�Ĝ@��@�"�@�@��H@�;d@�S�@�\)@�\)@�dZ@�\)@�S�@�K�@�;d@�33@�+@���@���@�v�@�^5@�E�@�=q@�$�@�@��@���@�@���@���@��@�`B@�&�@���@��9@�j@�9X@�S�@�-@���@�/@��9@�1'@��m@���@���@��7@�/@��`@���@��D@�bN@��
@��@�t�@�t�@�l�@�S�@�33@�"�@��@�
=@�@���@��y@�-@��7@��@�x�@�p�@�hs@�p�@�hs@��@��`@��@�A�@���@���@�
=@���@�J@���@��T@���@���@�O�@��@��@�V@���@���@�Ĝ@�j@��m@�K�@��+@��@���@���@���@��h@��h@��h@��7@�X@���@���@��@��/@��9@���@��@�j@�b@�t�@�K�@�+@�
=@���@���@�v�@�-@�{@�@�?}@���@�1'@��@~��@~@|��@|I�@|1@|1@{�F@{33@zJ@x��@w�;@w\)@v�y@v�R@vff@v@u`B@t��@t�j@t�@st�@so@r��@rJ@pĜ@pb@o�w@o|�@oK�@o+@o
=@n�+@n$�@n$�@n@m��@m@m�-@m��@m�h@m/@l�D@lz�@l9X@k��@k��@kt�@kC�@k"�@ko@j�@j��@j=q@iX@hQ�@gl�@f��@f$�@f{@f@f5?@e�@e�h@e�@eO�@d�@d�/@d�/@d�/@d�/@d�j@dZ@c�m@c�
@c��@c�
@c��@ct�@cdZ@cS�@cC�@co@b�@b��@b�!@b^5@b-@b�@a��@aG�@`�`@`�@`1'@_�;@_|�@^��@^V@]��@\�@\��@\I�@\�@[��@[��@[@Z��@Z^5@Z-@Y��@Y��@YX@Y7L@X��@XĜ@XĜ@X��@XbN@XQ�@X1'@Xb@W�@W�w@W|�@W+@V�y@V��@Vv�@Vff@VV@V$�@Up�@T��@T�/@T�j@T��@T�D@TZ@T�@S��@Rn�@Qhs@Q�@P�`@P�u@P1'@Pb@O�@O��@O|�@O;d@O;d@O;d@O;d@O+@O
=@N�y@Nv�@M�@M@Mp�@L�@K@J��@J�H@J��@J��@J��@J��@J��@J��@J��@J�\@J~�@J=q@JJ@I�@I�#@I�^@I��@I7L@I�@H�`@H��@H�@HQ�@H1'@G�@G�w@Gl�@F��@F�y@F�@F�@F�@F�R@F$�@F5?@F$�@E�@E�T@E@E��@E/@D�@D1@C�F@C�@C"�@B��@A��@AX@A7L@A�@@r�@>��@>�+@>v�@>ff@>$�@=�-@=/@=V@<�j@<j@<I�@;�
@;��@;t�@;C�@;o@:��@:n�@:�@97L@8bN@8 �@7�;@7;d@7
=@6�y@6�y@6�@6�@6�R@6��@6��@5�-@4�j@4z�@4Z@49X@4(�@4�@3�m@3�F@3��@3��@3t�@2�H@2J@1��@1�@1�^@1�7@1X@1X@1X@1X@1X@1%@0�`@0��@0�@0�@0Q�@01'@0A�@0A�@01'@0b@/�;@/|�@.��@.ff@.V@.V@.E�@.$�@-@-��@-O�@,��@,�@,z�@,j@,I�@,1@+o@)�^@)&�@(��@(��@(��@(Q�@(b@'��@'|�@'l�@'\)@'K�@';d@'�@&�+@%�@%/@%V@$j@$1@#�F@#t�@#S�@#33@"�@"�\@"J@!X@!&�@ Ĝ@��@\)@;d@+@�@��@ȴ@��@��@v�@5?@{@�-@/@��@�@z�@Z@Z@9X@(�@1@1@1@1@1@��@ƨ@t�@�@n�@^5@-@�@�u@1'@b@  @  @�w@�y@�R@��@�+@�+@�+@�T@p�@/@V@�j@I�@(�@ƨ@t�@C�@~�@=q@�@J@��@�@��@hs@7L@%@�`@��@Ĝ@��@Ĝ@�9@�9@��@��@�9@�@Q�@1'@�@��@\)@
=@ȴ@v�@V@$�@@��@��@p�@O�@?}@/@/@�@�@V@��@�@��@j@��@t�@33@o@o@@@@
�@
�@
�@
�@
�@
�@
�@
�H@
��@
��@
�\@
M�@
-@
-@
J@	�7@	X@	X@	G�@	�@�9@�@bN@Q�@A�@ �@b@  @��G�O�A�hsA�z�A�p�A�t�A�p�A�t�A�n�A�hsA�dZA�r�A�p�A�r�A�p�A�l�A�n�A�n�A�r�A�r�A�v�A�v�A�|�A�r�A�|�A�x�A�z�A�z�A�z�A�|�A�x�A�|�A�z�A�~�A�x�A�z�A�t�A�|�A�v�A�|�A�t�A�x�A�r�A�z�A�v�A�x�A�x�A�z�A�|�A�v�A�x�A�t�A�t�A�n�A�r�A�p�A�XA�S�A�M�A�K�A�9XA�I�A�G�A�K�A�A�A�A�A�?}A�7LA�+A��A�$�A�oA�VA�1A�%A��A��HA��/A���Aʺ^Aʰ!Aʧ�A�n�A�G�A� �A�{A�VA�A��A�ȴAɮAɥ�Aɡ�Aə�Aɝ�AɓuAɓuAɍPAɍPAɇ+A�~�A�|�A�v�A�v�A�l�A�l�A�`BA�K�A�33A�/A�"�A�oA�%A���A���A��#A���Aȡ�Aȝ�Aȏ\Aȇ+A�~�A�n�A�dZA�O�A�I�A�=qA�9XA�1'A�$�A�"�A��A�oA�A���A��A��TA�ȴAǮAǧ�AǙ�AǏ\A�|�A�r�A�XA�ZA�S�A�O�A�?}A�33A�/A�$�A��A��A�{A��A�oA��A�oA��A��A��A��A�1A�
=A�A�%A�A���A���A��A��A��mA��`A��/A���A���A���A���A���A�ȴA���AƬAƩ�AƋDA�hsA�XA�G�A��A��A��HA�ĜA�|�A�+A���A�1'Aé�A�?}A���A��`A�ƨA©�APA�p�A�bNA�I�A���A���A�A�A���A��RA���A���A��DA�t�A�G�A��A�\)A���A�JA�|�A�{A���A�^5A�%A�hsA�A���A���A�z�A�/A��A��HA��-A�ffA�K�A��A��A�1A���A���A��uA��\A�p�A�?}A�5?A�7LA�=qA��A�%A���A���A���A���A��7A�jA�K�A��A�{A�JA��A�ƨA��RA��A��+A�VA�/A�  A��^A��PA�ZA�A�A�A�A�A�A�5?A�5?A�+A� �A�%A��/A���A�I�A���A���A�7LA�%A���A��uA�ffA�+A��wA�~�A�I�A�  A���A�hsA���A��jA�"�A��uA��#A�ƨA���A���A�p�A�VA�&�A��A��FA��uA�dZA�$�A���A��mA��
A���A�A�A�-A���A��A���A���A��A�~�A�p�A�l�A�XA�C�A�9XA�/A�{A�
=A��yA���A���A�?}A�bA�  A��/A���A�x�A�S�A�oA��A�ȴA��PA�?}A�7LA�G�A�;dA�=qA�C�A�A�A�=qA�I�A�C�A�E�A�O�A�O�A�;dA�G�A�=qA�/A�/A�1'A� �A�-A�+A��A�1A�  A��RA�~�A�&�A��A�ZA��yA��PA�?}A�JA��mA��RA��\A�bNA���A��A� �A��
A�x�A�
=A��;A���A�n�A��A��A��FA���A�jA�9XA� �A�
=A��#A�5?A�$�A�oA�VA���A��9A�ffA�Q�A�7LA�ƨA�dZA��;A��A�p�A�"�A���A�`BA�M�A�O�A�K�A�?}A�;dA�7LA�/A�1A���A���A���A�x�A�A��A��wA�x�A�^5A�M�A�?}A���A��yA��^A���A��PA�z�A�hsA�=qA��A��A�ƨA��!A���A���A���A���A��uA��uA���A���A��uA��uA���A�l�A�`BA�ffA�ffA�\)A�^5A�bNA�XA�ZA�\)A�XA�ZA�\)A�\)A�\)A�I�A�G�A�VA�=qA�?}A�+A��A�oA�A��A��`A��HA�ȴA��A���A���A��wA��jA��9A��FA���A��7A�9XA�bA��A��RA��!A��A���A���A��DA�x�A�jA�\)A�C�A�&�A�JA��;A���A�ȴA�A��FA��!A��!A���A���A���A���A���A�|�A�r�A�K�A� �A�%A���A��HA���A���A���A��wA��jA��^A���A�VA��A��mA��A���A��!A�bNA�"�A�JA��/A�XA�1'A� �A��A�1'A��A�A��A��uA�dZA�E�A�-A��A���A���A��A���A� �A���A��yA��/A���A��^A���A��hA�z�A�dZA�`BA�Q�A�$�A��#A��-A�t�A�5?A��A�A���A���A��A�bNA�=qA�A��;A�ƨA���A�t�A�ZA�+A�(�A�+A��A��A���A��\A�t�A�5?A�JA��HA��RA��\A��A�r�A�9XA�
=A��!A��PA�^5A�5?A��`A�x�A��A��A��A�bNA�I�A�;dA�-A��A�A��AƨA��AA�A��A��Ax�A`BAK�A"�A~��A~�DA~r�A~ffA~ZA~M�A~=qA~{A}�TA}��A}p�A}�A|�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                      ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�BںBٴB�QB�B��B�BٴB�B��B�KBٴB�KB��B�mB՛B�[B� B�dB�EB��B��B��B��B��B��B��B�XB�^B��B�gB�KB�#B�6B�yB�,B��B�BuB1B�B�B+B�BBuB�B�B�BoB(BFB\B�B�B��B��B��B�B�+B�B�fB�B� B�&B� B�NB�jB�5B�NB��B��B�B��B�lBq�Bh�BS&BC�BC�B)�BBYB�B�B�B�B~BSB��B�TB��B�BB�UB��B��B.BoiB]�BP�B?�B0�BxB�BhB�B
��B
��B
�8B
�]B
�B
��B
�B
��B
��B
�LB
�9B
��B
�IB
��B
�OB
��B
��B
�(B
x8B
_pB
W�B
*�B
"�B
 �B
�B
1B
B
�B
�B
B	��B	�B	�vB	�/B	�B	��B	�gB	�B	��B	�<B	�0B	�#B	�RB	�zB	�gB	B	��B	��B	�FB	�4B	��B	�CB	��B	��B	�(B	�B	��B	��B	��B	�B	z�B	zB	t�B	u�B	m�B	iDB	g�B	g�B	d�B	c�B	a|B	_�B	^5B	Z�B	[#B	W
B	U2B	R B	RTB	Q�B	H�B	GB	FB	B�B	>wB	=<B	=�B	7�B	9$B	5?B	6�B	4nB	3�B	3hB	33B	2�B	2-B	1�B	1�B	1�B	0�B	0!B	.�B	/B	-wB	-B	*�B	(XB	$�B	+�B	,B	%B	$tB	#�B	$B	#:B	!�B	"hB	$B	 'B	IB	�B	�B	�B	�B	B	 �B	&�B	/OB	5?B	7�B	7�B	8RB	:�B	<�B	=�B	>�B	>�B	@B	@�B	?HB	>�B	@�B	IRB	K�B	D�B	H�B	IRB	H�B	HB	HB	IB	J�B	M�B	OBB	S�B	V9B	W
B	X�B	ZQB	\�B	aB	gmB	h�B	jKB	kQB	kQB	jB	o�B	poB	q�B	t�B	s�B	t�B	tB	{B	}�B	�4B	��B	�AB	�_B	��B	��B	�	B	�B	�.B	��B	��B	�CB	�:B	�B	��B	��B	��B	�kB	�B	��B	��B	�'B	�-B	�B	�RB	��B	�^B	��B	��B	�wB	�OB	�OB	��B	�3B	��B	�B	��B	�EB	��B	��B	ںB	�B	�B	�&B	�B	�sB	�sB	� B	�GB	��B	�8B	�8B	�	B	�DB	�B	��B
 �B
�B
1B
	�B
�B
	�B

rB
B
�B
�B
�B
�B
�B
MB
�B
�B
CB
IB
OB
!B
 \B
 �B
!bB
#�B
'RB
)�B
+6B
,�B
-�B
.�B
0�B
2�B
:�B
=<B
@�B
EmB
M�B
O�B
QB
Q�B
R�B
R�B
S�B
U2B
W
B
\�B
^�B
^�B
^�B
_B
_�B
`�B
b�B
b�B
b�B
c�B
dZB
f2B
j�B
lWB
m�B
m)B
n�B
n�B
ncB
oiB
o B
o B
o�B
p;B
qB
q�B
sMB
u�B
{�B
z�B
|�B
}�B
�B
�{B
��B
��B
��B
��B
��B
�=B
�DB
��B
��B
��B
��B
��B
�SB
��B
�$B
��B
��B
�+B
�+B
�+B
�_B
�_B
��B
��B
�B
�kB
��B
�xB
��B
��B
��B
�FB
�RB
��B
�_B
��B
��B
��B
�-B
��B
��B
�hB
�nB
��B
�LB
��B
��B
��B
��B
�B
�RB
��B
��B
��B
��B
��B
��B
�6B
�qB
�<B
�qB
�qB
�qB
�<B
�B
�BB
�BB
�B
��B
��B
��B
�aB
�B
�tB
ƨB
��B
�B
�EB
ȴB
��B
��B
��B
��B
�B
ȴB
ɺB
ʌB
��B
�<B
�B
ΥB
�pB
ΥB
��B
��B
ΥB
��B
��B
�HB
�B
�B
�B
�B
�}B
�}B
�}B
ѷB
ӏB
ӏB
��B
�,B
�aB
ԕB
ԕB
�gB
��B
�B
֡B
�
B
خB
�B
یB
یB
�dB
�jB
�5B
�B
�5B
ޞB
�B
�HB
�B
�B
�B
�NB
�B
� B
�B
�B
��B
�B
��B
��B
��B
��B
�mB
�B
��B
�B
��B
�
B
��B
��B
�B
�B
�B
�DB
�DB
�DB
�DB
�B
��B
�B
�B
�B
�B
��B
�B
�B
�B
��B
��B
�B
�B
�"B
��B
�cB
�B
��B
�B
�5B
��B
��B
�B
�B
�oB
�B
��B
��B
��B
�B
�B
�B
��B
��B
�vB
�B
�B
�B
�GB
�GB
�GB
�B
�|B
�B
�B
�B
�B
��B
�B
�B
�TB
�B
��B
��B
�ZB
��B
��B
�2B
��B
��B
��B
�B
��B
�lB
��B
�>B
�	B
�>B
�>B
��B
��B
��B
�B
�B
�B
�B
�xB
�DB
�DB
�xB
�DB
��B
�B
�B
�JB
��B
��B
�B
��B
��B
��B
�"B
��B
�"B
��B
�"B
��B
��B
��B
��B
�cB
��B
�cB 4B  B 4B iB iB �B �B �B �B �B �B �B �BoBoB;B�B�BBB�BMBMBBBMBBBMBB�B�B�B�B�BB�BSB�B�B%B%B%B�B�B�B�B_B_B�B_B_B�B�B�B1B�B�B1B�B�B	lB	lB	7B	�B
=BDBxBxBB~B�B�B�B�B�BVB�B�B(B(B�B�B�B�B�B.B.B.B.B�BoBBoB@BBB@BB�BB�B�B�B�B�BBBBMB�B�B�BMB�BB$B�B�B$BYB�B�BYB�B$B�B�B�B�B�B_B_B_B+B_B+B_B�B1B�B�B�B�B�B7B7BkB�B	B	B�B�B�B�BIB�BBBB�B�B!BVBVB�BVBVB!B�B!-B!-B!-B!�B"4B"hB"�B"�B"�B"�B#B#�B$tB$B$tB&B%�B&B%�B%�B&B&LB&LB&LB&�B&�B&�B'B'�B'�B($B(XB(XB(XB(�B(�B(�B(�B(�B(�B(�B(XB(�B(�B)�B*0B)�B)�B+B+kB+�B+�B+�B+�B,B-B-CB-CB-B-B,qB-�B-�B.B.B.}B.�B.�B/OB/OB/�B0�B0�B0�B0�B0�B0�B1[B1�B1�B1�B2-B2-B2-B2-B2aB2aB2aB2aB2aB2aB2�B2�B2�B33B3hB3�B49B4B4�B4�B5B5B5?B5�B5�B5�B6B6B6B6B6B6B6B5�B6B6�B7�B7�B8RB8�B8�B8�B8�B8�B8�B8�B8�B8�B8�B8�B8�B8�B8�B8�B9$B9�B9�B9�B:*B:�B:�B:�B:^B:�B;0B;�B;�B;�B;�B<B<B<6B<�B=BѷB�sBیB�B��B�B�WB�)B��B՛B�QBٴBچBںB��B��B�#BچB�yB�QB��B��BخB�B�KB��BٴB�KB�QBخB�QB�B��B�B�B�yB��B��B�WB�KB�WB�B�B�B��B�B��BںB�
B��B�yB�KB�KBخB�EB�EB�yB�sB�9B��B�mB��BרB�B��B�2B��B��B҉B�aBуB� B��B��BӏB�B��B�B�NBбB�?B�<B�HBɺB�#BʌB�XB�)B�B�?B��BƨBÖB��BÖB�gB�aB�aB��B��BB��B�[B��BBÖBÖB�HB��B�B��B��B��B�[B��B�OB��B�HB�B��B�B��B��B�6B�qB�B�B�<B�^B��B��B��B��B�B��B��B��B��B��B��B��B��B�XB��B�RB�B��B��B�^B��B��B��B�HB��B�B�B��B��B�-B�9B��B��B��B�BǮBȀB��B�KB��BǮB��B�#B�^B̘B�^B͟B˒B��B��B͟B�NB��B��B�BخBخB�5B�BچBޞB�B�B�B1B�DB��B��B��B�B�B��B��B��B�lBBB	7B�BuB��B�"B��B�.BGB	B�B�B�B�BVB�BFB�B)*B$BBeB�B�B�BB�B�B{BIB+BeB�BMBSBB�B�BFBB�BeB{B+B�BB�BYB@B�B	BhBoBuB�B�B�B7B�B�BMBB�BMBB�B�BoBPB(B�B(BhB�B�B�B�B�BB�B"B�B�B�BB�B�B�B�B�B�B�B%B  BB  B��B�B��B�B��BB�B�B{B�B�>B�lBB��B��B�B�JB�%B��B�8B��B��B�B��B�ZB��B�B��B�|B��B�B�lB  B�|B�iB�ZB�B�iB�vB�lB��B�iB�B�QB�2B�B��B�&B�B�B�B� B�TB�B�B�vB�B��B�B��B�BݘB�B��B��B��B�QB��B�cB�aB� B�dB��B�gBɆBуB��B��B�dB��B��B�*B�tB�B�tB�hB�LB��B��B��B�eB��B�B��B�(B��B��B��B�lB��B��BsMBq�Bt�B|�BncBjBg�B{BffBsMBY�B^5BY�Bh�BK^BF�BFtBD�BB[BD�BC�B@�BA�B<jBGzBF�B5B<B%zB)_B%B 'BIB	B#BB%�BB_B_BSB	BMBeBB�B�B"B(BPBhB�B�B�B�B.BDB�B�BVBuB B�B�BBhB�B�B�B�B�B.BBPB�BbBVB.B�B�B�B�B
	B�B�B�B�B"BVB�B	lB�B�BDB$B"B	lBoB�B�.B �B �BAB��B 4B�]B��B �B�]B��B�>B�B��B��B��B�B�%B�B��B�AB�B��B�B��B�B�B�QB�mB�B�B�B�B�|B�|B�TB��B�9B�2B��B�,B�XB�}BŢB�HB�9BΥB�LB��BƨB��B��B��B��B�B��B�VB�CB��B��B��B��B��B�$B�B��B�B�uB��B�MBcB��B~�B{�B{�B��BcBw�By�Bv`Bp�Bm)BjBo�Bn�BhsBe�Bg�Ba�B_;B_B_pBX�BV�BQ�BP�BQ�BP�BP}BXBL0BGEBQ�BGBJ�BA B@OB>�BA B<6BI�B2�B<�B*eB3hBEB?HB'B'RB!�B�B~B�BOB=BqBkB+B+BeB+B�B�BYB�B�B�B�B@B�B�B�B(B�B�B�B	lB�B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                                                      G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                                                      G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202209061825322022090618253220220906182532202209061825322022090618253220220906182532SI  SI  ARFMARFM                                                                                                                                                2022012314192620220123141926IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022020213005220220202130052QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022020213005220220202130052QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2022090607225920220906072259IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2022090618253820220906182538IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOW  OW  V1.0V1.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022090618253820220906182538IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022090618253820220906182538IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                