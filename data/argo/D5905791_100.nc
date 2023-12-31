CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-11-15T04:39:10Z creation; 2022-09-06T18:25:46Z DMQC;      
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
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20211115043910  20220907192126  5905791 5905791 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               d   dAA  AOAO7825_008765_100                 7825_008765_100                 2C  2C  DD  SOLO_II                         SOLO_II                         8765                            8765                            V2.6; SBE602 06Mar19            V2.6; SBE602 06Mar19            853 853 @٢�Y�@٢�Y�11  @٢Ğ쿱@٢Ğ쿱@4��>l@4��>l�e$�ud0�e$�ud011  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?��@�\@E�@�G�@��R@�G�@�  @��RAG�A   A*=qA@  AaG�A�Q�A�  A��A�\)A�  AϮA߮A�  B   B(�B(�B  B   B(  B0(�B8(�B@(�BH  BP  BX(�B_�
Bg�
Bp  Bx  B�B��B��B�  B�(�B�  B��B��B��B��B��B�  B�{B��B��B��B�{B�{B�  B��B�  B�(�B�{B�  B��B�  B�(�B�{B��B��B��B��C   C{C��C�C  C
  C�C��C��C  C
=C�C
=C��C  C  C��C"  C$
=C&  C'��C)��C+�C-�C0
=C2{C4  C5��C8  C9��C<  C>  C@
=CB
=CC��CE�CG��CJ  CK��CN  CO��CQ�CS�CU��CX  CZ  C\  C^
=C_��Cb  Cd
=Cf
=Ch  Cj  Ck��Cn  Co��Cr  Ct  Cu��Cw��Cz
=C|
=C}�C�C�  C�  C�  C�C�
=C�C���C���C���C�C�
=C�  C���C�
=C�C�  C�C�  C�  C�C�C���C���C�  C�  C�  C���C���C�C�
=C���C���C���C�C�C�  C���C���C�  C�  C�C�  C�C�  C���C�  C�C�C�  C�  C���C���C�  C�  C�  C�C���C���C���C���C�  C�  C���C�  C�  C�C�
=C�C�  C�C�C���C���C�  C���C���C���C���C�C�\C�C�  C�  C�C�  C�  C�  C�  C�  C���C�C�C�  C�
=C�C�  C�C�C�  C�C�
=C�  C���C���C�  C�  C�C�  C���C���C�C�C���C�  C�  C�  C�  C�  C�  C���C���C���C�C�
=C�C�
=C�C���D z�D�D��D�D� D�qD}qD�qD� D�qDz�D�qD� D  D��D  D��D	  D	}qD
  D
� D
�qD}qD�qD� D�qD}qD  D��D  D}qD�qD� D  D� D  Dz�D  D�D  D}qD��D� D�D}qD��D� D�D��D  D� D  D}qD�qDz�D��Dz�D�D��D��Dz�D  D� D�RD }qD!  D!��D"  D"� D"�qD#}qD#�qD$� D%�D%� D&  D&}qD&�RD'}qD(  D(��D)D)�D*D*��D+D+� D+�RD,xRD,�RD-xRD-�RD.xRD.�qD/��D0  D0}qD0�qD1}qD2  D2}qD2�qD3}qD3�qD4��D4�qD5z�D5�qD6� D7D7��D8  D8��D9�D9�D:�D:��D:�qD;z�D<  D<� D<�qD=}qD=�qD>��D?�D?��D@D@� D@�qDA}qDA�qDB}qDCDC�DC�qDD}qDD�qDE� DFDF�DF�qDGz�DG��DHz�DH��DI}qDJ  DJ��DKDK� DK�qDL� DM  DM� DN  DN}qDN�qDO��DP  DP}qDQ�DQ� DR  DR� DS  DS��DT�DT� DU  DU}qDU�qDV� DV�qDWz�DX  DX}qDX��DY}qDZ�DZ��D[  D[� D[�qD\��D]  D]� D^�D^��D^�qD_}qD`�D`� D`�qDa� Db  Db� Dc�Dc��Dd�Dd� De  De�Df  Dfz�Df�qDg� Dg�qDh� Di  Diz�Di�qDj� Dk�Dk��Dl  Dl}qDl�qDm� Dn�Dn� Dn�qDo� Dp�Dp� Dq  Dq� Dr  Dr� Ds  Ds}qDt  Dt� Du  Du}qDu��Dvz�Dv��Dwz�Dw�qDx��Dy  Dy}qDz  Dz��D{  D{}qD{�qD|z�D}  D}��D~�D~��D~�qD� D�  D�=qD�~�D���D�  D�B�D��HD��HD�HD�>�D�}qD�� D���D�>�D�~�D��qD�  D�AHD�~�D���D�  D�>�D��HD�D�HD�@ D��HD�D�  D�>�D��HD��HD���D�@ D�� D��HD�HD�>�D�~�D���D�  D�@ D�~�D�� D�HD�@ D�~�D�� D�  D�>�D�}qD���D���D�AHD���D��HD�  D�AHD�� D�� D�HD�@ D�� D�� D�  D�>�D�~�D���D��qD�AHD���D��HD�HD�>�D�~�D��HD�HD�@ D�� D���D���D�@ D���D�D�  D�@ D���D�� D��qD�@ D�~�D���D�  D�@ D�}qD���D���D�=qD�~�D�� D�  D�=qD�� D��HD�HD�@ D��HD�D�  D�AHD��HD��qD�  D�AHD�� D���D�  D�AHD�~�D��qD���D�B�D���D�� D�HD�B�D��HD���D��qD�>�D�� D��HD�  D�=qD��HD��HD���D�@ D��HD�D��D�B�D���D�� D��qD�AHD���D��HD��D�B�D���D�D�  D�>�D�� D��HD�HD�AHD�~�D��qD�  D�>�D�~�D��HD�  D�@ D���D��HD�HD�AHD�� D�D��D�AHD��HD��HD�  D�@ D��HD��HD�HD�AHD��HD��HD�HD�@ D�}qD�� D�HD�@ D�~�D��qD���D�B�D���D�� D�  D�>�D�~�D��HD�  D�@ D��HD�D��D�AHD�� D��HD�  D�>�D�� D��HD�  D�<)D�|)D��qD��qD�<)D�~�D��HD�  D�>�D�� D�� D�  D�@ D�� D�� D�HD�AHD��HD��HD�HD�B�D�� D���D���D�@ D�� D��HD���D�<)D�� D�� D���D�@ D��HD�� D�  D�@ D�� D�� D�  D�AHD��HD��HD�HD�@ D���D�D�  D�@ D�� D��qD���D�@ D D�� D�  D�B�DÂ�Dþ�D���D�AHDĂ�D��HD���D�=qD�}qD�� D��D�B�DƁHDƾ�D���D�>�D�~�DǽqD���D�@ D�~�DȾ�D���D�>�Dɀ D��HD�HD�@ DʁHD�D�HD�@ Dˀ D�� D���D�=qD�~�D�� D�  D�@ D̀ D;�D�  D�@ D΀ D�� D���D�=qD�~�DϾ�D��qD�>�D�~�D�� D�HD�@ D�~�DѾ�D��qD�@ D�~�DҾ�D�  D�>�D�}qDӾ�D���D�@ D�~�DԼ)D���D�AHDՂ�D��HD�  D�@ Dր D�� D�  D�AHD׀ D��HD��D�>�D�~�D�� D�  D�AHDفHD�� D�  D�>�D�}qDڽqD�  D�B�DہHD�D�HD�>�D܀ D��HD�  D�@ D݁HD���D��D�AHDހ D�� D�HD�AHD�}qD߽qD�  D�>�D�}qDྸD�  D�@ D�~�D��HD�HD�AHD�HD��HD�  D�@ DわD��HD�HD�AHD� D侸D��qD�@ D�~�D�qD���D�@ D� D��HD�HD�AHD�HD��HD�  D�@ D�HD�� D�HD�>�D�~�D龸D���D�AHD� D�� D�  D�@ D� D뾸D�HD�C�D� D��HD�HD�>�D�~�D�� D�  D�=qD�|)D�qD���D�>�D� D��HD�  D�>�D�� D��HD���D�>�D�HD�D�HD�AHD�HD�qD���D�B�D�D�D���D�>�D�~�D�� D�  D�@ D���D��HD�  D�>�D��HD��HD�  D�@ D�� D�� D�  D�@ D�� D�� D���D�>�D���D�D�HD�B�D�ff?�?8Q�?k�?��
?�Q�?�ff@�\@�@&ff@8Q�@Tz�@\(�@s33@��
@���@�z�@���@��@��@�Q�@�p�@�=q@�\)@ٙ�@�  @���@��@�p�A33A�A(�A��A�AffA��A\)A$z�A(Q�A,��A1G�A5A:�HA>{ADz�AG
=AL��AO\)AU�AXQ�A^�RAaG�Ag
=Aj=qAp  As�
AxQ�A}p�A�Q�A��A�z�A�  A���A�(�A�A���A��A���A�ffA���A��HA��A�\)A�G�A��
A�p�A�Q�A���A�z�A�{A���A��\A���A��RA���A��HA�{A�\)A\A��
AƸRA�Q�A�33A���AϮA���A��
A�p�A�Q�A��A���A޸RA��A�33A��A�A�G�A�z�A�{A���A�=qA�p�A��RA��A�33A�ffA��BG�B{B\)BQ�B�B�\B\)B��B	��B\)B  B��B�\B(�B��B�\B\)B��B�B�Bz�B=qB
=Bz�Bp�B
=B�
B!p�B"=qB#�
B$��B&{B'
=B(Q�B)p�B*�RB+�
B-G�B.=qB/�B0��B1�B3\)B4Q�B5�B6�HB8Q�B9G�B:�RB;�B=G�B>{B?�
B@z�BB{BB�HBDQ�BEG�BF�\BG�BH��BI�BK33BLQ�BMG�BN�RBO�BQ�BQ�BS�BTQ�BUBV�RBX(�BYG�BZ�RB[�B]G�B^{B_�B`��Bb{Bc33Bdz�BeBf�HBhQ�Bi�Bj�RBk�Bm�Bn{Bo�Bp��Br{Bs
=BtQ�Bu��Bv�RBw�
ByG�BzffB{�B|��B}B\)B�{B���B�G�B�{B�ffB�G�B��B�ffB���B���B�(�B���B�\)B��B�z�B�
=B�B�(�B��HB�\)B�{B�z�B�33B��B�ffB���B��B��B���B���B�B�{B��HB�33B��B�Q�B��B�p�B�{B���B��B���B�=qB��RB�G�B��B�Q�B��RB�p�B�B�z�B���B�\)B��
B�Q�B��RB�\)B��B�Q�B��RB�33B��B�{B��RB���B��B��B��\B��HB�p�B��B�ffB���B�G�B�  B�=qB�
=B�G�B�  B�Q�B�
=B�\)B�  B�z�B�
=B���B�  B��RB�
=B�B�{B��HB�33B��B�Q�B�
=B�p�B�(�B���B�33B��B�ffB��B��B�ffB���B��B�{B���B��B�{B���B�G�B�(�B���B��B��B��RB�p�B��B��HB�G�B�{BĸRB�\)B�(�BƸRBǙ�B�{B���B�p�B�=qB��HB˙�B�Q�B���B��
B�Q�B�33B��
BЏ\B�\)B��
B���B�\)B�(�B��HBՅB�Q�B��HB�B�Q�B�33B�Bڏ\B�\)B��
B���B�G�B�=qB���B߅B�Q�B��HB�B�Q�B�33B��
B�\B�G�B��B���B�G�B�=qB���B�B�(�B��B�B�ffB�33B�B��B�33B�  B�RB�33B�(�B��B�\)B�(�B��RB���B�(�B��HB��B�(�B��B��B��\B��B��
B��RB�33B�(�B���B�p�C (�C ffC �HC�C�\C�HC=qC�C��C\)C�RC  Cz�CC�C�\C��C33C�\C�
CG�C�C�CG�C�\C	  C	G�C	��C
{C
G�C
�RC
=C\)CC
=Cp�C��C�C�C��CG�C�C�CQ�C��C
=C\)C��C{C\)CC{CffC�
C{Cz�C��C{C�C�RC
=CffC�\C�C{CQ�C�\C�C  C{CG�C�C�\C��C�C{CQ�CffC��C��C�HC(�C=qCp�C�C�RC  C�CG�C�C��C�
C{C33CQ�C��C�RC��C�C=qC�\C�C�
C{C33CffC��C�RC  C�C=qC�\C��C�
C
=C�Cp�Cz�C�RC��C 
=C \)C p�C ��C �C!  C!G�C!z�C!��C!�C"  C"Q�C"z�C"��C"��C#{C#G�C#�\C#�C$  C$�C$Q�C$��C$�RC$��C%=qC%\)C%��C%�HC&  C&G�C&z�C&�C&��C'�C'\)C'��C'C({C(=qC(ffC(�RC(�C){C)ffC)�\C)�RC*
=C*=qC*ffC*�RC*�C+
=C+ffC+�\C+�RC,  C,(�C,ffC,�RC,�
C-{C-\)C-�C-�RC.
=C.(�C.p�C.�RC.�HC/(�C/ffC/�\C/�HC0�C0G�C0��C0�
C1  C1Q�C1�\C1�RC2{C2G�C2z�C2��C2�C3G�C3z�C3��C3��C4(�C4\)C4�C4�HC5
=C5\)C5�\C5C6{C6=qC6p�C6C6�C733C7z�C7��C7�C833C8Q�C8��C8�C9{C9\)C9��C9��C:�C:ffC:�C:�HC;{C;G�C;��C;�
C<  C<Q�C<��C<C=
=C=G�C=z�C=��C>
=C>33C>�\C>��C>��C?G�C?�C?�C@
=C@=qC@p�C@CA  CA(�CAz�CA�CA�HCB33CBffCB�\CB�CC{CCG�CC��CC��CC��CDQ�CDz�CD�CE  CE33CE\)CE�CE�CF
=CFffCF�\CF�RCG
=CGG�CGffCG�RCG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                            ?��@�\@E�@�G�@��R@�G�@�  @��RAG�A   A*=qA@  AaG�A�Q�A�  A��A�\)A�  AϮA߮A�  B   B(�B(�B  B   B(  B0(�B8(�B@(�BH  BP  BX(�B_�
Bg�
Bp  Bx  B�B��B��B�  B�(�B�  B��B��B��B��B��B�  B�{B��B��B��B�{B�{B�  B��B�  B�(�B�{B�  B��B�  B�(�B�{B��B��B��B��C   C{C��C�C  C
  C�C��C��C  C
=C�C
=C��C  C  C��C"  C$
=C&  C'��C)��C+�C-�C0
=C2{C4  C5��C8  C9��C<  C>  C@
=CB
=CC��CE�CG��CJ  CK��CN  CO��CQ�CS�CU��CX  CZ  C\  C^
=C_��Cb  Cd
=Cf
=Ch  Cj  Ck��Cn  Co��Cr  Ct  Cu��Cw��Cz
=C|
=C}�C�C�  C�  C�  C�C�
=C�C���C���C���C�C�
=C�  C���C�
=C�C�  C�C�  C�  C�C�C���C���C�  C�  C�  C���C���C�C�
=C���C���C���C�C�C�  C���C���C�  C�  C�C�  C�C�  C���C�  C�C�C�  C�  C���C���C�  C�  C�  C�C���C���C���C���C�  C�  C���C�  C�  C�C�
=C�C�  C�C�C���C���C�  C���C���C���C���C�C�\C�C�  C�  C�C�  C�  C�  C�  C�  C���C�C�C�  C�
=C�C�  C�C�C�  C�C�
=C�  C���C���C�  C�  C�C�  C���C���C�C�C���C�  C�  C�  C�  C�  C�  C���C���C���C�C�
=C�C�
=C�C���D z�D�D��D�D� D�qD}qD�qD� D�qDz�D�qD� D  D��D  D��D	  D	}qD
  D
� D
�qD}qD�qD� D�qD}qD  D��D  D}qD�qD� D  D� D  Dz�D  D�D  D}qD��D� D�D}qD��D� D�D��D  D� D  D}qD�qDz�D��Dz�D�D��D��Dz�D  D� D�RD }qD!  D!��D"  D"� D"�qD#}qD#�qD$� D%�D%� D&  D&}qD&�RD'}qD(  D(��D)D)�D*D*��D+D+� D+�RD,xRD,�RD-xRD-�RD.xRD.�qD/��D0  D0}qD0�qD1}qD2  D2}qD2�qD3}qD3�qD4��D4�qD5z�D5�qD6� D7D7��D8  D8��D9�D9�D:�D:��D:�qD;z�D<  D<� D<�qD=}qD=�qD>��D?�D?��D@D@� D@�qDA}qDA�qDB}qDCDC�DC�qDD}qDD�qDE� DFDF�DF�qDGz�DG��DHz�DH��DI}qDJ  DJ��DKDK� DK�qDL� DM  DM� DN  DN}qDN�qDO��DP  DP}qDQ�DQ� DR  DR� DS  DS��DT�DT� DU  DU}qDU�qDV� DV�qDWz�DX  DX}qDX��DY}qDZ�DZ��D[  D[� D[�qD\��D]  D]� D^�D^��D^�qD_}qD`�D`� D`�qDa� Db  Db� Dc�Dc��Dd�Dd� De  De�Df  Dfz�Df�qDg� Dg�qDh� Di  Diz�Di�qDj� Dk�Dk��Dl  Dl}qDl�qDm� Dn�Dn� Dn�qDo� Dp�Dp� Dq  Dq� Dr  Dr� Ds  Ds}qDt  Dt� Du  Du}qDu��Dvz�Dv��Dwz�Dw�qDx��Dy  Dy}qDz  Dz��D{  D{}qD{�qD|z�D}  D}��D~�D~��D~�qD� D�  D�=qD�~�D���D�  D�B�D��HD��HD�HD�>�D�}qD�� D���D�>�D�~�D��qD�  D�AHD�~�D���D�  D�>�D��HD�D�HD�@ D��HD�D�  D�>�D��HD��HD���D�@ D�� D��HD�HD�>�D�~�D���D�  D�@ D�~�D�� D�HD�@ D�~�D�� D�  D�>�D�}qD���D���D�AHD���D��HD�  D�AHD�� D�� D�HD�@ D�� D�� D�  D�>�D�~�D���D��qD�AHD���D��HD�HD�>�D�~�D��HD�HD�@ D�� D���D���D�@ D���D�D�  D�@ D���D�� D��qD�@ D�~�D���D�  D�@ D�}qD���D���D�=qD�~�D�� D�  D�=qD�� D��HD�HD�@ D��HD�D�  D�AHD��HD��qD�  D�AHD�� D���D�  D�AHD�~�D��qD���D�B�D���D�� D�HD�B�D��HD���D��qD�>�D�� D��HD�  D�=qD��HD��HD���D�@ D��HD�D��D�B�D���D�� D��qD�AHD���D��HD��D�B�D���D�D�  D�>�D�� D��HD�HD�AHD�~�D��qD�  D�>�D�~�D��HD�  D�@ D���D��HD�HD�AHD�� D�D��D�AHD��HD��HD�  D�@ D��HD��HD�HD�AHD��HD��HD�HD�@ D�}qD�� D�HD�@ D�~�D��qD���D�B�D���D�� D�  D�>�D�~�D��HD�  D�@ D��HD�D��D�AHD�� D��HD�  D�>�D�� D��HD�  D�<)D�|)D��qD��qD�<)D�~�D��HD�  D�>�D�� D�� D�  D�@ D�� D�� D�HD�AHD��HD��HD�HD�B�D�� D���D���D�@ D�� D��HD���D�<)D�� D�� D���D�@ D��HD�� D�  D�@ D�� D�� D�  D�AHD��HD��HD�HD�@ D���D�D�  D�@ D�� D��qD���D�@ D D�� D�  D�B�DÂ�Dþ�D���D�AHDĂ�D��HD���D�=qD�}qD�� D��D�B�DƁHDƾ�D���D�>�D�~�DǽqD���D�@ D�~�DȾ�D���D�>�Dɀ D��HD�HD�@ DʁHD�D�HD�@ Dˀ D�� D���D�=qD�~�D�� D�  D�@ D̀ D;�D�  D�@ D΀ D�� D���D�=qD�~�DϾ�D��qD�>�D�~�D�� D�HD�@ D�~�DѾ�D��qD�@ D�~�DҾ�D�  D�>�D�}qDӾ�D���D�@ D�~�DԼ)D���D�AHDՂ�D��HD�  D�@ Dր D�� D�  D�AHD׀ D��HD��D�>�D�~�D�� D�  D�AHDفHD�� D�  D�>�D�}qDڽqD�  D�B�DہHD�D�HD�>�D܀ D��HD�  D�@ D݁HD���D��D�AHDހ D�� D�HD�AHD�}qD߽qD�  D�>�D�}qDྸD�  D�@ D�~�D��HD�HD�AHD�HD��HD�  D�@ DわD��HD�HD�AHD� D侸D��qD�@ D�~�D�qD���D�@ D� D��HD�HD�AHD�HD��HD�  D�@ D�HD�� D�HD�>�D�~�D龸D���D�AHD� D�� D�  D�@ D� D뾸D�HD�C�D� D��HD�HD�>�D�~�D�� D�  D�=qD�|)D�qD���D�>�D� D��HD�  D�>�D�� D��HD���D�>�D�HD�D�HD�AHD�HD�qD���D�B�D�D�D���D�>�D�~�D�� D�  D�@ D���D��HD�  D�>�D��HD��HD�  D�@ D�� D�� D�  D�@ D�� D�� D���D�>�D���D�D�HD�B�G�O�?�?8Q�?k�?��
?�Q�?�ff@�\@�@&ff@8Q�@Tz�@\(�@s33@��
@���@�z�@���@��@��@�Q�@�p�@�=q@�\)@ٙ�@�  @���@��@�p�A33A�A(�A��A�AffA��A\)A$z�A(Q�A,��A1G�A5A:�HA>{ADz�AG
=AL��AO\)AU�AXQ�A^�RAaG�Ag
=Aj=qAp  As�
AxQ�A}p�A�Q�A��A�z�A�  A���A�(�A�A���A��A���A�ffA���A��HA��A�\)A�G�A��
A�p�A�Q�A���A�z�A�{A���A��\A���A��RA���A��HA�{A�\)A\A��
AƸRA�Q�A�33A���AϮA���A��
A�p�A�Q�A��A���A޸RA��A�33A��A�A�G�A�z�A�{A���A�=qA�p�A��RA��A�33A�ffA��BG�B{B\)BQ�B�B�\B\)B��B	��B\)B  B��B�\B(�B��B�\B\)B��B�B�Bz�B=qB
=Bz�Bp�B
=B�
B!p�B"=qB#�
B$��B&{B'
=B(Q�B)p�B*�RB+�
B-G�B.=qB/�B0��B1�B3\)B4Q�B5�B6�HB8Q�B9G�B:�RB;�B=G�B>{B?�
B@z�BB{BB�HBDQ�BEG�BF�\BG�BH��BI�BK33BLQ�BMG�BN�RBO�BQ�BQ�BS�BTQ�BUBV�RBX(�BYG�BZ�RB[�B]G�B^{B_�B`��Bb{Bc33Bdz�BeBf�HBhQ�Bi�Bj�RBk�Bm�Bn{Bo�Bp��Br{Bs
=BtQ�Bu��Bv�RBw�
ByG�BzffB{�B|��B}B\)B�{B���B�G�B�{B�ffB�G�B��B�ffB���B���B�(�B���B�\)B��B�z�B�
=B�B�(�B��HB�\)B�{B�z�B�33B��B�ffB���B��B��B���B���B�B�{B��HB�33B��B�Q�B��B�p�B�{B���B��B���B�=qB��RB�G�B��B�Q�B��RB�p�B�B�z�B���B�\)B��
B�Q�B��RB�\)B��B�Q�B��RB�33B��B�{B��RB���B��B��B��\B��HB�p�B��B�ffB���B�G�B�  B�=qB�
=B�G�B�  B�Q�B�
=B�\)B�  B�z�B�
=B���B�  B��RB�
=B�B�{B��HB�33B��B�Q�B�
=B�p�B�(�B���B�33B��B�ffB��B��B�ffB���B��B�{B���B��B�{B���B�G�B�(�B���B��B��B��RB�p�B��B��HB�G�B�{BĸRB�\)B�(�BƸRBǙ�B�{B���B�p�B�=qB��HB˙�B�Q�B���B��
B�Q�B�33B��
BЏ\B�\)B��
B���B�\)B�(�B��HBՅB�Q�B��HB�B�Q�B�33B�Bڏ\B�\)B��
B���B�G�B�=qB���B߅B�Q�B��HB�B�Q�B�33B��
B�\B�G�B��B���B�G�B�=qB���B�B�(�B��B�B�ffB�33B�B��B�33B�  B�RB�33B�(�B��B�\)B�(�B��RB���B�(�B��HB��B�(�B��B��B��\B��B��
B��RB�33B�(�B���B�p�C (�C ffC �HC�C�\C�HC=qC�C��C\)C�RC  Cz�CC�C�\C��C33C�\C�
CG�C�C�CG�C�\C	  C	G�C	��C
{C
G�C
�RC
=C\)CC
=Cp�C��C�C�C��CG�C�C�CQ�C��C
=C\)C��C{C\)CC{CffC�
C{Cz�C��C{C�C�RC
=CffC�\C�C{CQ�C�\C�C  C{CG�C�C�\C��C�C{CQ�CffC��C��C�HC(�C=qCp�C�C�RC  C�CG�C�C��C�
C{C33CQ�C��C�RC��C�C=qC�\C�C�
C{C33CffC��C�RC  C�C=qC�\C��C�
C
=C�Cp�Cz�C�RC��C 
=C \)C p�C ��C �C!  C!G�C!z�C!��C!�C"  C"Q�C"z�C"��C"��C#{C#G�C#�\C#�C$  C$�C$Q�C$��C$�RC$��C%=qC%\)C%��C%�HC&  C&G�C&z�C&�C&��C'�C'\)C'��C'C({C(=qC(ffC(�RC(�C){C)ffC)�\C)�RC*
=C*=qC*ffC*�RC*�C+
=C+ffC+�\C+�RC,  C,(�C,ffC,�RC,�
C-{C-\)C-�C-�RC.
=C.(�C.p�C.�RC.�HC/(�C/ffC/�\C/�HC0�C0G�C0��C0�
C1  C1Q�C1�\C1�RC2{C2G�C2z�C2��C2�C3G�C3z�C3��C3��C4(�C4\)C4�C4�HC5
=C5\)C5�\C5C6{C6=qC6p�C6C6�C733C7z�C7��C7�C833C8Q�C8��C8�C9{C9\)C9��C9��C:�C:ffC:�C:�HC;{C;G�C;��C;�
C<  C<Q�C<��C<C=
=C=G�C=z�C=��C>
=C>33C>�\C>��C>��C?G�C?�C?�C@
=C@=qC@p�C@CA  CA(�CAz�CA�CA�HCB33CBffCB�\CB�CC{CCG�CC��CC��CC��CDQ�CDz�CD�CE  CE33CE\)CE�CE�CF
=CFffCF�\CF�RCG
=CGG�CGffCG�RCG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                            @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�J@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�E�A�C�A�C�A�A�A�A�A�C�A�C�A�C�A�E�A�E�A�E�A�E�A�E�A�G�A�G�A�G�A�I�A�I�A�K�A�K�A�K�A�K�A�M�A�C�A�9XA�9XA�9XA�;dA�9XA�33A�&�A�Aי�A��AֶFA֬A֣�A֑hA�dZA�\)A�5?A�(�A��A�A՗�A�JA�l�A��A�S�A�ĜA�7LA�bA�dZA��
A�ZA��A��A���A�-A��-A��A�$�A��uA�A��A�~�A��A�A�A�C�A�S�A�"�A���A��A���A���A�ƨA�$�A�n�A���A���A���A�?}A��TA�|�A���A�E�A�M�A�oA���A�^5A��mA���A�M�A���A��7A�oA��uA��A��\A�O�A��wA�~�A�bNA���A���A�&�A�^5A�bNA��A���A�E�A��TA��RA�r�A���A��9A�\)A���A�(�A��-A�-A�ƨA�Q�A�
=AA}�hA{�A{O�Az�Az��Az�AyhsAv5?As��Aq�^Ap�HAm��AkVAj�jAjA�AiXAg�
AfAd�Ac�^Ac�7Ac�Aa�mA`ZA_G�A\�`AY�AW"�AU��AT�RAR^5AQ�AM��AL1AK\)AJ��AJ1AF�9AD~�AB��AA��A@�9A?O�A;t�A6�yA4��A4Q�A3l�A333A2ĜA2M�A1��A0�yA.�DA+��A*VA(��A'7LA$9XA#/A!��A Q�AO�A��AM�A5?A1A�#A�A`BAjAZA$�A�A��AffA&�AE�AG�A�Av�A9XA9XAC�A�A�/A��Az�A�^AM�A-AA�PA�A
M�A	�#A	�-A	p�Av�AK�A^5Al�A%A��A��A��A1A �@���@���@��@�Ĝ@�(�@�C�@��7@���@�@�v�@�|�@�^5@��@�@�K�@��y@�\@�V@���@�hs@��@��@�x�@�ff@�O�@�j@�S�@ץ�@��/@�j@�(�@��;@�"�@�@�l�@�@�v�@�5?@ѡ�@�G�@υ@�v�@�hs@�(�@� �@��
@��@�+@��@�  @�
=@�=q@�`B@���@���@�ƨ@�$�@��-@�r�@�ƨ@��@��F@��m@��@��;@���@���@�hs@�Ĝ@�|�@��@���@���@���@�v�@��@���@�hs@�X@�O�@�hs@�O�@��9@��F@��@�=q@�v�@�v�@��@��@��@��F@��@�l�@�l�@�dZ@�dZ@�l�@�t�@���@��@��;@��F@�E�@��@��u@��@�ƨ@��@���@��@�K�@�@���@���@���@��R@�n�@�E�@�@��-@���@��7@��@�?}@��@���@�j@��;@��@���@��@��H@�-@���@�&�@��/@��/@���@���@��@�Ĝ@��@��u@�9X@�  @�  @�1@� �@� �@���@��P@�33@�@��y@��@��\@�n�@�M�@�=q@�-@�{@�@���@��@��T@���@�hs@�7L@�%@��`@��j@���@�z�@�(�@�ƨ@�t�@�;d@���@�v�@�M�@�J@��@���@���@�X@�V@���@�1'@��;@��@�t�@�;d@��@���@��!@���@�~�@�n�@�E�@�{@���@��@���@��^@�hs@��@��j@��@���@��
@��
@��;@��m@���@���@�\)@�33@�o@�@��\@�E�@�{@��@���@�@�p�@��`@���@�A�@�(�@�  @��m@���@��@��@�dZ@�C�@��@���@���@���@�n�@�$�@���@��#@���@�@��@�hs@�&�@��@��@�Q�@�9X@�1@��@��
@��F@���@�t�@�K�@�@��@�ȴ@�ff@�$�@���@�p�@��@�bN@�I�@�b@��@���@��@��@�|�@�\)@�C�@�33@���@�ȴ@�v�@�$�@��@���@�G�@��j@���@��u@��@�j@�bN@�Q�@�I�@�(�@���@�C�@��@�@���@�^5@�E�@��#@��-@�hs@�7L@�%@��`@���@�z�@�I�@�A�@�9X@�1@��
@��w@��@��@�l�@�S�@�C�@�C�@�;d@�"�@�o@��y@��@��!@�=q@��T@��@�p�@�%@�Ĝ@��9@���@��@�I�@�  @~ff@}?}@{�m@{�@{C�@{@z�H@z��@y�#@x�9@w�@w�@w�;@w��@wK�@w+@v��@v�+@v$�@v{@u�@v@u�@u@u`B@uO�@u?}@t�@t�@t�D@tj@t1@s��@sC�@s33@r��@r�@q%@o�@o
=@n��@n$�@m��@m�h@mO�@mV@l�/@lz�@l(�@k�m@k��@kt�@k@j�!@jn�@j�@i��@i��@iX@i7L@i&�@i&�@h��@g�;@fE�@e�@c��@c33@c@b��@b�@a�@`��@`��@`Q�@`1'@` �@`b@`  @_�;@_��@_l�@_+@^v�@]�@]@]p�@\��@\1@[�F@[dZ@Z=q@Y�#@Y�7@XĜ@X �@W\)@V��@V�R@Vff@U�@U�h@UO�@U/@T�@T�/@T�@T��@T��@T��@T��@T��@T�D@Tz�@Tz�@TZ@TI�@TI�@T(�@T1@S��@S�m@S�
@S�F@S��@S��@S�@SS�@S33@S@R�H@R��@R^5@Q�^@P�`@O�w@Nȴ@NV@NE�@N@M�-@MV@L�j@KdZ@J��@J=q@I�@I�^@I�^@I�7@IX@H��@H1'@G+@Fȴ@Fv�@F5?@F$�@Ep�@DI�@C��@C@B~�@B^5@B^5@BM�@B=q@B-@A��@@Ĝ@?\)@>�R@>5?@=��@=�@<�/@<��@<z�@<z�@<z�@<j@<Z@<I�@<(�@;�F@:�@:J@9��@8�u@8bN@8b@7+@6ff@65?@6$�@5�-@4�j@4(�@3�@2�\@1�^@1��@1��@1��@1�7@1x�@1hs@1G�@1&�@0�`@0�u@0bN@0Q�@/�w@/\)@/+@/�@/�@/
=@.�@.�+@-�-@-`B@-/@,��@,�/@,��@,�j@,�j@,�D@,j@+�
@*�H@*n�@)��@)x�@)�@(��@(�`@(��@(bN@(1'@(1'@( �@(b@(  @(  @'�@'��@&�y@&�+@&V@&{@%��@%�-@%p�@$�j@$9X@#�
@#��@#o@#@#@"�@"��@"-@!�^@!�7@!hs@!7L@!�@ ��@ ��@ �@ bN@ r�@ bN@ bN@ bN@ bN@ Q�@ Q�@ Q�@ A�@ 1'@�@�y@�y@�@��@E�@{@@�@?}@��@��@�@��@�D@z�@j@I�@I�@9X@9X@�@��@�
@�F@�@33@�@��@�!@��@n�@n�@^5@M�@��@�^@��@hs@&�@%@�`@Ĝ@��@�u@�@r�@bN@Q�@A�@b@�P@�@��@V@V@V@E�@E�@5?@{@�@��@p�@`B@`B@O�@?}@/@�@��@(�@ƨ@dZ@^5@J@�#@��@��@��@��@�7@x�@%@��@r�@bN@Q�@�;@��@��@��@�P@|�@+@�@�y@�@�@�y@�R@��@v�@V@5?@$�@{@��@��@�@`B@`B@?}@��@�@9X@��@ƨ@ƨ@�F@�F@��@�@S�@"�@@@@@@
�@
�H@
�H@
�H@
�H@
��@
J@	��@	��@	�@	G�@	G�@	G�@	G�@	G�@	7L@	7L@	7LA�A�A�E�A�E�A�E�A�E�A�C�A�E�A�C�A�C�A�A�A�?}A�G�A�?}A�E�A�=qA�A�A�?}A�?}A�C�A�?}A�G�A�A�A�E�A�C�A�E�A�A�A�E�A�?}A�G�A�A�A�E�A�C�A�C�A�G�A�C�A�I�A�A�A�G�A�A�A�G�A�C�A�G�A�C�A�E�A�E�A�E�A�E�A�E�A�E�A�C�A�G�A�C�A�G�A�C�A�I�A�C�A�I�A�G�A�G�A�G�A�G�A�I�A�E�A�K�A�C�A�I�A�C�A�I�A�C�A�K�A�E�A�K�A�G�A�I�A�I�A�K�A�G�A�K�A�G�A�M�A�E�A�K�A�I�A�K�A�G�A�K�A�I�A�I�A�K�A�I�A�K�A�G�A�M�A�I�A�O�A�I�A�O�A�I�A�O�A�I�A�O�A�K�A�M�A�I�A�M�A�I�A�I�A�I�A�K�A�K�A�G�A�M�A�I�A�M�A�G�A�M�A�I�A�M�A�G�A�M�A�K�A�M�A�K�A�M�A�K�A�K�A�M�A�K�A�M�A�G�A�?}A�9XA�;dA�7LA�;dA�7LA�=qA�7LA�=qA�5?A�;dA�5?A�;dA�7LA�;dA�7LA�;dA�7LA�=qA�7LA�=qA�7LA�=qA�7LA�?}A�7LA�=qA�;dA�=qA�;dA�=qA�;dA�=qA�5?A�7LA�1'A�9XA�33A�-A�1'A�/A�5?A�-A�1'A�+A�1'A� �A��A��A��A��A��A�%A���A��A��yA��#A��
A�ƨA׮A׋DA�S�A�"�A��A�JA��A��A���A�ȴA���Aֲ-AָRAֲ-AֶFA֮Aִ9A֬Aְ!A֥�A֮A֥�A֬A֣�A֩�A֣�A֥�A֟�A֣�A֝�A֝�A֙�A֙�A֓uA֋DAցA�n�A�hsA�dZA�bNA�dZA�^5A�dZA�\)A�`BA�\)A�`BA�XA�\)A�Q�A�A�A�1'A�7LA�/A�5?A�/A�5?A�-A�/A�&�A�-A�$�A�&�A�$�A�$�A��A� �A��A��A��A�{A�{A�JA�JA�A�A���A���A��A��yA��A�ȴA՛�A�n�A�bNA�VA�A�A�1'A� �A��A�
=A�VA�1A��A���A�ĜAԑhA���A�\)A�{A��mAҺ^AґhA�A�Aџ�A�
=AБhA���A��A���A·+A�;dA�1A���AͅA�?}A�&�A�%A̬A�/A˼jA�A�A��Aʲ-A�Q�A��AɬA�
=A�(�A�z�A�JAƍPA���A�bNA�hsA�1'A�1A���AhA��A��A���A�G�A��A�JA�A��A��TA��HA��;A��#A���A��hA�v�A�^5A�\)A�O�A�E�A�VA�t�A�bA��A�ȴA��FA��\A�Q�A�$�A�ƨA�~�A�O�A��A���A��A���A�~�A�bNA�S�A�9XA�9XA�{A�A���A�%A�A���A�XA�oA��A���A���A��+A��A�l�A�ZA�O�A�A�A���A�\)A��HA���A��uA���A�`BA�$�A�{A��A�bA���A��yA���A��wA��FA��A���A��\A��+A��7A��DA�r�A�l�A�p�A�bNA�M�A� �A��;A���A��7A�t�A�  A���A�-A���A��A���A��7A�XA��A���A���A�v�A�^5A���A��!A��A�I�A���A��#A�I�A�?}A�A�A�33A�/A�oA��HA���A��FA���A���A��DA�z�A�XA�A�A�/A�%A��;A���A� �A�A���A���A��A�Q�A�"�A���A���A��A�C�A�{A��#A�ĜA���A���A��DA�p�A�dZA�M�A�{A��RA��/A���A��uA�z�A�/A�
=A���A��mA��^A��hA�v�A�-A�bA���A��^A���A��7A�~�A�n�A�l�A�VA�9XA��A��A�A���A���A��\A��A�n�A�hsA�I�A�(�A�VA���A��TA���A�v�A�`BA�A�A��A��A�;dA�(�A�+A�A�VA��mA��mA��`A��/A��#A��A��^A��FA��A���A���A��+A��7A��7A��+A�x�A�l�A�M�A�C�A�&�A�-A�-A�+A��A���A��#A���A���A���A�ƨA��^A��RA�x�A��+A�z�A��A�  A���A���A��yA��yA�ƨA�x�A�7LA�+A�$�A�(�A��A�{A�JA�  A��mA��wA�l�A���A�jA�K�A�A�A��A�oA��A��HA���A��wA��9A��-A��A���A��PA��+A�v�A�n�A�^5A�VA�C�A�+A��A�oA�A���A���A��A��mA�ƨA��A���A���A��A�n�A�O�A���A��jA��+A�ZA���A��A��A��A�O�A�oA��A���A��+A�I�A�1'A�oA�A��A��TA��;A��
A���A�A��-A���A��\A�~�A�dZA�Q�A�A�A�9XA�1'A�&�A��A�VA�A�  A��A��#A��A��^A��A���A��7A�p�A�O�A�?}A�;dA�oA�  A��A��TA���A���A��FA��!A��A���A��\A��7A��+A�|�A�x�A�x�A�p�A�hsA�bNA�VA�?}A�9XA�(�A��A�  A��`A���A��FA���A���A���A��hA��hA��PA��A��A�v�A�p�A�t�A�p�A�jA�n�A�jA�hsA�ffA�bNA�\)A�M�A��A�z�A��HA���A���A�ĜA���A��wA��wA��9A��A���A���A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                            A�E�A�C�A�C�A�A�A�A�A�C�A�C�A�C�A�E�A�E�A�E�A�E�A�E�A�G�A�G�A�G�A�I�A�I�A�K�A�K�A�K�A�K�A�M�A�C�A�9XA�9XA�9XA�;dA�9XA�33A�&�A�Aי�A��AֶFA֬A֣�A֑hA�dZA�\)A�5?A�(�A��A�A՗�A�JA�l�A��A�S�A�ĜA�7LA�bA�dZA��
A�ZA��A��A���A�-A��-A��A�$�A��uA�A��A�~�A��A�A�A�C�A�S�A�"�A���A��A���A���A�ƨA�$�A�n�A���A���A���A�?}A��TA�|�A���A�E�A�M�A�oA���A�^5A��mA���A�M�A���A��7A�oA��uA��A��\A�O�A��wA�~�A�bNA���A���A�&�A�^5A�bNA��A���A�E�A��TA��RA�r�A���A��9A�\)A���A�(�A��-A�-A�ƨA�Q�A�
=AA}�hA{�A{O�Az�Az��Az�AyhsAv5?As��Aq�^Ap�HAm��AkVAj�jAjA�AiXAg�
AfAd�Ac�^Ac�7Ac�Aa�mA`ZA_G�A\�`AY�AW"�AU��AT�RAR^5AQ�AM��AL1AK\)AJ��AJ1AF�9AD~�AB��AA��A@�9A?O�A;t�A6�yA4��A4Q�A3l�A333A2ĜA2M�A1��A0�yA.�DA+��A*VA(��A'7LA$9XA#/A!��A Q�AO�A��AM�A5?A1A�#A�A`BAjAZA$�A�A��AffA&�AE�AG�A�Av�A9XA9XAC�A�A�/A��Az�A�^AM�A-AA�PA�A
M�A	�#A	�-A	p�Av�AK�A^5Al�A%A��A��A��A1A �@���@���@��@�Ĝ@�(�@�C�@��7@���@�@�v�@�|�@�^5@��@�@�K�@��y@�\@�V@���@�hs@��@��@�x�@�ff@�O�@�j@�S�@ץ�@��/@�j@�(�@��;@�"�@�@�l�@�@�v�@�5?@ѡ�@�G�@υ@�v�@�hs@�(�@� �@��
@��@�+@��@�  @�
=@�=q@�`B@���@���@�ƨ@�$�@��-@�r�@�ƨ@��@��F@��m@��@��;@���@���@�hs@�Ĝ@�|�@��@���@���@���@�v�@��@���@�hs@�X@�O�@�hs@�O�@��9@��F@��@�=q@�v�@�v�@��@��@��@��F@��@�l�@�l�@�dZ@�dZ@�l�@�t�@���@��@��;@��F@�E�@��@��u@��@�ƨ@��@���@��@�K�@�@���@���@���@��R@�n�@�E�@�@��-@���@��7@��@�?}@��@���@�j@��;@��@���@��@��H@�-@���@�&�@��/@��/@���@���@��@�Ĝ@��@��u@�9X@�  @�  @�1@� �@� �@���@��P@�33@�@��y@��@��\@�n�@�M�@�=q@�-@�{@�@���@��@��T@���@�hs@�7L@�%@��`@��j@���@�z�@�(�@�ƨ@�t�@�;d@���@�v�@�M�@�J@��@���@���@�X@�V@���@�1'@��;@��@�t�@�;d@��@���@��!@���@�~�@�n�@�E�@�{@���@��@���@��^@�hs@��@��j@��@���@��
@��
@��;@��m@���@���@�\)@�33@�o@�@��\@�E�@�{@��@���@�@�p�@��`@���@�A�@�(�@�  @��m@���@��@��@�dZ@�C�@��@���@���@���@�n�@�$�@���@��#@���@�@��@�hs@�&�@��@��@�Q�@�9X@�1@��@��
@��F@���@�t�@�K�@�@��@�ȴ@�ff@�$�@���@�p�@��@�bN@�I�@�b@��@���@��@��@�|�@�\)@�C�@�33@���@�ȴ@�v�@�$�@��@���@�G�@��j@���@��u@��@�j@�bN@�Q�@�I�@�(�@���@�C�@��@�@���@�^5@�E�@��#@��-@�hs@�7L@�%@��`@���@�z�@�I�@�A�@�9X@�1@��
@��w@��@��@�l�@�S�@�C�@�C�@�;d@�"�@�o@��y@��@��!@�=q@��T@��@�p�@�%@�Ĝ@��9@���@��@�I�@�  @~ff@}?}@{�m@{�@{C�@{@z�H@z��@y�#@x�9@w�@w�@w�;@w��@wK�@w+@v��@v�+@v$�@v{@u�@v@u�@u@u`B@uO�@u?}@t�@t�@t�D@tj@t1@s��@sC�@s33@r��@r�@q%@o�@o
=@n��@n$�@m��@m�h@mO�@mV@l�/@lz�@l(�@k�m@k��@kt�@k@j�!@jn�@j�@i��@i��@iX@i7L@i&�@i&�@h��@g�;@fE�@e�@c��@c33@c@b��@b�@a�@`��@`��@`Q�@`1'@` �@`b@`  @_�;@_��@_l�@_+@^v�@]�@]@]p�@\��@\1@[�F@[dZ@Z=q@Y�#@Y�7@XĜ@X �@W\)@V��@V�R@Vff@U�@U�h@UO�@U/@T�@T�/@T�@T��@T��@T��@T��@T��@T�D@Tz�@Tz�@TZ@TI�@TI�@T(�@T1@S��@S�m@S�
@S�F@S��@S��@S�@SS�@S33@S@R�H@R��@R^5@Q�^@P�`@O�w@Nȴ@NV@NE�@N@M�-@MV@L�j@KdZ@J��@J=q@I�@I�^@I�^@I�7@IX@H��@H1'@G+@Fȴ@Fv�@F5?@F$�@Ep�@DI�@C��@C@B~�@B^5@B^5@BM�@B=q@B-@A��@@Ĝ@?\)@>�R@>5?@=��@=�@<�/@<��@<z�@<z�@<z�@<j@<Z@<I�@<(�@;�F@:�@:J@9��@8�u@8bN@8b@7+@6ff@65?@6$�@5�-@4�j@4(�@3�@2�\@1�^@1��@1��@1��@1�7@1x�@1hs@1G�@1&�@0�`@0�u@0bN@0Q�@/�w@/\)@/+@/�@/�@/
=@.�@.�+@-�-@-`B@-/@,��@,�/@,��@,�j@,�j@,�D@,j@+�
@*�H@*n�@)��@)x�@)�@(��@(�`@(��@(bN@(1'@(1'@( �@(b@(  @(  @'�@'��@&�y@&�+@&V@&{@%��@%�-@%p�@$�j@$9X@#�
@#��@#o@#@#@"�@"��@"-@!�^@!�7@!hs@!7L@!�@ ��@ ��@ �@ bN@ r�@ bN@ bN@ bN@ bN@ Q�@ Q�@ Q�@ A�@ 1'@�@�y@�y@�@��@E�@{@@�@?}@��@��@�@��@�D@z�@j@I�@I�@9X@9X@�@��@�
@�F@�@33@�@��@�!@��@n�@n�@^5@M�@��@�^@��@hs@&�@%@�`@Ĝ@��@�u@�@r�@bN@Q�@A�@b@�P@�@��@V@V@V@E�@E�@5?@{@�@��@p�@`B@`B@O�@?}@/@�@��@(�@ƨ@dZ@^5@J@�#@��@��@��@��@�7@x�@%@��@r�@bN@Q�@�;@��@��@��@�P@|�@+@�@�y@�@�@�y@�R@��@v�@V@5?@$�@{@��@��@�@`B@`B@?}@��@�@9X@��@ƨ@ƨ@�F@�F@��@�@S�@"�@@@@@@
�@
�H@
�H@
�H@
�H@
��@
J@	��@	��@	�@	G�@	G�@	G�@	G�@	G�@	7L@	7LG�O�A�A�A�E�A�E�A�E�A�E�A�C�A�E�A�C�A�C�A�A�A�?}A�G�A�?}A�E�A�=qA�A�A�?}A�?}A�C�A�?}A�G�A�A�A�E�A�C�A�E�A�A�A�E�A�?}A�G�A�A�A�E�A�C�A�C�A�G�A�C�A�I�A�A�A�G�A�A�A�G�A�C�A�G�A�C�A�E�A�E�A�E�A�E�A�E�A�E�A�C�A�G�A�C�A�G�A�C�A�I�A�C�A�I�A�G�A�G�A�G�A�G�A�I�A�E�A�K�A�C�A�I�A�C�A�I�A�C�A�K�A�E�A�K�A�G�A�I�A�I�A�K�A�G�A�K�A�G�A�M�A�E�A�K�A�I�A�K�A�G�A�K�A�I�A�I�A�K�A�I�A�K�A�G�A�M�A�I�A�O�A�I�A�O�A�I�A�O�A�I�A�O�A�K�A�M�A�I�A�M�A�I�A�I�A�I�A�K�A�K�A�G�A�M�A�I�A�M�A�G�A�M�A�I�A�M�A�G�A�M�A�K�A�M�A�K�A�M�A�K�A�K�A�M�A�K�A�M�A�G�A�?}A�9XA�;dA�7LA�;dA�7LA�=qA�7LA�=qA�5?A�;dA�5?A�;dA�7LA�;dA�7LA�;dA�7LA�=qA�7LA�=qA�7LA�=qA�7LA�?}A�7LA�=qA�;dA�=qA�;dA�=qA�;dA�=qA�5?A�7LA�1'A�9XA�33A�-A�1'A�/A�5?A�-A�1'A�+A�1'A� �A��A��A��A��A��A�%A���A��A��yA��#A��
A�ƨA׮A׋DA�S�A�"�A��A�JA��A��A���A�ȴA���Aֲ-AָRAֲ-AֶFA֮Aִ9A֬Aְ!A֥�A֮A֥�A֬A֣�A֩�A֣�A֥�A֟�A֣�A֝�A֝�A֙�A֙�A֓uA֋DAցA�n�A�hsA�dZA�bNA�dZA�^5A�dZA�\)A�`BA�\)A�`BA�XA�\)A�Q�A�A�A�1'A�7LA�/A�5?A�/A�5?A�-A�/A�&�A�-A�$�A�&�A�$�A�$�A��A� �A��A��A��A�{A�{A�JA�JA�A�A���A���A��A��yA��A�ȴA՛�A�n�A�bNA�VA�A�A�1'A� �A��A�
=A�VA�1A��A���A�ĜAԑhA���A�\)A�{A��mAҺ^AґhA�A�Aџ�A�
=AБhA���A��A���A·+A�;dA�1A���AͅA�?}A�&�A�%A̬A�/A˼jA�A�A��Aʲ-A�Q�A��AɬA�
=A�(�A�z�A�JAƍPA���A�bNA�hsA�1'A�1A���AhA��A��A���A�G�A��A�JA�A��A��TA��HA��;A��#A���A��hA�v�A�^5A�\)A�O�A�E�A�VA�t�A�bA��A�ȴA��FA��\A�Q�A�$�A�ƨA�~�A�O�A��A���A��A���A�~�A�bNA�S�A�9XA�9XA�{A�A���A�%A�A���A�XA�oA��A���A���A��+A��A�l�A�ZA�O�A�A�A���A�\)A��HA���A��uA���A�`BA�$�A�{A��A�bA���A��yA���A��wA��FA��A���A��\A��+A��7A��DA�r�A�l�A�p�A�bNA�M�A� �A��;A���A��7A�t�A�  A���A�-A���A��A���A��7A�XA��A���A���A�v�A�^5A���A��!A��A�I�A���A��#A�I�A�?}A�A�A�33A�/A�oA��HA���A��FA���A���A��DA�z�A�XA�A�A�/A�%A��;A���A� �A�A���A���A��A�Q�A�"�A���A���A��A�C�A�{A��#A�ĜA���A���A��DA�p�A�dZA�M�A�{A��RA��/A���A��uA�z�A�/A�
=A���A��mA��^A��hA�v�A�-A�bA���A��^A���A��7A�~�A�n�A�l�A�VA�9XA��A��A�A���A���A��\A��A�n�A�hsA�I�A�(�A�VA���A��TA���A�v�A�`BA�A�A��A��A�;dA�(�A�+A�A�VA��mA��mA��`A��/A��#A��A��^A��FA��A���A���A��+A��7A��7A��+A�x�A�l�A�M�A�C�A�&�A�-A�-A�+A��A���A��#A���A���A���A�ƨA��^A��RA�x�A��+A�z�A��A�  A���A���A��yA��yA�ƨA�x�A�7LA�+A�$�A�(�A��A�{A�JA�  A��mA��wA�l�A���A�jA�K�A�A�A��A�oA��A��HA���A��wA��9A��-A��A���A��PA��+A�v�A�n�A�^5A�VA�C�A�+A��A�oA�A���A���A��A��mA�ƨA��A���A���A��A�n�A�O�A���A��jA��+A�ZA���A��A��A��A�O�A�oA��A���A��+A�I�A�1'A�oA�A��A��TA��;A��
A���A�A��-A���A��\A�~�A�dZA�Q�A�A�A�9XA�1'A�&�A��A�VA�A�  A��A��#A��A��^A��A���A��7A�p�A�O�A�?}A�;dA�oA�  A��A��TA���A���A��FA��!A��A���A��\A��7A��+A�|�A�x�A�x�A�p�A�hsA�bNA�VA�?}A�9XA�(�A��A�  A��`A���A��FA���A���A���A��hA��hA��PA��A��A�v�A�p�A�t�A�p�A�jA�n�A�jA�hsA�ffA�bNA�\)A�M�A��A�z�A��HA���A���A�ĜA���A��wA��wA��9A��A���A���A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                            ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B{B{�B{B|B{�B{�B{�B{�B{�B|B{�B{�B|PB|PB|�B|�B|�B|�B|�B|�B}VB}�B}VB.B�B�oB�;B�;B��B�B�SB�B�VB��B��B��B�^B�B�B�}BB��B�-B��B�KB�XB�B	lB1B(XB9�B/OB*0B$@B#�B+�B-�B>�BDgBK�BI�BF?BH�BK)B@OB:�B8B6zB5�B(�B+kB7B�B@B�B iB�lB��B�8B�vB��B�WBҽB�B��BB��B�aB�0B�eB��B�~B�MBm)BV�BQ�BK�BF?B>wB8�B5tB0!B*�B'BIB�B�B�B�BیB��B��B��B��B��B��B�B��B�iBv+BpBlWB[#BS&BN<BJXBAUB9�B3hB/�B,�B)�B#:B�B�B
��B
�oB
�B
�B
��B
ȀB
�9B
�B
��B
�*B
��B
�IB
��B
��B
�B
{�B
s�B
_�B
L�B
C�B
<6B
2aB
(XB
"�B
.B
�B
�B
oB	�JB	�QB	� B	چB	՛B	��B	�tB	��B	�_B	��B	�B	�4B	��B	�VB	�B	��B	��B	�rB	��B	��B	}�B	v`B	q�B	q�B	j�B	j�B	gmB	f�B	gB	g8B	g�B	gmB	h�B	p;B	rGB	qB	qAB	g8B	bB	YB	U�B	OB	L�B	>�B	<�B	=B	J#B	OBB	P}B	PHB	O�B	O�B	g8B	zB	}�B	��B	��B	�B	��B	�YB	�B	�1B	�B	.B	�AB	��B	.B	|�B	��B	�4B	��B	}VB	{JB	zxB	wfB	s�B	q�B	jB	c B	]�B	V�B	`B	a�B	c B	ffB	e�B	g8B	g�B	h>B	h>B	m�B	x�B	�B	��B	~]B	~(B	�;B	��B	�_B	{B	z�B	z�B	|PB	�MB	�B	� B	��B	��B	�VB	��B	��B	��B	��B	��B	�B	��B	��B	�B	�1B	�7B	�7B	�PB	�bB	��B	�B	��B	��B	�	B	�CB	�B	�B	��B	�B	��B	��B	�B	��B	��B	�)B	�MB	�B	�5B	�/B	�]B	��B	�B	�B	��B	�EB	ޞB	��B	�8B	�sB	��B	�B	�B	�B	�%B	�+B	��B	�VB
�B
B
{B
B
GB
{B
�B
�B
SB
�B
B
(B
�B
�B
�B
qB
OB
!bB
&�B
+�B
,=B
,�B
-�B
.B
-�B
/�B
1�B
3�B
2�B
4�B
5�B
5�B
6FB
6zB
8�B
=�B
DgB
FB
HKB
HKB
J�B
J�B
QB
RTB
T,B
TaB
T�B
W�B
Z�B
]dB
^jB
^�B
^5B
^�B
`B
aHB
b�B
c�B
d&B
e`B
g�B
i�B
k�B
l�B
l�B
lWB
n�B
oiB
p�B
rB
sMB
s�B
uZB
u�B
v+B
v`B
wfB
y>B
z�B
{�B
|�B
}�B
~�B
� B
��B
��B
��B
�B
��B
��B
�VB
��B
�bB
��B
�B
�B
�FB
��B
�kB
�xB
�IB
��B
��B
��B
�bB
�4B
�:B
�FB
�LB
��B
�XB
��B
��B
��B
�*B
�eB
��B
�B
��B
��B
�'B
�[B
�'B
��B
��B
��B
�3B
��B
�B
�B
��B
��B
�LB
��B
��B
��B
�XB
�0B
�jB
�qB
��B
�BB
��B
�B
�}B
�OB
��B
�UB
��B
��B
��B
��B
�-B
�gB
�gB
ĜB
ĜB
ĜB
�B
��B
��B
�zB
�RB
ɆB
ɺB
�XB
ʌB
ʌB
��B
�)B
��B
��B
�dB
�0B
�B
��B
�B
��B
�B
�&B
ԕB
�aB
�2B
�gB
�9B
֡B
�
B
�B
�EB
�yB
�EB
�B
خB
�B
�B
�B
�B
یB
ܒB
ܒB
��B
��B
�/B
�/B
�dB
�/B
�dB
��B
�B
�B
�BB
�HB
��B
�B
� B
� B
��B
��B
��B
��B
�&B
��B
�B
��B
�B
�B
�mB
�mB
�mB
��B
��B
�>B
�
B
�
B
�>B
�sB
�B
��B
��B
��B
�B
�B
�B
�B
�WB
�"B
�"B
�"B
��B
�"B
�"B
��B
�5B
�vB
�B
�AB
�vB
�AB
�B
��B
�TB
�ZB
��B
��B
��B
��B
��B
��B
��B
�fB
�2B
�2B
�2B
�2B
�fB
��B
��B
��B
�8B
�8B
�lB
��B
�	B
�rB
��B
�>B
��B
��B
�B
�VB
��B
��B
��B
�(B
�(B
��B
��B
��B
��B  B 4B �B iBoBB�BBuBuB�B�BuBB�B{BYB�B�BfBfBfB	lBB
�B
�BDBDBDBDBDBxB�BxB�B�BB�BJB~BBPB�BVB"BVB�B\B�B�B�B�B.BbB.B.BbB.BbB.B.B.B.B.B.B.BbBbBbBbBbB�BbB�BbB�B�B�B�B�B�B�B�B�B�B�B4B�B�B�B�B�BB�B{B�B�B�B�B�B�B�B�B�B�B+B�B�B_B�B�B7BkB�B�B�B�BqBqB	B=B�B�B�BVB�B \B \B �B �B �B �B �B �B \B 'B �B!�B"4B"hB#B#:B#B$�B$�B$�B$tB%FB&�B&LB'�B(�B)�B)�B)�B)_B)�B)�B)�B)�B)�B*�B+B*�B*eB+�B+�B,B,B,B+�B,B,qB-wB-�B-�B.B.IB.IB.IB.B-�B.B.}B/�B/�B0!B0�B0�B1'B1'B1'B1�B1�B1�B1�B1�B1�B1�B1�B2aB2�B33B3hB3�B3�B3�B3�B4�B5B5�B5�B6�B6FB6FB6FB6�B7LB7�B7�B8B8RB8RB8�B8�B8�B8�B8�B9$B9$B8�B9$B8�B9$B8�B8�B9$B:�B:^B:*B:^B:�B:�B:�B;0B;dB;�B<6B<6B<jB<6B<�B<jB<�B<�B=B=B<�B=B=B=<B=qB=�B>B>BB>wB>wB>�B>�B>�B>�B>�B?HB?}B?�B?�B@OB@OB@OB@�B@�B@�B@�B@�B@�B@�B@�BA BA�BB'BB�BC-BC-BC-BC-BC-BCaBCaBC�BDgBDgBD�BD�BD�BD�BD�BDgBE9BFBFBF�BH�BH�BH�BH�BH�BH�BH�BH�BH�BI�BI�BI�BI�BI�BJ�BJ�BJ�BJ�BJ�BK^BK�BL0BLdBLdBL0BL0BL�BL�BMBM6BMjBMjBMjBNBN<BNpBN�BN�BN�BOBBOvBP}BP}BP�BP�BP�BQBQBQBQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BR BQ�BQ�BQ�BQ�BR�BR�BR�BR�BS�BS[BS�BS�BS�BS�BS�BS�B{�B|PBz�B{�Bz�B|PBy�B|Bz�B{�B|�Bz�B}"BzxB}�B{B|PB|�B{JB|�Bz�B|�B{JB|Bz�B|�Bz�B|�Bz�B|�B{B|�B|B{JB|�BzxB}"Bz�B}VBz�B|�B{B|�B{B|PB{�B|B|�B{�B|�B{JB}VB{B}VB{B}VB{B|�B|PB|B|�B|PB}�B{B}�B{B}�B{�B}�B{JB}�B{JB}VB|PB|�B{�B}VB|B}VB{JB}�B{�B}"B{�B}"B|PB|�B|�B|PB}VB|�B}�B{�B}�B{B}�B{B~(B{B}�B{JB}VB|�B}�B|�B}�B}"B}�B}"B}"B~]B|PB~]B|�B~�B|�B~]B|�B~�B|�B}�B|�B}�B|�B}�B}VB}"B}�B|�B.B�B�;B�4B��B�iB�B� B�AB� B�uB�iB��B�4B�uB�iB�uB�iB�uB� B�uB� B��B�iB�uB�B�AB�iB��B��B�oB��B��B��B�GB�B�B�;B�{B��B��B�MB��B�SB�B��B��B��B�%B�1B��B��B�YB�B��B�bB��B�B�oB�B��B�B�B��B�?B��B��B��B�B��B��B�dB�RB��B��B��B�B��B��B�0B��B�0B�RB��B��B�0B�$B��B��B�dB��B�dB�0B�dB��B�<B�OB��B�HB�}B��B�B��B��B�wB�OB��B��B�BB��B��B�3B�UB��B� B�aB�UBÖB��B��B��BÖB�[B�aB��B��B��B��BÖB�'B�gB��B�BB��BB��B�-B�gB�-B�9B�3B�6B��BȴB�B�^BɺB˒B�XB��B�zB��B��B��B��B҉B�NB�B�B��B��B�2B�]B�BGB�(B�BIB�B�BB�B�B�BB�B�B�B �B,=B0�B*�B!bB.}B&�B/�BCaBI�B6�B,�B=B@�B)*BjB0�B!�B"hB 'B+B*�BD3B)_B �B#:B#nB%FB#nB!�B"�B!�B#B2aB$�B'�B!�B#nB!B*eBD�B,�B%�B'RB&�B+kB+�B)�B5B+�B1�B5B9�B<jBE9BN�BC-BD�BCaBA�BI�BB�BB�BB�BEBcTBO�BQ�BI�BNpBJ�BF�BEmBF�BEB>wB@OBF�BR�BJXBH�BA�B?}BK)BG�BD3B@�BA�B\)Bi�B@�B?�BA BA�B@�B<�B;�B;�B9$B<�B6�B5tB7�B4�B;dB9XB=B0�B0UBAUB2aB9�B?�BB'B<6B1[B($B+�B,�B%zB(�B%FB,�B+�B!�B'�B:*B<jB�B=B+B=B�BBxB�B�B�BFB�B�B�B�B�BBoB�BB
rB�B%B�BBYB�VB��B�B �B�8B��B��B�2B��B��B��B��B��B�BB�B�"B�MB�B��B�AB�|B�B��B��B�/B�`B�B��B�&B�sB�&B�B�TB�B��B��B�B�B�vB�#B�#B�
B�yBخB�2B��B�B��B�2B��B�BӏB˒B�NB�gBуB�2B�pBŢB��BÖB�B�EB�zB�BȴB�?B�B�KB�EB��BĜB�KB�mBĜB�tB��B˒B��BȀB��B�OB��B�-B�9B��B��B��B�[B�0B�OB�-B�B��B�?B��B�B��B��B�RB�?B�hB��B�OB��B��B��B��B�OB��B��B�=B��B�6B�tB�9B��B�VB�xB��B��B��B�	B�1B�kB�+B�MB��B�+B��B�B�MB�uB��B�B�hB�@B��B��B�PB��B��B��B�PB��B�rB��B�+B�+B��B��B�B�1Bx�B~�B�oB|�BcBo5Bo�Bh>Bb�B`�Bh�BZ�BZ�BY�BW?BVmBVmBS&BTaBS�BS[BT,BS&BR BR BQBN�BN�BLdBK�BMBK�BL0BI�BJ�BJ�BK�BF�BI�BGBE�BF?BF?BF�BFB@�B=qB>�B>wB=�B>wB;�B:�B:�B:�B:�B:*B8RB7LB8�B7B5�B8B7B5B7B7LB2aB6B3�B2�B33B2�B.�B0!B-B,qB-B*�B+kB-wB*0B+B*�B($B)*B(�B'B(XB(XB&�B%FB&�B#:B&�B8RB%�B�B�B�B�B�BB�B�BB�B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                            G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                            G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202209061825322022090618253220220906182532202209061825322022090618253220220906182532SI  SI  ARFMARFM                                                                                                                                                2021111504391020211115043910IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021112503011220211125030112QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021112503011220211125030112QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2022090607225720220906072257IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2022090618253620220906182536IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOW  OW  V1.0V1.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022090618253620220906182536IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022090618253620220906182536IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                