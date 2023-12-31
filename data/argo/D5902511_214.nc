CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  8   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2022-05-27T11:17:27Z creation; 2023-02-10T23:09:44Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.2   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
_FillValue        G�O�     �  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 p  V�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  ]8   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 p  v�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  }h   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �(   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 p  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �X   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 p  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ׈   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     �  �H   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 p    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � x   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 p +8   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � 1�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` Kh   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   K�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   Q�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   W�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ]�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ^   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ^$   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ^,   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ^4   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ^<   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ^�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ^�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ^�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        _    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        _   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       _   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    _Argo profile    3.1 1.2 19500101000000  20220527111727  20230210230944  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_214                 6810_008521_214                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @��D�"@��D�"11  @��J#9�@��J#9�@0�~���$@0�~���$�dͪ�9m�dͪ�9m11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  @�\@G�@��
@�G�@�p�@�p�A ��AG�A   A+�A?\)A^{A�  A���A�  A�  A�Q�AУ�A�  A�  B   B(�BQ�B(�B   B(  B0  B8Q�B@Q�BH(�BP  BX  B`  BhQ�BpQ�Bx(�B�(�B�{B��
B��
B��B�  B��B�  B�  B�  B�{B��B��B��B�{B�{B��B��B�  B��B�(�B�  B��
B��B��
B��
B��B�{B�  B�  B�{B�  B��C  C  C  C  C	��C  C
=C  C  C��C�C  C  C��C  C��C!��C#�C%�HC'��C*  C+��C-��C0
=C2
=C4  C6
=C8  C:
=C;��C=�C@  CB  CC�CE��CG��CJ
=CL
=CM�CP  CR
=CT{CV
=CW��CY��C\  C]��C`  Cb  Cc��Ce��Ch
=Ci��Cl  Cn  Co��Cr  Cs��Cv  Cx  Cz  C{��C}�C��C�C�  C���C�  C�  C�  C�C�C�C�  C�C�  C�  C���C�  C�C�C���C�  C�
=C�
=C�  C���C�  C�C�  C�  C�C�C�C�  C�C�C�  C�  C�  C�  C���C���C���C���C�  C�  C�  C���C�  C�C�
=C�C�C�  C�  C�  C�  C�  C���C���C���C���C���C�  C���C�  C�C�  C���C���C���C�C�  C���C�C�
=C���C���C���C���C�C�  C���C���C�  C�C�  C�  C�  C���C�  C�  C�  C�C�  C�  C�  C���C���C�  C�  C�
=C�C���C�  C�C���C�C�  C���C�  C�  C�C�
=C�
=C�
=C�C�  C�C�  C���C�  C�
=C�C�  C�C�  C���C�  C�D �D �D�D� D�qD� D�qD}qD�D��D�D��D  D� D�qDz�D  D�D	D	��D
  D
� D
�qDz�D��D��D  D� D  D� D  D}qD��D� D�qDz�D  D�D�D}qD�qD��D  D� D  D}qD�D��D  D}qD  D� D�qDz�D�qD}qD�qD� D  D��D�D�D�D� D�qD }qD!  D!��D"D"�D#  D#}qD#��D$}qD%  D%� D&  D&� D'  D'��D(  D(��D)D)��D*D*� D+  D+� D+��D,� D-D-��D.�D.��D.�qD/z�D/�qD0� D1  D1}qD2  D2� D3�D3��D3�qD4}qD4�qD5}qD6  D6� D6�qD7}qD8  D8��D9  D9��D:D:��D;�D;��D<  D<� D=�D=��D>�D>� D>��D?z�D?��D@}qDA  DA}qDA�qDB}qDC  DC� DD  DD}qDD�qDE� DF  DF��DGDG��DH  DH� DI�DI� DJ  DJ� DK�DK��DL  DL��DM�DM}qDN  DN� DO  DO}qDO�qDP� DQ  DQ� DQ��DR}qDS�DS�DT  DT}qDU  DU� DV  DV� DV�qDW� DX�DX}qDX�qDY��DZ�DZ� D[  D[��D\�D\}qD\�qD]� D^�D^��D_�D_��D`D`��Da  Da� Db  Db� Dc  Dc��Dd�Dd��DeDe��DfDf��Dg  Dg��DhDh��Di  Di� Dj  Dj� Dj�qDk� Dl  Dl}qDl�qDm}qDn  Dn� Do  Do��Dp  Dp}qDq  Dq� Dq�qDr}qDr��Ds� Dt�Dt��Du  Du��Dv�Dv��Dw�Dw� Dx�Dx��Dy  Dy� Dz  Dz� D{  D{� D|  D|z�D|�qD}��D~�D~� D~�qD� D�  D�@ D�� D���D�  D�@ D�� D���D���D�>�D�~�D��HD�HD�@ D�~�D��HD���D�>�D��HD��HD�  D�>�D�� D���D���D�@ D�� D�� D�HD�AHD��HD��HD�  D�@ D��HD��HD�HD�@ D�� D��HD�  D�@ D��HD���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�Q�?W
=?��?�33?�ff@�@�R@8Q�@O\)@^�R@z�H@�=q@�33@�G�@���@�
=@\@У�@�(�@��@�33A   Az�A
�HA�A
=A(�A#�
A)��A.{A4z�A;�A@  AG
=AN{AR�\AZ=qA`  Ae�Al��As33Aw
=A~{A��\A��A�  A��A�ffA���A�z�A�\)A��A���A�Q�A��\A�A���A��
A�{A���A�z�A��RA���A���A�\)A�G�A�z�A�ffAȣ�A��
A�A�\)A��A�(�A�p�A�  A�=qA��
A޸RA�G�A�33A�ffA�G�A�A�{A��A���A��RA��A��A�\)B ��B�RB�B��B=qB33Bz�B	�B
�RB�B�B�B�\B  B��Bp�BffB�B�
B��BB{B�HB�
Bz�B��B{B�HB\)BQ�B�B��BffB�B   B ��B!B"ffB"�HB#�
B$��B%G�B&{B&�HB'\)B(Q�B)G�B)B*ffB+�B,(�B,��B-B.{B/33B0(�B0z�B1�B2=qB3
=B3�B4(�B5G�B5�B6=qB733B8(�B8��B9�B9�B:�HB;\)B<  B<��B=��B>{B>�HB?�
B@(�B@��BA��BB�\BC33BC�BDz�BEp�BEBFffBG\)BH(�BHz�BIG�BJ=qBJ�HBK\)BL  BM�BMp�BN{BO33BO�BP(�BQ�BR=qBR�\BS\)BT(�BUG�BU�BV=qBW
=BX(�BX��BYp�BZ=qB[\)B\  B\Q�B]G�B^=qB^�RB_\)B`z�B`��Bap�Bb�\Bc\)Bc�
Bd��Be��BfffBf�RBg�Bh��Bi�BiBj�HBk�Bk�
Bl��Bm�Bn=qBn�HBp  Bp��Bp��BqBr�RBs33Bs�Bt��Bu��Bu�Bv�HBw�
BxQ�Bx��By�Bz�HB{\)B{�
B|��B}B~{B~�RB�B�(�B�ffB���B�G�B��B��B�=qB��RB��B�G�B�B�=qB��\B��HB�33B�B�(�B�ffB��RB�33B�B�(�B�z�B���B�33B�B�(�B�z�B���B�\)B��
B�(�B��\B��HB�\)B��
B�=qB�z�B���B��B��B�(�B��\B��B���B��B�=qB��RB�G�B�B�(�B�ffB��HB�p�B��B�(�B�z�B���B��B�  B�Q�B��\B�
=B��B�  B�=qB���B�33B���B��B�Q�B���B�G�B�B�{B�ffB���B�p�B�B�{B��\B�
=B���B��B�Q�B��RB�G�B��B��B�ffB���B�p�B�B�(�B��\B�
=B���B��B�Q�B��HB�\)B��
B�(�B�z�B��HB�p�B��B�Q�B���B��B���B�{B�z�B���B�33B��B�=qB��\B��HB�\)B��
B�ffB��RB��B���B�(�B��\B���B�\)B�B�Q�B���B�G�B��B�{B��RB�33B��B��B�ffB���B�p�B�B�(�B���B�G�B��B�  B�ffB���B��B��B�Q�B���B�
=B��B�(�B£�B���B�G�B�B�=qB���B�G�BŮB�  B�z�B��BǙ�B�  B�Q�B��HB�p�B��B�=qBʏ\B��BˮB�=qB̏\B���B�\)B�  B�z�B���B�G�B��
B�ffB�
=B�p�B�B�=qB���B�\)B��B�=qBԣ�B�33B��
B�(�B�z�B���B�p�B�  B؏\B��HB�G�B�B�ffB��HB�G�BۮB�{B܏\B��BݮB�{B�z�B���Bߙ�B�{B�ffB���B�p�B�  B�ffB���B�33B�B�=qB���B�G�B��
B�=qB��B��B癚B�(�B��B�33B�B�(�B�z�B���B�B�(�B��B�
=B�p�B��
B�Q�B��HB�p�B�  B�z�B��HB�G�B�B�Q�B��HB�p�B��B�=qB��RB�G�B��B�ffB��HB�G�B��B�=qB��HB�\)B�B�(�B��RB�\)B�B�(�B��\B�33B�B�=qB��\B���B��C 
=C =qC ffC �RC ��C33CffC��C��C{C\)C��C�
C  C=qC�\C�
C
=C=qCp�C�C  CG�Cp�C��C�C=qCp�C��C�C33Cp�C��C�HC�Cp�C�RC�C	�C	ffC	�RC	��C
(�C
\)C
��C
�C(�CQ�C�\C�
C�C\)C�\CC  CQ�C��C��C  CG�C�\C��C�CQ�Cz�CC
=C\)C��C��C  CG�C�\C�
C�C\)C�CC  C=qC�C�
C�CQ�C�CC
=CQ�C��CC  CG�C��C�HC�CQ�C�\C�
C(�Cz�C�C�C(�Cz�C�RC��C(�Cp�C�RC
=CQ�C�\C��C
=C=qCz�C��C{C\)C�\C��C  C=qC�C��C�CQ�C�C�RC��C =qC �C �
C!
=C!=qC!z�C!C"
=C"\)C"��C"�
C#
=C#G�C#�C#C$
=C$G�C$�\C$��C%�C%Q�C%�\C%C&
=C&G�C&��C&�HC'(�C'p�C'�RC'�HC((�C(\)C(��C(�HC)(�C)p�C)�RC*  C*G�C*�\C*��C+
=C+=qC+p�C+�RC,
=C,Q�C,��C,�
C-{C-G�C-�\C-��C.
=C.Q�C.��C.�HC/33C/z�C/C0  C0=qC0z�C0�RC1  C1=qC1�\C1�
C2(�C2ffC2�RC2�C3(�C3\)C3��C3�HC4(�C4p�C4C5  C5G�C5�\C5C6  C6=qC6�C6��C7{C7ffC7��C7�
C8{C8Q�C8��C8��C9=qC9�\C9��C:{C:Q�C:�\C:�HC;�C;Q�C;��C;�
C<�C<p�C<�RC=  C==qC=�C=�RC=��C>=qC>�C>C?{C?\)C?�C?�C@(�C@ffC@��C@�CA33CA�CA��CB�CB\)CB��CB�HCC�CC\)CC��CC�HCD(�CDp�CDCE
=CE\)CE��CE�HCF(�CFp�CF�RCG  CGG�CG�\CG��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                                       11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?�  @�\@G�@��
@�G�@�p�@�p�A ��AG�A   A+�A?\)A^{A�  A���A�  A�  A�Q�AУ�A�  A�  B   B(�BQ�B(�B   B(  B0  B8Q�B@Q�BH(�BP  BX  B`  BhQ�BpQ�Bx(�B�(�B�{B��
B��
B��B�  B��B�  B�  B�  B�{B��B��B��B�{B�{B��B��B�  B��B�(�B�  B��
B��B��
B��
B��B�{B�  B�  B�{B�  B��C  C  C  C  C	��C  C
=C  C  C��C�C  C  C��C  C��C!��C#�C%�HC'��C*  C+��C-��C0
=C2
=C4  C6
=C8  C:
=C;��C=�C@  CB  CC�CE��CG��CJ
=CL
=CM�CP  CR
=CT{CV
=CW��CY��C\  C]��C`  Cb  Cc��Ce��Ch
=Ci��Cl  Cn  Co��Cr  Cs��Cv  Cx  Cz  C{��C}�C��C�C�  C���C�  C�  C�  C�C�C�C�  C�C�  C�  C���C�  C�C�C���C�  C�
=C�
=C�  C���C�  C�C�  C�  C�C�C�C�  C�C�C�  C�  C�  C�  C���C���C���C���C�  C�  C�  C���C�  C�C�
=C�C�C�  C�  C�  C�  C�  C���C���C���C���C���C�  C���C�  C�C�  C���C���C���C�C�  C���C�C�
=C���C���C���C���C�C�  C���C���C�  C�C�  C�  C�  C���C�  C�  C�  C�C�  C�  C�  C���C���C�  C�  C�
=C�C���C�  C�C���C�C�  C���C�  C�  C�C�
=C�
=C�
=C�C�  C�C�  C���C�  C�
=C�C�  C�C�  C���C�  C�D �D �D�D� D�qD� D�qD}qD�D��D�D��D  D� D�qDz�D  D�D	D	��D
  D
� D
�qDz�D��D��D  D� D  D� D  D}qD��D� D�qDz�D  D�D�D}qD�qD��D  D� D  D}qD�D��D  D}qD  D� D�qDz�D�qD}qD�qD� D  D��D�D�D�D� D�qD }qD!  D!��D"D"�D#  D#}qD#��D$}qD%  D%� D&  D&� D'  D'��D(  D(��D)D)��D*D*� D+  D+� D+��D,� D-D-��D.�D.��D.�qD/z�D/�qD0� D1  D1}qD2  D2� D3�D3��D3�qD4}qD4�qD5}qD6  D6� D6�qD7}qD8  D8��D9  D9��D:D:��D;�D;��D<  D<� D=�D=��D>�D>� D>��D?z�D?��D@}qDA  DA}qDA�qDB}qDC  DC� DD  DD}qDD�qDE� DF  DF��DGDG��DH  DH� DI�DI� DJ  DJ� DK�DK��DL  DL��DM�DM}qDN  DN� DO  DO}qDO�qDP� DQ  DQ� DQ��DR}qDS�DS�DT  DT}qDU  DU� DV  DV� DV�qDW� DX�DX}qDX�qDY��DZ�DZ� D[  D[��D\�D\}qD\�qD]� D^�D^��D_�D_��D`D`��Da  Da� Db  Db� Dc  Dc��Dd�Dd��DeDe��DfDf��Dg  Dg��DhDh��Di  Di� Dj  Dj� Dj�qDk� Dl  Dl}qDl�qDm}qDn  Dn� Do  Do��Dp  Dp}qDq  Dq� Dq�qDr}qDr��Ds� Dt�Dt��Du  Du��Dv�Dv��Dw�Dw� Dx�Dx��Dy  Dy� Dz  Dz� D{  D{� D|  D|z�D|�qD}��D~�D~� D~�qD� D�  D�@ D�� D���D�  D�@ D�� D���D���D�>�D�~�D��HD�HD�@ D�~�D��HD���D�>�D��HD��HD�  D�>�D�� D���D���D�@ D�� D�� D�HD�AHD��HD��HD�  D�@ D��HD��HD�HD�@ D�� D��HD�  D�@ D��HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�Q�?W
=?��?�33?�ff@�@�R@8Q�@O\)@^�R@z�H@�=q@�33@�G�@���@�
=@\@У�@�(�@��@�33A   Az�A
�HA�A
=A(�A#�
A)��A.{A4z�A;�A@  AG
=AN{AR�\AZ=qA`  Ae�Al��As33Aw
=A~{A��\A��A�  A��A�ffA���A�z�A�\)A��A���A�Q�A��\A�A���A��
A�{A���A�z�A��RA���A���A�\)A�G�A�z�A�ffAȣ�A��
A�A�\)A��A�(�A�p�A�  A�=qA��
A޸RA�G�A�33A�ffA�G�A�A�{A��A���A��RA��A��A�\)B ��B�RB�B��B=qB33Bz�B	�B
�RB�B�B�B�\B  B��Bp�BffB�B�
B��BB{B�HB�
Bz�B��B{B�HB\)BQ�B�B��BffB�B   B ��B!B"ffB"�HB#�
B$��B%G�B&{B&�HB'\)B(Q�B)G�B)B*ffB+�B,(�B,��B-B.{B/33B0(�B0z�B1�B2=qB3
=B3�B4(�B5G�B5�B6=qB733B8(�B8��B9�B9�B:�HB;\)B<  B<��B=��B>{B>�HB?�
B@(�B@��BA��BB�\BC33BC�BDz�BEp�BEBFffBG\)BH(�BHz�BIG�BJ=qBJ�HBK\)BL  BM�BMp�BN{BO33BO�BP(�BQ�BR=qBR�\BS\)BT(�BUG�BU�BV=qBW
=BX(�BX��BYp�BZ=qB[\)B\  B\Q�B]G�B^=qB^�RB_\)B`z�B`��Bap�Bb�\Bc\)Bc�
Bd��Be��BfffBf�RBg�Bh��Bi�BiBj�HBk�Bk�
Bl��Bm�Bn=qBn�HBp  Bp��Bp��BqBr�RBs33Bs�Bt��Bu��Bu�Bv�HBw�
BxQ�Bx��By�Bz�HB{\)B{�
B|��B}B~{B~�RB�B�(�B�ffB���B�G�B��B��B�=qB��RB��B�G�B�B�=qB��\B��HB�33B�B�(�B�ffB��RB�33B�B�(�B�z�B���B�33B�B�(�B�z�B���B�\)B��
B�(�B��\B��HB�\)B��
B�=qB�z�B���B��B��B�(�B��\B��B���B��B�=qB��RB�G�B�B�(�B�ffB��HB�p�B��B�(�B�z�B���B��B�  B�Q�B��\B�
=B��B�  B�=qB���B�33B���B��B�Q�B���B�G�B�B�{B�ffB���B�p�B�B�{B��\B�
=B���B��B�Q�B��RB�G�B��B��B�ffB���B�p�B�B�(�B��\B�
=B���B��B�Q�B��HB�\)B��
B�(�B�z�B��HB�p�B��B�Q�B���B��B���B�{B�z�B���B�33B��B�=qB��\B��HB�\)B��
B�ffB��RB��B���B�(�B��\B���B�\)B�B�Q�B���B�G�B��B�{B��RB�33B��B��B�ffB���B�p�B�B�(�B���B�G�B��B�  B�ffB���B��B��B�Q�B���B�
=B��B�(�B£�B���B�G�B�B�=qB���B�G�BŮB�  B�z�B��BǙ�B�  B�Q�B��HB�p�B��B�=qBʏ\B��BˮB�=qB̏\B���B�\)B�  B�z�B���B�G�B��
B�ffB�
=B�p�B�B�=qB���B�\)B��B�=qBԣ�B�33B��
B�(�B�z�B���B�p�B�  B؏\B��HB�G�B�B�ffB��HB�G�BۮB�{B܏\B��BݮB�{B�z�B���Bߙ�B�{B�ffB���B�p�B�  B�ffB���B�33B�B�=qB���B�G�B��
B�=qB��B��B癚B�(�B��B�33B�B�(�B�z�B���B�B�(�B��B�
=B�p�B��
B�Q�B��HB�p�B�  B�z�B��HB�G�B�B�Q�B��HB�p�B��B�=qB��RB�G�B��B�ffB��HB�G�B��B�=qB��HB�\)B�B�(�B��RB�\)B�B�(�B��\B�33B�B�=qB��\B���B��C 
=C =qC ffC �RC ��C33CffC��C��C{C\)C��C�
C  C=qC�\C�
C
=C=qCp�C�C  CG�Cp�C��C�C=qCp�C��C�C33Cp�C��C�HC�Cp�C�RC�C	�C	ffC	�RC	��C
(�C
\)C
��C
�C(�CQ�C�\C�
C�C\)C�\CC  CQ�C��C��C  CG�C�\C��C�CQ�Cz�CC
=C\)C��C��C  CG�C�\C�
C�C\)C�CC  C=qC�C�
C�CQ�C�CC
=CQ�C��CC  CG�C��C�HC�CQ�C�\C�
C(�Cz�C�C�C(�Cz�C�RC��C(�Cp�C�RC
=CQ�C�\C��C
=C=qCz�C��C{C\)C�\C��C  C=qC�C��C�CQ�C�C�RC��C =qC �C �
C!
=C!=qC!z�C!C"
=C"\)C"��C"�
C#
=C#G�C#�C#C$
=C$G�C$�\C$��C%�C%Q�C%�\C%C&
=C&G�C&��C&�HC'(�C'p�C'�RC'�HC((�C(\)C(��C(�HC)(�C)p�C)�RC*  C*G�C*�\C*��C+
=C+=qC+p�C+�RC,
=C,Q�C,��C,�
C-{C-G�C-�\C-��C.
=C.Q�C.��C.�HC/33C/z�C/C0  C0=qC0z�C0�RC1  C1=qC1�\C1�
C2(�C2ffC2�RC2�C3(�C3\)C3��C3�HC4(�C4p�C4C5  C5G�C5�\C5C6  C6=qC6�C6��C7{C7ffC7��C7�
C8{C8Q�C8��C8��C9=qC9�\C9��C:{C:Q�C:�\C:�HC;�C;Q�C;��C;�
C<�C<p�C<�RC=  C==qC=�C=�RC=��C>=qC>�C>C?{C?\)C?�C?�C@(�C@ffC@��C@�CA33CA�CA��CB�CB\)CB��CB�HCC�CC\)CC��CC�HCD(�CDp�CDCE
=CE\)CE��CE�HCF(�CFp�CF�RCG  CGG�CG�\CG��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                                       11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�A���A�%A�1A�1A�1A�%A�1A�1A�1A�1A�1A�
=A�
=A�
=A�JA�
=A�
=A�1A�  A��A��;Aպ^Aգ�A�~�A�ffA�VA�Q�A�M�A�A�A�7LA���A�l�A��A�E�A���A�v�A��AЅAϸRA���Aͩ�A�x�A�ZA��ȂhA���A��A�n�A�A�ȴA�JAȇ+A��Aǝ�A�1A�x�Aģ�A�AËDA�-A��yA���A���A��`A�%A��HA��yA�7LA��\A�$�A��TA�l�A�ĜA�"�A�|�A��A�5?A���A��A���A��A�`BA��A���A��A��7A�A���A�=qA���A�G�A��A���A�+A���A�v�A�~�A�ffA��FA�ȴA��A�M�A�A��yA�A�A�1A��\A�JA�l�A�M�A���A�|�A�5?A|�uAx�`Ar�\Apz�AnAiC�AcA_��A^1'A]7LA\1AZ�jAY"�AQ�;AMp�AJ��AIAGXAB�HAA��A<5?A9��A8ZA6bNA4��A2ȴA0~�A.�A-7LA,ffA+�^A*�\A)t�A(�A(�+A(JA'XA%t�A#�A"^5A ZA�A=qA��A��AhsA��AƨA%A=qA��A/A�HA�wA��A�\A(�A�mA�PA�yAM�A�AA�A�mA�;AA\)A��A�\A�A�\A��AQ�A�TA�FA"�A�`A�HA��A-Ap�AȴA^5A9XA
=A�wA33A
�yA
��A
~�A
=qA
1A	�7A	�A	x�A	G�A�A�#A+A��A�AXA�uA��A�A�uA~�A�!A��At�A��A��A�7A1A�\A�wA ��A ZA Q�A  �@�l�@��R@��@��@�\)@�Ĝ@�A�@��@��T@�&�@���@�Ĝ@��@���@�z�@���@���@��#@�/@�|�@�S�@��@�ȴ@@�n�@�V@�v�@�V@�$�@홚@�X@��@�b@���@�V@�-@���@��@�@���@�;d@���@�R@��@�?}@䛦@�@��@�5?@��#@���@� �@ߥ�@�;d@���@���@���@۾w@��H@���@�G�@�X@��@�S�@֧�@�p�@��
@�@�hs@��@�j@�1'@� �@�1@�dZ@�=q@�@���@���@�r�@ˍP@�@�5?@ɡ�@�`B@�7L@���@�9X@��@�ff@Ų-@���@�A�@öF@�
=@���@�Ĝ@�(�@��w@���@��@�l�@�\)@�C�@�+@���@�ff@��@�7L@�Q�@�ƨ@��@��@��\@�E�@��^@��D@��@��w@���@�|�@�\)@���@���@��h@��`@���@�Z@� �@���@�l�@���@�V@���@��@��@�p�@�p�@�?}@���@��/@��@��D@�A�@��m@��w@���@�K�@�n�@�@��@��9@��@�Q�@�1'@���@��P@���@�@��@���@��@�%@��@��@�bN@� �@���@�;d@�+@��y@���@��\@�n�@�5?@��#@�@�hs@�%@��/@���@��D@���@�l�@�@��+@�E�@�@��T@�@���@�G�@��@��/@���@�r�@� �@���@�;d@�"�@�@�ȴ@�n�@��@�7L@��@��D@�Q�@��@���@��@�v�@��@��@�@���@�x�@��@��@�1'@�ƨ@���@�dZ@��@���@���@��R@�n�@���@�G�@���@��9@��u@�1'@��m@��
@��w@�|�@�C�@�o@�o@�o@�@�@��@��@��!@��+@�E�@�@��T@��h@�&�@��@�Ĝ@�Z@��@�|�@�;d@��H@���@�V@�{@���@��^@��@��@�%@���@��@�9X@��@���@�l�@�C�@���@�v�@�-@�J@��@���@���@��7@��@���@��/@��@�bN@��F@��@�l�@�S�@�;d@�+@��R@���@���@�~�@�$�@��#@��h@�`B@��@�Z@��m@��F@��P@�dZ@�;d@�
=@���@�v�@�^5@�=q@��^@�?}@��@��@�1'@�w@|�@;d@~��@~�@~�R@~��@~v�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��HA���A���A�1A�1A�A�%A�
=A�1A�A�
=A�1A�A�
=A�
=A�%A�%A�
=A�1A�%A�
=A�
=A�%A�%A�
=A�%A�A�
=A�1A�%A�1A�JA�1A�
=A�JA�1A�JA�1A�1A�
=A�JA�1A�JA�VA�
=A�1A�VA�JA�
=A�VA�VA�
=A�JA�VA�1A�1A�JA�
=A�1A�
=A�JA�1A�1A�
=A�
=A�1A�
=A�%A�1A�1A�1A�A�  A���A��A���A���A��A��A��A��A���A�  A���A���A��
A���A���A�ƨAվwAոRAոRAհ!Aհ!AնFAնFAա�A՛�Aգ�A՗�AՋDAՃAՁAՁAՃAՃA�|�A�|�A�~�A�t�A�n�A�n�A�ffA�dZA�jA�jA�ffA�l�A�`BA�S�A�XA�XA�S�A�VA�XA�S�A�Q�A�VA�S�A�O�A�Q�A�S�A�Q�A�Q�A�S�A�O�A�Q�A�VA�O�A�O�A�S�A�O�A�M�A�Q�A�K�A�O�A�Q�A�K�A�K�A�M�A�M�A�K�A�I�A�M�A�K�A�C�A�E�A�G�A�?}A�9XA�;dA�=qA�9XA�7LA�;dA�;dA�7LA�;dA�;dA�7LA�5?A�5?A�7LA�5?A�/A� �A�-A�&�A�"�A��A�(�A��A�oAԣ�A�t�A�v�A�|�AԃA�z�A�v�AԁA�~�A�r�A�A�A�`BAԅA�;dA�JAӶFA�r�A�\)A�?}A��A��mAҸRAҬAҧ�Aҡ�AҍPA�|�A�x�A�hsA�O�A�C�A�33A��A�bA�%A���A��A��;A���AѸRAѺ^AѸRAѲ-AѲ-AѲ-AѰ!Aѧ�Aџ�Aћ�AэPA�~�A�x�A�p�A�ffA�bNA�bNA�^5A�XA�XA�M�A�E�A�9XA�1'A�$�A�VA���A��A���A�ƨA�ĜA���AиRAЬAЧ�AЛ�AЅA�jA�M�A�=qA� �A�
=A���A��A��
A�ƨA϶FAϣ�Aϕ�A�|�A�ffA�\)A�I�A�;dA��A�  A���A��`A���Aΰ!A�-A��#A���AͼjAͰ!A͟�A͝�A͕�A͏\A͏\A͏\A͋DÁA�z�A�z�A�v�A�n�A�l�A�p�A�r�A�p�A�l�A�l�A�p�A�p�A�jA�VA�5?A�7LA�5?A�-A�$�A��A��A��A��A�bA�VA�VA�VA�%A��A̼jA̗�A�ffA�VA�VA�S�A�Q�A�K�A�;dA�&�A���AˮA˝�A˙�A˓uA˃A�r�A�XA�-A�JA��;A��A���A���A���AʾwAʶFAʧ�Aʣ�Aʇ+A�t�A�`BA�K�A�9XA�(�A��A�{A�bA�1A�%A�A��A��A��A��A��;A���A���A���A���A�AɼjAɲ-Aɣ�A�r�A�-A��A�bA���A���A��TA��
A���AȺ^AȬAȓuAȇ+AȁA�|�A�l�A�bNA�Q�A�9XA� �A�A���A��;A���A���A�AǮAǥ�Aǡ�Aǥ�Aǣ�Aǟ�AǗ�AǓuAǋDAǍPAǇ+A�x�A�S�A�=qA���Aƴ9A�z�A�XA�?}A��A�VA���A�|�A��TA��;A��A���A�ĜAĶFAĮAĲ-Aħ�Aě�Aď\AąA�t�A�?}A�/A��A�
=A���A��mA��#AüjAç�Aß�AÝ�AÛ�AËDA�z�A�l�A�hsA�dZA�Q�A�I�A�G�A�?}A�"�A��A���A�A�(�A�1A�  A���A��`A��A��!A���A��A�VA�A�A��A���A�`BA�1'A� �A��A�A���A��A��#A��DA��7A�p�A�`BA�M�A�5?A�$�A���A�ƨA��FA���A�|�A�^5A�;dA�1'A�1'A�&�A��yA���A��jA���A��+A�bNA�M�A�/A��9A�n�A�&�A��A��jA��7A�\)A�/A��;A�?}A���A��;A��
A���A�ZA�7LA�JA���A��;A���A���A���A���A��\A�|�A�n�A�l�A�jA�dZA�^5A�ZA�XA�O�A�9XA�%A��A�?}A��#A�v�A�?}A�%A�ƨA���A�K�A��;A�S�A�ƨA���A�n�A�ZA�A�A� �A�VA�A���A��
A�A��9A��9A��!A��A��-A���A��PA�z�A�bNA�G�A��mA�\)A��A��A�A���A��RA�x�A��#A�ƨA�ȴA�ȴA�ƨA�A��A��TA�\)A�JA��HA���A�ffA�`BA�Q�A�A�A�&�A�
=A��A��A�`BA�oA�ƨA�z�A�(�A�JA�A���A���A��A���A��+A�Q�A��A��jA�`BA�33A�{A��A���A�`BA��A���A���A���A�l�A�$�A��A���A��hA��PA��A��A�z�A�v�A�p�A�\)A�-A��A�VA�%A���A��A��TA��
A���A�ȴA�A��RA���A���A��hA��DA��+A�v�A�n�A�hsA�ZA�?}A���A��FA��A���A���A���A��\A��7A��A�t�A�n�A�A�A�ȴA�ZA�&�A��`A���A�VA�A�A�-A��A�%A��#A��RA��A�^5A�C�A��A��`A���A�?}A��
A���A��\A�|�A�G�A��A��FA���A���A���A��+A�l�A�S�A�A�A�"�A��9A�dZA�=qA�
=A���A�ffA�A�A�=qA�
=A��/A���A���A���A��
A��;A��;A��/A�ĜA��7A�`BA�/A��A��A�(�A�1'A�33A�33A�/A��A�oA�A���A��A��TA���A�A��FA���A��DA�n�A�K�A� �A���A���A���A���A��uA��\A�v�A�A��uA�bNA�1A��+A�&�A��HA�p�A�A�A�r�A�\)A�C�A��A�{A��mA��/A��
A���A���A�ȴA�ĜA�ĜA�ĜA�A���A��jA���A�S�A�ƨA�l�A�A���A���A���A��A�\)A��A���A���A�E�A���A��A��A�\)A�E�A�=qA�-A� �A�{A�1A��A��A�ffA�5?A��HA�M�A��A��A��A��^A�ffA�K�A��A�V111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                                       11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A�%A�1A�1A�1A�%A�1A�1A�1A�1A�1A�
=A�
=A�
=A�JA�
=A�
=A�1A�  A��A��;Aպ^Aգ�A�~�A�ffA�VA�Q�A�M�A�A�A�7LA���A�l�A��A�E�A���A�v�A��AЅAϸRA���Aͩ�A�x�A�ZA��ȂhA���A��A�n�A�A�ȴA�JAȇ+A��Aǝ�A�1A�x�Aģ�A�AËDA�-A��yA���A���A��`A�%A��HA��yA�7LA��\A�$�A��TA�l�A�ĜA�"�A�|�A��A�5?A���A��A���A��A�`BA��A���A��A��7A�A���A�=qA���A�G�A��A���A�+A���A�v�A�~�A�ffA��FA�ȴA��A�M�A�A��yA�A�A�1A��\A�JA�l�A�M�A���A�|�A�5?A|�uAx�`Ar�\Apz�AnAiC�AcA_��A^1'A]7LA\1AZ�jAY"�AQ�;AMp�AJ��AIAGXAB�HAA��A<5?A9��A8ZA6bNA4��A2ȴA0~�A.�A-7LA,ffA+�^A*�\A)t�A(�A(�+A(JA'XA%t�A#�A"^5A ZA�A=qA��A��AhsA��AƨA%A=qA��A/A�HA�wA��A�\A(�A�mA�PA�yAM�A�AA�A�mA�;AA\)A��A�\A�A�\A��AQ�A�TA�FA"�A�`A�HA��A-Ap�AȴA^5A9XA
=A�wA33A
�yA
��A
~�A
=qA
1A	�7A	�A	x�A	G�A�A�#A+A��A�AXA�uA��A�A�uA~�A�!A��At�A��A��A�7A1A�\A�wA ��A ZA Q�A  �@�l�@��R@��@��@�\)@�Ĝ@�A�@��@��T@�&�@���@�Ĝ@��@���@�z�@���@���@��#@�/@�|�@�S�@��@�ȴ@@�n�@�V@�v�@�V@�$�@홚@�X@��@�b@���@�V@�-@���@��@�@���@�;d@���@�R@��@�?}@䛦@�@��@�5?@��#@���@� �@ߥ�@�;d@���@���@���@۾w@��H@���@�G�@�X@��@�S�@֧�@�p�@��
@�@�hs@��@�j@�1'@� �@�1@�dZ@�=q@�@���@���@�r�@ˍP@�@�5?@ɡ�@�`B@�7L@���@�9X@��@�ff@Ų-@���@�A�@öF@�
=@���@�Ĝ@�(�@��w@���@��@�l�@�\)@�C�@�+@���@�ff@��@�7L@�Q�@�ƨ@��@��@��\@�E�@��^@��D@��@��w@���@�|�@�\)@���@���@��h@��`@���@�Z@� �@���@�l�@���@�V@���@��@��@�p�@�p�@�?}@���@��/@��@��D@�A�@��m@��w@���@�K�@�n�@�@��@��9@��@�Q�@�1'@���@��P@���@�@��@���@��@�%@��@��@�bN@� �@���@�;d@�+@��y@���@��\@�n�@�5?@��#@�@�hs@�%@��/@���@��D@���@�l�@�@��+@�E�@�@��T@�@���@�G�@��@��/@���@�r�@� �@���@�;d@�"�@�@�ȴ@�n�@��@�7L@��@��D@�Q�@��@���@��@�v�@��@��@�@���@�x�@��@��@�1'@�ƨ@���@�dZ@��@���@���@��R@�n�@���@�G�@���@��9@��u@�1'@��m@��
@��w@�|�@�C�@�o@�o@�o@�@�@��@��@��!@��+@�E�@�@��T@��h@�&�@��@�Ĝ@�Z@��@�|�@�;d@��H@���@�V@�{@���@��^@��@��@�%@���@��@�9X@��@���@�l�@�C�@���@�v�@�-@�J@��@���@���@��7@��@���@��/@��@�bN@��F@��@�l�@�S�@�;d@�+@��R@���@���@�~�@�$�@��#@��h@�`B@��@�Z@��m@��F@��P@�dZ@�;d@�
=@���@�v�@�^5@�=q@��^@�?}@��@��@�1'@�w@|�@;d@~��@~�@~�R@~��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��HA���A���A�1A�1A�A�%A�
=A�1A�A�
=A�1A�A�
=A�
=A�%A�%A�
=A�1A�%A�
=A�
=A�%A�%A�
=A�%A�A�
=A�1A�%A�1A�JA�1A�
=A�JA�1A�JA�1A�1A�
=A�JA�1A�JA�VA�
=A�1A�VA�JA�
=A�VA�VA�
=A�JA�VA�1A�1A�JA�
=A�1A�
=A�JA�1A�1A�
=A�
=A�1A�
=A�%A�1A�1A�1A�A�  A���A��A���A���A��A��A��A��A���A�  A���A���A��
A���A���A�ƨAվwAոRAոRAհ!Aհ!AնFAնFAա�A՛�Aգ�A՗�AՋDAՃAՁAՁAՃAՃA�|�A�|�A�~�A�t�A�n�A�n�A�ffA�dZA�jA�jA�ffA�l�A�`BA�S�A�XA�XA�S�A�VA�XA�S�A�Q�A�VA�S�A�O�A�Q�A�S�A�Q�A�Q�A�S�A�O�A�Q�A�VA�O�A�O�A�S�A�O�A�M�A�Q�A�K�A�O�A�Q�A�K�A�K�A�M�A�M�A�K�A�I�A�M�A�K�A�C�A�E�A�G�A�?}A�9XA�;dA�=qA�9XA�7LA�;dA�;dA�7LA�;dA�;dA�7LA�5?A�5?A�7LA�5?A�/A� �A�-A�&�A�"�A��A�(�A��A�oAԣ�A�t�A�v�A�|�AԃA�z�A�v�AԁA�~�A�r�A�A�A�`BAԅA�;dA�JAӶFA�r�A�\)A�?}A��A��mAҸRAҬAҧ�Aҡ�AҍPA�|�A�x�A�hsA�O�A�C�A�33A��A�bA�%A���A��A��;A���AѸRAѺ^AѸRAѲ-AѲ-AѲ-AѰ!Aѧ�Aџ�Aћ�AэPA�~�A�x�A�p�A�ffA�bNA�bNA�^5A�XA�XA�M�A�E�A�9XA�1'A�$�A�VA���A��A���A�ƨA�ĜA���AиRAЬAЧ�AЛ�AЅA�jA�M�A�=qA� �A�
=A���A��A��
A�ƨA϶FAϣ�Aϕ�A�|�A�ffA�\)A�I�A�;dA��A�  A���A��`A���Aΰ!A�-A��#A���AͼjAͰ!A͟�A͝�A͕�A͏\A͏\A͏\A͋DÁA�z�A�z�A�v�A�n�A�l�A�p�A�r�A�p�A�l�A�l�A�p�A�p�A�jA�VA�5?A�7LA�5?A�-A�$�A��A��A��A��A�bA�VA�VA�VA�%A��A̼jA̗�A�ffA�VA�VA�S�A�Q�A�K�A�;dA�&�A���AˮA˝�A˙�A˓uA˃A�r�A�XA�-A�JA��;A��A���A���A���AʾwAʶFAʧ�Aʣ�Aʇ+A�t�A�`BA�K�A�9XA�(�A��A�{A�bA�1A�%A�A��A��A��A��A��;A���A���A���A���A�AɼjAɲ-Aɣ�A�r�A�-A��A�bA���A���A��TA��
A���AȺ^AȬAȓuAȇ+AȁA�|�A�l�A�bNA�Q�A�9XA� �A�A���A��;A���A���A�AǮAǥ�Aǡ�Aǥ�Aǣ�Aǟ�AǗ�AǓuAǋDAǍPAǇ+A�x�A�S�A�=qA���Aƴ9A�z�A�XA�?}A��A�VA���A�|�A��TA��;A��A���A�ĜAĶFAĮAĲ-Aħ�Aě�Aď\AąA�t�A�?}A�/A��A�
=A���A��mA��#AüjAç�Aß�AÝ�AÛ�AËDA�z�A�l�A�hsA�dZA�Q�A�I�A�G�A�?}A�"�A��A���A�A�(�A�1A�  A���A��`A��A��!A���A��A�VA�A�A��A���A�`BA�1'A� �A��A�A���A��A��#A��DA��7A�p�A�`BA�M�A�5?A�$�A���A�ƨA��FA���A�|�A�^5A�;dA�1'A�1'A�&�A��yA���A��jA���A��+A�bNA�M�A�/A��9A�n�A�&�A��A��jA��7A�\)A�/A��;A�?}A���A��;A��
A���A�ZA�7LA�JA���A��;A���A���A���A���A��\A�|�A�n�A�l�A�jA�dZA�^5A�ZA�XA�O�A�9XA�%A��A�?}A��#A�v�A�?}A�%A�ƨA���A�K�A��;A�S�A�ƨA���A�n�A�ZA�A�A� �A�VA�A���A��
A�A��9A��9A��!A��A��-A���A��PA�z�A�bNA�G�A��mA�\)A��A��A�A���A��RA�x�A��#A�ƨA�ȴA�ȴA�ƨA�A��A��TA�\)A�JA��HA���A�ffA�`BA�Q�A�A�A�&�A�
=A��A��A�`BA�oA�ƨA�z�A�(�A�JA�A���A���A��A���A��+A�Q�A��A��jA�`BA�33A�{A��A���A�`BA��A���A���A���A�l�A�$�A��A���A��hA��PA��A��A�z�A�v�A�p�A�\)A�-A��A�VA�%A���A��A��TA��
A���A�ȴA�A��RA���A���A��hA��DA��+A�v�A�n�A�hsA�ZA�?}A���A��FA��A���A���A���A��\A��7A��A�t�A�n�A�A�A�ȴA�ZA�&�A��`A���A�VA�A�A�-A��A�%A��#A��RA��A�^5A�C�A��A��`A���A�?}A��
A���A��\A�|�A�G�A��A��FA���A���A���A��+A�l�A�S�A�A�A�"�A��9A�dZA�=qA�
=A���A�ffA�A�A�=qA�
=A��/A���A���A���A��
A��;A��;A��/A�ĜA��7A�`BA�/A��A��A�(�A�1'A�33A�33A�/A��A�oA�A���A��A��TA���A�A��FA���A��DA�n�A�K�A� �A���A���A���A���A��uA��\A�v�A�A��uA�bNA�1A��+A�&�A��HA�p�A�A�A�r�A�\)A�C�A��A�{A��mA��/A��
A���A���A�ȴA�ĜA�ĜA�ĜA�A���A��jA���A�S�A�ƨA�l�A�A���A���A���A��A�\)A��A���A���A�E�A���A��A��A�\)A�E�A�=qA�-A� �A�{A�1A��A��A�ffA�5?A��HA�M�A��A��A��A��^A�ffA�K�A��A�V111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                                       11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��B
�B
��B
�OB
�B
��B
�VB
��B
��B
��B
�!B
�VB
��B
��B
��B
��B
��B
�OB
�OB
�\B
�hB
��B
��B
��B
��B
��B
�XB
��B
��B
��B
�XB
�tB
�=B
�=B
�9B
�'B
�^B
ݘB
��B%FBM�B��B��B��B�B�B��B��B�B��B�%B�rB�%B�JB�cB�B�B�B�BuB�BYB�B�B$B#B#�B$@B#nB#�B!�B(�B�B�BIB4nB'�B~B�B
	B;BBFB1BYB�B�B�B�;B��BĜB��B�?B��B�qB�gB�B��B��Bz�Ba�B]/B>�B'RB\B
�VB
�B
��B
��B
�$B
�(B
o5B
\]B
R�B
F�B
&�B
�B	�cB	�B	��B	�3B	��B	� B	��B	��B	z�B	pB	`B	>BB	33B	'�B	-B	{B	:B	YB	uB	uB	uB	(B	:B	+B	�B	DB	�B	~B	�B	�B	FB	�B	�B	�B	kB	@B	uB	SB	�B	�B	�B	 'B	$B	($B	1�B	5�B	B[B	HKB	HKB	I�B	MB	OB	S&B	T�B	V�B	Y�B	_�B	c�B	c B	b�B	b�B	bNB	b�B	c�B	iDB	l�B	o5B	poB	p�B	u%B	z�B	w2B	xlB	x�B	zxB	yrB	y�B	��B	�"B	��B	��B	��B	��B	��B	��B	�\B	�-B	��B	��B	��B	�qB	�CB	��B	�'B	��B	��B	�hB	�xB	��B	�B	��B	��B	�B	��B	�+B	��B	��B	��B	�tB	�<B	�dB	��B	�B	��B	��B	��B	�!B	��B	��B	��B	�}B	��B	��B	��B	�=B	�nB	��B	��B	�LB	��B	�LB	�LB	�LB	�RB	��B	��B	��B	�*B	��B	�^B	��B	��B	�*B	��B	�6B	�B	��B	�UB	��B	��B	ĜB	ŢB	�B	�B	�mB	��B	��B	�B	ɺB	ɆB	�RB	�^B	�dB	�jB	�BB	�}B	��B	бB	� B	уB	� B	ҽB	�[B	�TB	��B	͟B	��B	ϫB	�HB	ҽB	��B	��B	�TB	�gB	�B	��B	��B	یB	�]B	�]B	�)B	�)B	�5B	��B	�pB	ߤB	�HB	�HB	�B	� B	�,B	��B	�,B	��B	��B	�mB	�
B	�DB	�B	�QB	��B	�"B	�B	�oB	�B	�B	�MB	�MB	�B	�B	�B	�MB	�MB	�B	�%B	�ZB	�+B	�8B	�8B	��B	�rB	�DB	�B	�B	��B	�(B	�(B	�(B	�]B	�(B	��B
 �B
B
B
AB
uB
�B
�B
�B
�B
�B
�B
YB
YB
%B
%B
�B
�B
_B
�B
�B
�B
	7B
	7B
�B

	B
�B
B
�B
(B
\B
�B
�B
�B
�B
4B
@B
B
:B
�B
@B
�B
�B
�B
FB
MB
MB
B
�B
SB
SB
SB
�B
$B
�B
�B
+B
+B
+B
�B
�B
7B
�B
�B
B
CB
xB
xB
xB
�B
�B
B
OB
OB
�B
�B
 �B
 �B
 �B
!bB
"hB
#nB
$B
#�B
#�B
#�B
$B
#�B
$@B
'RB
&LB
&�B
&�B
&�B
&�B
'�B
(XB
)*B
)_B
)�B
)�B
*�B
*�B
+B
+B
+kB
-B
-�B
.B
-�B
.B
.�B
.�B
.�B
/B
/�B
0!B
0�B
0UB
0�B
0�B
0�B
0�B
0�B
1[B
1�B
1�B
2aB
2�B
3hB
3hB
33B
4nB
5tB
8B
7�B
7�B
7LB
6FB
7B
7B
7�B
7�B
7�B
8�B
8RB
9$B
9�B
:^B
:�B
:�B
<B
;�B
<jB
;�B
;�B
;�B
;�B
;�B
;dB
<�B
=B
=qB
=qB
=qB
=�B
?HB
>�B
>�B
?B
?B
?HB
@B
?�B
?�B
@OB
@�B
AUB
@�B
@�B
A�B
C-B
C-B
CaB
C�B
C�B
CaB
C�B
D�B
D�B
D�B
D�B
FB
FtB
FtB
F�B
HB
HB
H�B
H�B
H�B
IB
IB
IRB
IG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@���B
��B
�1B
��B
��B
��B
��B
�B
��B
��B
�~B
�B
��B
�B
�B
�VB
��B
�B
�OB
��B
�OB
�B
��B
�VB
�OB
��B
�\B
�OB
��B
��B
�VB
��B
��B
��B
��B
�'B
��B
�VB
�!B
�B
�~B
��B
��B
�B
��B
��B
�B
��B
��B
�OB
�B
��B
�VB
�IB
��B
�VB
�IB
�OB
�!B
�B
�IB
�VB
��B
��B
�~B
��B
�B
��B
�VB
��B
��B
��B
��B
��B
��B
�bB
��B
��B
�4B
�4B
��B
��B
�OB
��B
�XB
�B
��B
�0B
�6B
�eB
�qB
��B
��B
��B
�6B
�B
�OB
�UB
�qB
��B
�nB
�aB
�B
�B
�3B
��B
�tB
�9B
�hB
��B
�zB
�tB
��B
��B
�B
��B
�B
�B
��B
��B
��B
��B
��B
�XB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�$B
�*B
��B
��B
�^B
�^B
�$B
�*B
��B
�$B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�^B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�$B
�^B
�$B
�RB
��B
�*B
�XB
��B
�B
�^B
�6B
�aB
��B
��B
��B
�B
��B
�FB
ɺB
�=B
��B
��B
��B
�CB
�B
�qB
�B
�UB
�!B
��B
�!B
�3B
��B
��B
�eB
��B
�qB
�kB
�=B
��B
�B
�0B
��B
��B
�}B
�B
��B
��B
��B
�?B
��B
�RB
�$B
��B
�<B
��B
��B
ĜB
B
��B
�3B
��B
�[B
��B
�9B
��B
�mB
ȀB
˒B
�XB
��B
�B
�BB
�<B
�pB
�B
��B
҉B
��B
��B
�9B
�#B
��B
�,B
��B
�/B
�B
�B
�;B
�B
�ZB
�B
�2B
��B�B�B�B\BB�B�B�B#B%zB)*B,�B2aB6FB6�B:�B?BGBJXBK�BO�BV9B\�B�GB��B��B��B�B�_B�0B��B��B��B�kB�qB��B�IB�wB��B�UB��B��B��B��B��B�UB�B�}B�!B�tB�wB��B��B�<B�B��B�B�OB�B��B��B�OB��BBƨB�B��B��B��B֡B՛B՛B��BیB�5B��B�B�>B�yB��B�B��B�vB��B�iB�KB�WB��B�B��B��B��B� B�B�`B��B��B�>B�2B�2B��B��B�ZB��B�B�B��B��B�MB�B��B��B�%B��B��B�B�%B�2B��B�VB�B�DB��B�8B�`B�fB��B��B�ZB�`B��B��B�B�B�B�B��B��B�lB�xB��B�(B��B��B��B �BuB  B��B��B��B��B  B 4B��B�B��BAB�B�B	�B	7B�B_B
=BYBSB)�BuB�B�B�B�BBB BhB�B�B�B�B�BSB�BFBuB:BB�B�BoBhB�B�B�B@BhB:B�B�B�B@BVB�B�B �B$�BYBFB�BB�BMB�BkB�B B.B7B	B�B�B�B�B	BCB"4B-�B�B&�B �B"�B!�B"�B(�B%�B#:B!�B%B$@B%�B"4B!bB!�B(�BVBVB"�B!-B!bB�B!-B.�B"�B(XB$B"�B#nBOB$�B%zB6�B$B�BqB($B"hB&B'B#:B#�B#nB"�B%FB#B%zB#�B%�B!�B �B \B �B�B�BIB!B$�B#�B*�B.IB*0B1'B'RB$�B%�BVB&�B%FB7B1B\B�BbB\B�B�BBbB�B�BxB�B�B�BuB�B�B$B�B*�B2�B+B.IB6�B8B1�B<�BF?B,�B*0B)�B(�B(�B,�B;�B'B�B+B�B�B
=BxBB	�B\B	�BB�B�BVB�B�BDB	B
=B	7B	7B
�B�B_B�B�B�B�BB�(B��BVB��B�JB�rBYB��B iB;B  B�BDB�BbB�B�BuB�B�B�BeB+B�B�B�B	B�B_B�B$B�B�BYBSB�B�BB�BBB)�B�B4B�B"B(B�B�B�BB�B�B4B��B��B��B�DB�2B�B�B�&B�,B�ZB�B��B՛B��BҽB�B�XB��B��B�<B�^B�dB�gB��B�B�OB�3BǮB�)B�BбB�B�B�mB�B��B��B�HB��B�B�hB��B�aB�B�B�B�B�[B�B��B��B�-B�6B� B�B��B�FB��B��BĜB��BȀB��B�B�3B��B�'B�[B� B�}B�B�HB�B��B��B��B��B�UB��B�qB��B��B�^B�wB�4B�LB�B�	B�4B��B��B�BuZBx�Bx8B�4Bh�Bd�Bc�Bc�BcTBbNBa�B_pB^5B]�B\]B[�B]�Bd�B[�BR�Bh�BC�BI�B>BB<6BB�BD3BAUB/OBC�B3�B0�B!-B�B�B�BFB�B.B\B�BxB�BB4BB
�rB
�;B
��B
�`B
�yB
�B
��B
��444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                       44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                       44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202302102309422023021023094220230210230942202302102309422023021023094220230210230942SI  SI  ARFMARFM                                                                                                                                                2022052711172720220527111727IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022060607012720220606070127QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�205F03E         200703E         AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022060607012720220606070127QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2023021013194520230210131945IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                