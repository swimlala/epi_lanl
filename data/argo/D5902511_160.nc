CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-02-07T20:32:19Z creation; 2021-04-29T20:27:10Z DMQC;      
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
_FillValue        G�O�     h  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \p   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     h  dL   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     h  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     h  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     h  �<   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     h  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     h �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 8P   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     h @,   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � _�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     h gp   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �8   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �8   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �8   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �8   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �,   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �H   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �P   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �p   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �x   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210207203219  20210429202819  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_160                 6810_008521_160                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @�\�����@�\�����11  @�\��&��@�\��&��@1�����@1������e#�w[�0�e#�w[�011  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  ?�@:�H@}p�@�G�@�G�@�G�A   AG�A!G�A+�A?\)A`��A�  A�  A��A��A��AϮA߮A�  B Q�Bz�BQ�B  B�
B(  B0  B7�
B?�
BH  BP  BX  B`  Bh  Bp  Bx  B�{B�{B�{B�  B�  B�{B�  B��
B��
B��B�{B�  B�  B�  B�{B�  B��B��B��B��B�  B�  B�  B�  B�  B�  B�  B�{B�{B��B��B�  C 
=C
=C{C
=C{C
  C  C  C  C
=C  C��C��C  C{C
=C��C"  C${C&{C(
=C*  C+��C.  C0  C2
=C4
=C6
=C8  C:  C<  C=��C@  CB
=CD
=CF  CG��CJ  CL
=CM��CO��CR  CT  CU��CW��CY��C[�C]��C`  Cb
=Cd
=Cf  Cg��Cj  Cl
=Cn
=Cp
=Cq��Ct  Cv{Cx
=Cz  C|  C~
=C�  C�  C�C�C���C���C�C�  C�  C�C�  C�  C���C�  C���C�  C���C�C�  C�
=C�C���C�  C���C�
=C�C�  C�C���C�  C�  C�  C�  C�C�  C�C�C�  C�  C�C�  C�C�  C���C�  C�  C�  C�  C�  C���C�  C�  C�  C���C���C���C�  C���C���C���C�  C�  C���C�  C�  C�C�
=C�
=C�
=C�C�C�C�  C�  C�  C�  C���C���C�  C���C�  C�C�  C���C�  C�  C���C���C���C���C�C�C�C�  C�  C���C���C�  C�  C�C�
=C�  C���C���C���C�  C�  C���C���C���C���C���C���C�  C�C�C�C���C�  C�
=C���C���C�  C�
=C�
=C���C�C�D �D � D�D��D�D��D�D� DD�D  D}qD�qD� D�D}qD�D��D	�D	��D	�qD
� D
��D}qD�D� D�D��D  D� D�D��DD��DD��D  D� D  D� D  D��D�D��DD�D�qD}qD  D��D�D� D�qD}qD�qD��D  D}qD  D�D�D� D��Dz�D   D ��D!  D!}qD"  D"� D#  D#� D$�D$��D$�qD%z�D&  D&��D&�qD'}qD(�D(��D)  D)� D*  D*}qD+�D+� D,�D,��D-�D-}qD-�qD.� D/�D/�D0�D0� D1  D1��D2�D2��D3  D3}qD3��D4}qD5�D5��D6�D6� D6�qD7}qD8  D8��D8�qD9� D:  D:}qD;  D;��D<  D<� D=�D=��D>�D>��D?  D?}qD?�qD@�DA  DAz�DA�qDB� DC  DC� DD�DD��DE�DE}qDF�DF�DG  DG}qDG�qDHz�DH�qDI� DJ  DJ� DJ��DK}qDL  DL}qDM�DM� DN  DN� DO  DO��DP  DP� DQ  DQ� DR  DR� DS  DS� DS�qDTz�DU�DU��DVDV� DV�qDW� DX  DX}qDY  DY� DZ  DZ� D[  D[� D\  D\� D\�qD]� D^D^��D_�D_�D`  D`}qDa�Da��Db�Db��Dc�Dc� Dc��Dd}qDd�qDe}qDe�qDf� Dg�Dg}qDh  Dh� Di  Di� Dj�Dj��Dk�Dk�Dl  Dl� Dl�qDmz�Dm�qDn}qDn�qDo��Dp�Dp� Dp�qDqz�Dq�qDr� Ds  Ds}qDs�qDt}qDu  Du��Dv�Dv� Dw  Dw��Dx  Dx}qDx��Dy}qDy�qDz� D{  D{� D|  D|� D}  D}� D~  D~� DD�D�  D�=qD�}qD�� D�HD�B�D���D�D�  D�>�D��HD�D�HD�AHD��HD��HD��D�B�D��HD��qD�  D�@ D�� D��HD���D�@ D���D��HD�  D�B�D�� D�� D�  D�>�D�~�D��qD���D�>�D�� D��HD��D�C�D��HD��HD��D�AHD�� D�� D�HD�@ D��HD��HD�  D�@ D��HD��HD�  D�>�D�~�D��qD���D�>�D�� D�� D���D�>�D�� D���D���D�AHD��HD���D��qD�>�D��HD�� D�HD�@ D�}qD��qD�  D�AHD��HD��HD�HD�AHD�~�D���D�  D�AHD��HD�� D�HD�>�D�~�D�� D�HD�>�D�}qD�� D�HD�@ D�� D�D��D�B�D���D��HD�  D�@ D�~�D���D���D�@ D�� D��HD�HD�>�D�~�D�� D�  D�AHD�� D�� D��D�AHD��HD��HD�HD�@ D�~�D�� D�HD�@ D��HD�� D�  D�@ D�~�D�� D�HD�>�D��HD��HD�  D�AHD�� D���D�  D�@ D��HD�� D�  D�AHD��HD���D�HD�AHD�� D�� D�HD�@ D�� D��HD�  D�@ D�~�D�� D�  D�>�D�~�D��HD��D�AHD�� D��HD���D�@ D�~�D��qD�HD�B�D���D��HD���D�>�D�� D�� D�HD�@ D�~�D�� D�  D�=qD�~�D�� D�  D�>�D�~�D��HD�  D�=qD�~�D���D�  D�>�D�~�D�� D�HD�AHD�� D�� D�  D�AHD�� D���D�  D�@ D�� D�� D�  D�@ D�� D���D���D�@ D�� D�� D���D�@ D�� D���D�  D�>�D�}qD�� D��D�@ D�~�D��qD���D�=qD�|)D���D�  D�@ D�~�D��HD�  D�=qD�}qD�� D�  D�>�D�}qD�� D�HD�@ D�� D��HD�HD�@ D�� D�� D�  D�AHD�D��HD�  D�@ DÀ D��HD��D�AHDĀ D�� D�  D�@ Dŀ D�� D���D�=qDƀ Dƾ�D�  D�>�Dǀ D�� D�HD�B�DȀ D�D��D�@ D�}qDɾ�D�HD�AHDʁHD�� D�  D�AHDˁHD�D�HD�@ D�~�D̾�D�  D�AHD̀ D�� D�  D�>�D�}qD�� D�HD�@ DρHD�� D���D�>�DЀ D��HD�  D�>�Dр D��HD�HD�@ DҀ D��HD�HD�>�D�~�D��HD��D�AHDԁHD��HD�  D�>�D�~�D��HD�  D�AHDցHDֽqD�  D�AHDׁHD�� D�  D�B�D؁HD�� D���D�>�Dق�D�� D�  D�AHDځHD��HD�  D�=qDۀ D�� D���D�AHD܁HD��HD�HD�AHD�~�D�� D�HD�AHDނ�D��HD���D�@ D߁HD߾�D�  D�AHD�� D��HD�  D�>�D�}qD�qD���D�@ D� D��HD�  D�@ D� D�� D��qD�>�D� D��HD�HD�@ D�~�D�� D�  D�@ D� D澸D���D�>�D�HD��HD���D�=qD� D�� D�HD�@ D� D�� D�  D�B�D�HD꾸D�HD�AHD�HD��HD���D�>�D� D�� D�  D�AHD�HD�� D���D�>�D�HD�� D���D�>�D� D�� D�HD�AHD�� D�D�HD�B�D� D�� D�HD�AHD� D�� D���D�@ D� D��HD�HD�@ D�~�D���D�  D�@ D�~�D���D���D�@ D��HD�� D�HD�AHD�~�D�� D�HD�@ D�� D��HD���D�AHD��HD���>���?L��?�\)?\?�@
=q@&ff@8Q�@Tz�@n{@�G�@��@�(�@�ff@���@��R@�=q@�33@�\@���@�
=A�\A
=Ap�A�
A�A{A$z�A)��A0��A5�A:�HAAG�AEAL��AS33AW
=A^{Adz�AhQ�An�RAuAz=qA�  A�33A�A�Q�A��A�p�A���A�33A�p�A���A��HA��A��A��\A�(�A�\)A���A��A�ffA���A��HA�A���A��HA�p�A���A�33A��A�Q�A�=qA���A�  Aљ�A�z�A�\)A�G�A�33A�ffA�  A�\A��A�\)A���A�(�A�RA�Q�A�A�A��A��\A��A��RB z�B{B�HB  Bp�B�\B�B��B
{B
�HB�
BG�B{B
=BQ�BG�B�B�HB  B��BG�B=qB33B�BQ�B��Bp�BffB�HB\)BQ�B��B��B=qB�RB�B z�B!�B!��B"�\B#33B#�B$��B%��B&{B'
=B'�
B(z�B)�B*=qB+
=B+�B,��B-��B-�B/
=B0  B0z�B1G�B2ffB333B3�B4��B5p�B6=qB733B8  B8z�B9p�B:�\B:�HB<(�B<��B=p�B>�\B?
=B?�B@��BA��BB{BC33BC�
BDQ�BEG�BF=qBF�\BG\)BHQ�BH��BIp�BJffBK33BK�BLz�BM��BM�BN�RBO�
BPQ�BQ�BR{BR�RBS
=BT(�BT��BU�BV{BV�HBW\)BX(�BX��BYBZ{B[
=B[�
B\  B\��B]�B^ffB_
=B_�
B`��B`��Bb{Bb�HBc
=BdQ�Bd��BeG�Bf�\Bf�HBg�Bh��Bi�Bi��Bj�HBk\)Bk�
Bl��Bmp�Bn{Bo
=Bo�
Bp(�Bq�Br{BrffBs\)Bt  Btz�Bup�Bv=qBv�RBw\)BxQ�By�By��Bz�RB{�B{�
B|��B}�B~ffB�B�=qB�ffB���B�p�B���B�=qB���B��HB��B�  B�=qB���B�33B��B��B�ffB��HB�G�B���B�(�B�z�B���B�G�B��
B�  B��\B�
=B�\)B��B�=qB��RB���B�\)B��B�ffB���B��B��B�(�B��\B��HB��B�  B�=qB��HB�p�B�B�=qB��HB�\)B�B�(�B��HB�G�B��B�=qB���B�33B���B�Q�B���B��B��B�(�B�z�B���B��B�{B�ffB�
=B���B�  B�ffB���B���B�(�B�z�B���B��B�(�B���B�
=B��B�=qB��\B��B�B�=qB�z�B�G�B�B�{B��\B��B��B�  B���B�33B��B�{B���B���B�p�B�(�B�ffB���B���B�  B�ffB��B��B��
B�ffB���B�G�B��
B�ffB���B��B�B�=qB��\B��B��B�  B�ffB���B�p�B�B�Q�B���B�\)B��B�ffB���B�33B�B�Q�B��RB��B��B�=qB\B�
=BîB�{B�ffB�
=B�p�B�B�ffB��HB�33B�B�Q�Bȣ�B�
=Bə�B�{B�z�B���B�\)B��B�(�Ḅ�B�33B�B�{BΏ\B�33Bϙ�B��BЏ\B��B�p�B��B�z�B��HB�G�B��B�ffBԸRB�\)B��
B�=qB֏\B�G�B�B�  B�z�B�33BمB��B�z�B�
=B�G�B�B�ffBܸRB�
=BݮB�(�Bޏ\B��HB߅B��B�=qB���B�\)B�B�=qB�RB�
=B㙚B�(�B�z�B���B�\)B��B�(�B�\B�
=B癚B��B�=qB���B�G�B陚B��B�ffB���B�\)B뙚B�{B�RB�33B�B��B�\B��B�p�B��
B�ffB�
=B�\)B��B�z�B��HB�G�B�B�ffB���B�33B��
B�Q�B���B��B��B�=qB��\B���B���B�{B�Q�B���B�p�B��B�(�B���B�G�B���B��B�z�B�
=B�\)B�C {C ffC ��C ��C  CG�C�\C��C��C(�Cz�CC�HC(�Cz�C��C�
C(�CffC�\C��C{C\)C�C�C��C=qCp�C��C�
C�CffC��C��C  CG�C��CC�C	(�C	p�C	�RC	�HC

=C
Q�C
��C
�
C  C=qC�\CC�C�CffC�C�C{CG�C�C��C{C=qCffC�C��C=qCffC��C�HC33CQ�C�\C�
C�C\)C�\CC  CQ�C�\CC��CG�C�C�C�C=qCp�C��C�C33CQ�C�\C�
C�CG�Cz�CC
=C33CffC�RC�HC{C\)C�C��C  CG�C�\C�RC�C33Cz�C��C�
C(�CffC��C�
C(�Cp�C��C��C�Cp�C�C�
C�Cz�C�RC�HC33C�C�C�C G�C �C �C!
=C!Q�C!�C!�C"  C"G�C"�C"�C"��C#=qC#�C#�C#�HC$=qC$z�C$�C$�HC%�C%p�C%�C%�HC&{C&ffC&�C&�
C'�C'ffC'��C'�
C(
=C(\)C(�C(�HC){C)Q�C)��C)�HC*{C*Q�C*��C*�C+�C+\)C+�C,  C,(�C,\)C,�C-  C-=qC-p�C-C.
=C.33C.�C.�
C/
=C/=qC/�\C/�HC0
=C0Q�C0��C0�
C1{C1p�C1�C1�HC2(�C2p�C2C2�C333C3z�C3C3��C433C4�C4��C4��C5G�C5�\C5��C5��C6=qC6��C6�
C7  C7Q�C7��C7�C8{C8Q�C8�C8��C9�C9ffC9�RC9�C:�C:ffC:�RC:��C;(�C;z�C;��C<  C<=qC<�\C<�
C=
=C=G�C=�\C=�HC>(�C>Q�C>�\C>�HC?33C?ffC?��C?�C@=qC@p�C@�CA  CAG�CAp�CACB�CBQ�CB�CB�
CC(�CCffCC��CC�HCD(�CDz�CD�CD�HCE33CEz�CECE�CF(�CFp�CF�RCF��CG(�CGffCG�RCH
=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                    ?�  ?�@:�H@}p�@�G�@�G�@�G�A   AG�A!G�A+�A?\)A`��A�  A�  A��A��A��AϮA߮A�  B Q�Bz�BQ�B  B�
B(  B0  B7�
B?�
BH  BP  BX  B`  Bh  Bp  Bx  B�{B�{B�{B�  B�  B�{B�  B��
B��
B��B�{B�  B�  B�  B�{B�  B��B��B��B��B�  B�  B�  B�  B�  B�  B�  B�{B�{B��B��B�  C 
=C
=C{C
=C{C
  C  C  C  C
=C  C��C��C  C{C
=C��C"  C${C&{C(
=C*  C+��C.  C0  C2
=C4
=C6
=C8  C:  C<  C=��C@  CB
=CD
=CF  CG��CJ  CL
=CM��CO��CR  CT  CU��CW��CY��C[�C]��C`  Cb
=Cd
=Cf  Cg��Cj  Cl
=Cn
=Cp
=Cq��Ct  Cv{Cx
=Cz  C|  C~
=C�  C�  C�C�C���C���C�C�  C�  C�C�  C�  C���C�  C���C�  C���C�C�  C�
=C�C���C�  C���C�
=C�C�  C�C���C�  C�  C�  C�  C�C�  C�C�C�  C�  C�C�  C�C�  C���C�  C�  C�  C�  C�  C���C�  C�  C�  C���C���C���C�  C���C���C���C�  C�  C���C�  C�  C�C�
=C�
=C�
=C�C�C�C�  C�  C�  C�  C���C���C�  C���C�  C�C�  C���C�  C�  C���C���C���C���C�C�C�C�  C�  C���C���C�  C�  C�C�
=C�  C���C���C���C�  C�  C���C���C���C���C���C���C�  C�C�C�C���C�  C�
=C���C���C�  C�
=C�
=C���C�C�D �D � D�D��D�D��D�D� DD�D  D}qD�qD� D�D}qD�D��D	�D	��D	�qD
� D
��D}qD�D� D�D��D  D� D�D��DD��DD��D  D� D  D� D  D��D�D��DD�D�qD}qD  D��D�D� D�qD}qD�qD��D  D}qD  D�D�D� D��Dz�D   D ��D!  D!}qD"  D"� D#  D#� D$�D$��D$�qD%z�D&  D&��D&�qD'}qD(�D(��D)  D)� D*  D*}qD+�D+� D,�D,��D-�D-}qD-�qD.� D/�D/�D0�D0� D1  D1��D2�D2��D3  D3}qD3��D4}qD5�D5��D6�D6� D6�qD7}qD8  D8��D8�qD9� D:  D:}qD;  D;��D<  D<� D=�D=��D>�D>��D?  D?}qD?�qD@�DA  DAz�DA�qDB� DC  DC� DD�DD��DE�DE}qDF�DF�DG  DG}qDG�qDHz�DH�qDI� DJ  DJ� DJ��DK}qDL  DL}qDM�DM� DN  DN� DO  DO��DP  DP� DQ  DQ� DR  DR� DS  DS� DS�qDTz�DU�DU��DVDV� DV�qDW� DX  DX}qDY  DY� DZ  DZ� D[  D[� D\  D\� D\�qD]� D^D^��D_�D_�D`  D`}qDa�Da��Db�Db��Dc�Dc� Dc��Dd}qDd�qDe}qDe�qDf� Dg�Dg}qDh  Dh� Di  Di� Dj�Dj��Dk�Dk�Dl  Dl� Dl�qDmz�Dm�qDn}qDn�qDo��Dp�Dp� Dp�qDqz�Dq�qDr� Ds  Ds}qDs�qDt}qDu  Du��Dv�Dv� Dw  Dw��Dx  Dx}qDx��Dy}qDy�qDz� D{  D{� D|  D|� D}  D}� D~  D~� DD�D�  D�=qD�}qD�� D�HD�B�D���D�D�  D�>�D��HD�D�HD�AHD��HD��HD��D�B�D��HD��qD�  D�@ D�� D��HD���D�@ D���D��HD�  D�B�D�� D�� D�  D�>�D�~�D��qD���D�>�D�� D��HD��D�C�D��HD��HD��D�AHD�� D�� D�HD�@ D��HD��HD�  D�@ D��HD��HD�  D�>�D�~�D��qD���D�>�D�� D�� D���D�>�D�� D���D���D�AHD��HD���D��qD�>�D��HD�� D�HD�@ D�}qD��qD�  D�AHD��HD��HD�HD�AHD�~�D���D�  D�AHD��HD�� D�HD�>�D�~�D�� D�HD�>�D�}qD�� D�HD�@ D�� D�D��D�B�D���D��HD�  D�@ D�~�D���D���D�@ D�� D��HD�HD�>�D�~�D�� D�  D�AHD�� D�� D��D�AHD��HD��HD�HD�@ D�~�D�� D�HD�@ D��HD�� D�  D�@ D�~�D�� D�HD�>�D��HD��HD�  D�AHD�� D���D�  D�@ D��HD�� D�  D�AHD��HD���D�HD�AHD�� D�� D�HD�@ D�� D��HD�  D�@ D�~�D�� D�  D�>�D�~�D��HD��D�AHD�� D��HD���D�@ D�~�D��qD�HD�B�D���D��HD���D�>�D�� D�� D�HD�@ D�~�D�� D�  D�=qD�~�D�� D�  D�>�D�~�D��HD�  D�=qD�~�D���D�  D�>�D�~�D�� D�HD�AHD�� D�� D�  D�AHD�� D���D�  D�@ D�� D�� D�  D�@ D�� D���D���D�@ D�� D�� D���D�@ D�� D���D�  D�>�D�}qD�� D��D�@ D�~�D��qD���D�=qD�|)D���D�  D�@ D�~�D��HD�  D�=qD�}qD�� D�  D�>�D�}qD�� D�HD�@ D�� D��HD�HD�@ D�� D�� D�  D�AHD�D��HD�  D�@ DÀ D��HD��D�AHDĀ D�� D�  D�@ Dŀ D�� D���D�=qDƀ Dƾ�D�  D�>�Dǀ D�� D�HD�B�DȀ D�D��D�@ D�}qDɾ�D�HD�AHDʁHD�� D�  D�AHDˁHD�D�HD�@ D�~�D̾�D�  D�AHD̀ D�� D�  D�>�D�}qD�� D�HD�@ DρHD�� D���D�>�DЀ D��HD�  D�>�Dр D��HD�HD�@ DҀ D��HD�HD�>�D�~�D��HD��D�AHDԁHD��HD�  D�>�D�~�D��HD�  D�AHDցHDֽqD�  D�AHDׁHD�� D�  D�B�D؁HD�� D���D�>�Dق�D�� D�  D�AHDځHD��HD�  D�=qDۀ D�� D���D�AHD܁HD��HD�HD�AHD�~�D�� D�HD�AHDނ�D��HD���D�@ D߁HD߾�D�  D�AHD�� D��HD�  D�>�D�}qD�qD���D�@ D� D��HD�  D�@ D� D�� D��qD�>�D� D��HD�HD�@ D�~�D�� D�  D�@ D� D澸D���D�>�D�HD��HD���D�=qD� D�� D�HD�@ D� D�� D�  D�B�D�HD꾸D�HD�AHD�HD��HD���D�>�D� D�� D�  D�AHD�HD�� D���D�>�D�HD�� D���D�>�D� D�� D�HD�AHD�� D�D�HD�B�D� D�� D�HD�AHD� D�� D���D�@ D� D��HD�HD�@ D�~�D���D�  D�@ D�~�D���D���D�@ D��HD�� D�HD�AHD�~�D�� D�HD�@ D�� D��HD���D�AHD��HG�O�>���?L��?�\)?\?�@
=q@&ff@8Q�@Tz�@n{@�G�@��@�(�@�ff@���@��R@�=q@�33@�\@���@�
=A�\A
=Ap�A�
A�A{A$z�A)��A0��A5�A:�HAAG�AEAL��AS33AW
=A^{Adz�AhQ�An�RAuAz=qA�  A�33A�A�Q�A��A�p�A���A�33A�p�A���A��HA��A��A��\A�(�A�\)A���A��A�ffA���A��HA�A���A��HA�p�A���A�33A��A�Q�A�=qA���A�  Aљ�A�z�A�\)A�G�A�33A�ffA�  A�\A��A�\)A���A�(�A�RA�Q�A�A�A��A��\A��A��RB z�B{B�HB  Bp�B�\B�B��B
{B
�HB�
BG�B{B
=BQ�BG�B�B�HB  B��BG�B=qB33B�BQ�B��Bp�BffB�HB\)BQ�B��B��B=qB�RB�B z�B!�B!��B"�\B#33B#�B$��B%��B&{B'
=B'�
B(z�B)�B*=qB+
=B+�B,��B-��B-�B/
=B0  B0z�B1G�B2ffB333B3�B4��B5p�B6=qB733B8  B8z�B9p�B:�\B:�HB<(�B<��B=p�B>�\B?
=B?�B@��BA��BB{BC33BC�
BDQ�BEG�BF=qBF�\BG\)BHQ�BH��BIp�BJffBK33BK�BLz�BM��BM�BN�RBO�
BPQ�BQ�BR{BR�RBS
=BT(�BT��BU�BV{BV�HBW\)BX(�BX��BYBZ{B[
=B[�
B\  B\��B]�B^ffB_
=B_�
B`��B`��Bb{Bb�HBc
=BdQ�Bd��BeG�Bf�\Bf�HBg�Bh��Bi�Bi��Bj�HBk\)Bk�
Bl��Bmp�Bn{Bo
=Bo�
Bp(�Bq�Br{BrffBs\)Bt  Btz�Bup�Bv=qBv�RBw\)BxQ�By�By��Bz�RB{�B{�
B|��B}�B~ffB�B�=qB�ffB���B�p�B���B�=qB���B��HB��B�  B�=qB���B�33B��B��B�ffB��HB�G�B���B�(�B�z�B���B�G�B��
B�  B��\B�
=B�\)B��B�=qB��RB���B�\)B��B�ffB���B��B��B�(�B��\B��HB��B�  B�=qB��HB�p�B�B�=qB��HB�\)B�B�(�B��HB�G�B��B�=qB���B�33B���B�Q�B���B��B��B�(�B�z�B���B��B�{B�ffB�
=B���B�  B�ffB���B���B�(�B�z�B���B��B�(�B���B�
=B��B�=qB��\B��B�B�=qB�z�B�G�B�B�{B��\B��B��B�  B���B�33B��B�{B���B���B�p�B�(�B�ffB���B���B�  B�ffB��B��B��
B�ffB���B�G�B��
B�ffB���B��B�B�=qB��\B��B��B�  B�ffB���B�p�B�B�Q�B���B�\)B��B�ffB���B�33B�B�Q�B��RB��B��B�=qB\B�
=BîB�{B�ffB�
=B�p�B�B�ffB��HB�33B�B�Q�Bȣ�B�
=Bə�B�{B�z�B���B�\)B��B�(�Ḅ�B�33B�B�{BΏ\B�33Bϙ�B��BЏ\B��B�p�B��B�z�B��HB�G�B��B�ffBԸRB�\)B��
B�=qB֏\B�G�B�B�  B�z�B�33BمB��B�z�B�
=B�G�B�B�ffBܸRB�
=BݮB�(�Bޏ\B��HB߅B��B�=qB���B�\)B�B�=qB�RB�
=B㙚B�(�B�z�B���B�\)B��B�(�B�\B�
=B癚B��B�=qB���B�G�B陚B��B�ffB���B�\)B뙚B�{B�RB�33B�B��B�\B��B�p�B��
B�ffB�
=B�\)B��B�z�B��HB�G�B�B�ffB���B�33B��
B�Q�B���B��B��B�=qB��\B���B���B�{B�Q�B���B�p�B��B�(�B���B�G�B���B��B�z�B�
=B�\)B�C {C ffC ��C ��C  CG�C�\C��C��C(�Cz�CC�HC(�Cz�C��C�
C(�CffC�\C��C{C\)C�C�C��C=qCp�C��C�
C�CffC��C��C  CG�C��CC�C	(�C	p�C	�RC	�HC

=C
Q�C
��C
�
C  C=qC�\CC�C�CffC�C�C{CG�C�C��C{C=qCffC�C��C=qCffC��C�HC33CQ�C�\C�
C�C\)C�\CC  CQ�C�\CC��CG�C�C�C�C=qCp�C��C�C33CQ�C�\C�
C�CG�Cz�CC
=C33CffC�RC�HC{C\)C�C��C  CG�C�\C�RC�C33Cz�C��C�
C(�CffC��C�
C(�Cp�C��C��C�Cp�C�C�
C�Cz�C�RC�HC33C�C�C�C G�C �C �C!
=C!Q�C!�C!�C"  C"G�C"�C"�C"��C#=qC#�C#�C#�HC$=qC$z�C$�C$�HC%�C%p�C%�C%�HC&{C&ffC&�C&�
C'�C'ffC'��C'�
C(
=C(\)C(�C(�HC){C)Q�C)��C)�HC*{C*Q�C*��C*�C+�C+\)C+�C,  C,(�C,\)C,�C-  C-=qC-p�C-C.
=C.33C.�C.�
C/
=C/=qC/�\C/�HC0
=C0Q�C0��C0�
C1{C1p�C1�C1�HC2(�C2p�C2C2�C333C3z�C3C3��C433C4�C4��C4��C5G�C5�\C5��C5��C6=qC6��C6�
C7  C7Q�C7��C7�C8{C8Q�C8�C8��C9�C9ffC9�RC9�C:�C:ffC:�RC:��C;(�C;z�C;��C<  C<=qC<�\C<�
C=
=C=G�C=�\C=�HC>(�C>Q�C>�\C>�HC?33C?ffC?��C?�C@=qC@p�C@�CA  CAG�CAp�CACB�CBQ�CB�CB�
CC(�CCffCC��CC�HCD(�CDz�CD�CD�HCE33CEz�CECE�CF(�CFp�CF�RCF��CG(�CGffCG�RCH
=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                    @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�A�1A�A��A�-A�1'A�1'A�1'A�/A�/A�-A�5?A�7LA�33A�-A�+A�&�A�+A�(�A� �A��A� �A�-A�/A�-A�33A�7LA�;dA�=qA�?}A�=qA�?}A�=qA�=qA�?}A�?}A�A�A�C�A�C�A�E�A�C�A�G�A�K�A�K�A�K�A�M�A�M�A�O�A�M�A�Q�A�Q�A�S�A�S�A�XA�XA�ZA�ZA�VA�XA�S�A�S�A�S�A�A�A��AΑhA�ĜA�
=A�ȴA�{A�v�A�ffA�t�A�+A�ZA��mA�bA��A��+A��A�\)A��yA���A�=qA�z�A�/A��A���A�%A�/A�XA��`A���A���A�dZA�7LA�/A�l�A��PA�9XA�-A��mA��;A�p�A�t�A�A�A�$�A�$�A���A�I�A�Q�A��9A�^5A��/A���A��A�A�A��TA�5?A���A�\)A�p�A��A�K�A�
=A��A�t�A�(�AhsA~  Ax��Au��Arr�Aol�Ak7LAi�hAf�Ac+A_VAZ��AXJAU�AS%AR��AR~�ARVAR�AQ��AQO�AP�+AN�AIAF��ADn�AC
=A?��A;�A6~�A5;dA4�A2v�A1\)A0�A0z�A0A/&�A,��A)dZA&VA$r�A#hsA"=qA!�A!��A!�A!K�A!%A �A��A33A�jA��A�AbAƨAz�A�A�Az�Ar�A�\A�hAE�A�
A5?A�AbAA�A�mA��A�FAVA(�A�7A
{A�!A��AK�A�jA��A��A�A�A��AhsAO�A�A��AbAK�A9XA�;A ��@��@��D@��y@��#@���@���@��@�A�@��P@��@���@�M�@�7L@�V@�Q�@��@��@�b@�;d@��@�w@��@�@�/@�z�@�  @㝲@�+@�ȴ@���@���@�^5@��T@ݡ�@�G�@��/@���@۝�@�S�@�v�@��@�&�@���@�r�@�o@���@�Q�@�o@�J@��@�Q�@υ@���@��@���@�-@�`B@�/@�r�@ˮ@�C�@��@ɲ-@�&�@ȼj@�bN@�b@�l�@�=q@ř�@ēu@�t�@�33@�o@�@�-@��#@��^@�x�@���@�Z@�ƨ@�;d@�@���@��!@�~�@��T@���@��@�9X@�  @��;@���@��R@��R@���@�~�@�E�@�$�@��#@���@�`B@�G�@��@�1'@�  @��@��
@��;@�ƨ@��@��@���@��P@��P@�dZ@�+@��R@�-@���@���@���@�1'@��;@���@�l�@���@�{@��-@��@�/@�&�@�V@���@���@���@��@�1'@��;@��w@���@�t�@�\)@�
=@���@�$�@��@���@�@��^@���@�x�@�G�@�&�@���@�A�@� �@� �@��@���@���@���@��P@�"�@��y@��!@���@�E�@��@���@���@���@�p�@�X@�?}@�&�@�V@���@��`@��/@���@���@�r�@��F@�K�@��@���@���@���@�7L@��/@���@���@��D@�Q�@��
@�S�@�"�@���@���@�M�@�J@���@�`B@��/@�1@���@���@���@���@���@���@��P@��@�33@��@��\@�~�@�~�@�^5@��@���@�hs@�O�@��@���@�I�@��@���@��;@���@��w@��@���@��@�ȴ@���@��+@�v�@�?}@���@�Ĝ@��9@��9@���@��@�j@� �@���@��;@��F@��@�o@��R@�=q@��@���@���@��@�X@��`@��j@�Z@�1@�  @��m@��;@���@���@���@�K�@�ȴ@���@�v�@�ff@�ff@�^5@�V@�=q@�=q@�-@�$�@�J@���@�@��^@��@�`B@�/@���@��w@���@�n�@�n�@�n�@�ff@�5?@�J@���@���@�hs@��@��D@�b@��w@�S�@���@�~�@�E�@�$�@�J@���@��@���@��7@��@�hs@�?}@���@��@��u@�z�@�bN@�Q�@�9X@��@��@���@�
=@�{@���@�p�@�`B@�?}@�&�@���@���@��9@��u@�Z@� �@��m@���@�+@��R@�n�@�V@�5?@�-@�J@���@�O�@��@��/@���@�A�@�@|�@+@~ȴ@}�@|�/@{ƨ@z�!@z�!@z��@z�\@zJ@y��@y��@y&�@y�@y�@y&�@y&�@y&�@y�@y%@x��@x�`@x��@xĜ@x��@x��@x�u@xr�@xA�@x  @w|�@v�+@v5?@vV@v��@w
=@u�T@t�@tz�@tZ@t9X@t9X@t�@s��@sƨ@s�@s"�@r��@q��@q%@p�@p �@o|�@n$�@mO�@l��@lj@kdZ@j�!@jn�@j=q@j�@i��@i�#@i�^@i��@i��@i��@i�7@i�7@iG�@h��@h�@hA�@h1'@h �@g��@f��@f$�@e�@c"�@bn�@b-@a�@a�7@a7L@a%@`��@`�`@`�9@`Q�@`b@`  @_�@_�w@_��@_l�@_K�@_
=@^�+@]�@\j@\I�@[�m@[33@Z��@ZM�@Z=q@Z=q@Z=q@Z-@Z�@Z�@ZJ@Y�@Yhs@Y7L@X��@X��@Xr�@X �@W�;@W�@W\)@V�@V�+@Vff@Vff@Vff@V{@V{@U�T@U��@U@U�-@Up�@T��@SS�@R�\@Q�#@Q7L@PĜ@O\)@N�R@N�+@Nff@NV@N$�@M��@M�T@Lz�@KS�@J�@J��@J��@J��@J��@J�!@Jn�@JJ@I��@IX@I�@H�9@HbN@Hb@F��@E@E�@EO�@E/@E�@D��@D�@D9X@C�@B��@BM�@B-@B�@A��@A�@A�#@A�^@A��@A��@A��@Ahs@@��@?��@?l�@?;d@?�@?
=@?
=@>�y@>�@>�@>�R@>�R@>v�@>E�@>{@=�h@=�@<��@<�D@<z�@<z�@<j@<(�@<(�@;t�@;@:�H@:��@:��@:�\@:^5@:M�@:-@9�@9��@9&�@8�`@8��@8r�@8A�@8A�@8b@7�@7
=@6�R@65?@5�h@5`B@5O�@4�@4j@3��@3@2n�@2-@1�^@0�`@0��@0��@0��@0�u@0r�@0r�@0A�@0b@0  @/�P@/�@.�y@.V@.E�@.5?@.$�@-�@-��@-�-@-�h@-p�@-O�@-V@,��@,�@,�@,j@,I�@+��@+C�@+33@+o@*�@*��@*��@*=q@)��@( �@'�@'��@'�@'�@'��@'��@'��@'|�@'+@'�@'�@'�@'�@'�@'
=@&�R@&{@%��@%@%��@%p�@$��@$�j@$�D@#��@"~�@!��@!hs@ �9@ b@|�@K�@�@ȴ@��@V@$�@�@��@@�-@�-@p�@`B@O�@`B@?}@�D@��@�@�^@��@��@��@��@��@�7@x�@G�@7L@��@�9@�u@bN@1'@�@�w@�P@l�@;d@+@+@+@+@+@+@��@��@V@{@�T@��@��@�h@�@�@p�@`B@O�@/@�@V@�@��@�j@�@�D@1@�F@�@dZ@33@o@��@��@��@��@��@��@�\@�\@�\@~�@n�@n�@^5@M�@M�@-@J@�#@�^@��@�@��@�9@�9@�9@��@�u@bN@Q�@Q�@Q�@A�@ �@��@��@��@l�@l�A��A�
=A���A�A�
=A�A�%A�A�  A�bA�(�A�$�A�/A�1'A�(�A�33A�33A�/A�33A�1'A�33A�33A�-A�1'A�33A�-A�1'A�/A�/A�1'A�-A�5?A�9XA�7LA�9XA�;dA�5?A�9XA�;dA�5?A�7LA�9XA�/A�1'A�5?A�33A�5?A�-A�(�A�-A�+A�(�A�-A�+A�+A�-A�-A�&�A�-A�+A� �A�(�A�(�A�(�A�&�A�-A�&�A�(�A�1'A�-A�+A�/A�$�A�/A�-A�$�A�&�A�&�A� �A� �A�(�A� �A��A� �A��A��A��A�"�A��A�"�A� �A� �A�"�A� �A��A�$�A�$�A� �A��A��A�/A�+A�33A�33A�(�A�-A�33A�-A�-A�/A�1'A�-A�-A�/A�-A�(�A�/A�/A�+A�/A�/A�-A�33A�-A�-A�5?A�33A�7LA�5?A�33A�5?A�7LA�33A�1'A�7LA�7LA�1'A�5?A�5?A�33A�7LA�9XA�7LA�7LA�9XA�;dA�7LA�;dA�;dA�7LA�;dA�;dA�9XA�9XA�=qA�;dA�9XA�?}A�;dA�;dA�?}A�;dA�9XA�=qA�?}A�9XA�?}A�=qA�=qA�A�A�=qA�=qA�A�A�?}A�=qA�A�A�?}A�;dA�=qA�A�A�;dA�=qA�?}A�=qA�;dA�=qA�?}A�;dA�;dA�?}A�?}A�A�A�E�A�?}A�;dA�?}A�=qA�9XA�=qA�=qA�9XA�;dA�=qA�9XA�=qA�A�A�C�A�?}A�A�A�?}A�;dA�?}A�A�A�=qA�;dA�?}A�?}A�;dA�A�A�?}A�;dA�A�A�?}A�=qA�A�A�=qA�A�A�G�A�C�A�?}A�A�A�?}A�;dA�C�A�?}A�=qA�A�A�C�A�=qA�?}A�A�A�=qA�A�A�A�A�=qA�A�A�C�A�?}A�=qA�C�A�C�A�?}A�C�A�E�A�A�A�C�A�G�A�A�A�C�A�C�A�?}A�E�A�E�A�A�A�G�A�E�A�A�A�A�A�E�A�A�A�E�A�C�A�A�A�;dA�E�A�I�A�C�A�C�A�G�A�E�A�C�A�E�A�G�A�A�A�C�A�G�A�C�A�A�A�E�A�G�A�A�A�A�A�E�A�G�A�E�A�C�A�I�A�K�A�E�A�G�A�M�A�K�A�G�A�K�A�M�A�G�A�I�A�M�A�M�A�I�A�I�A�M�A�K�A�G�A�K�A�M�A�K�A�I�A�O�A�K�A�I�A�M�A�M�A�I�A�I�A�O�A�M�A�I�A�M�A�O�A�M�A�K�A�M�A�Q�A�O�A�K�A�I�A�M�A�O�A�O�A�K�A�M�A�Q�A�M�A�M�A�O�A�O�A�K�A�Q�A�O�A�M�A�M�A�Q�A�Q�A�M�A�K�A�Q�A�K�A�O�A�Q�A�O�A�Q�A�S�A�O�A�Q�A�S�A�Q�A�O�A�S�A�S�A�O�A�Q�A�S�A�O�A�Q�A�VA�Q�A�O�A�S�A�S�A�O�A�S�A�VA�Q�A�Q�A�VA�S�A�O�A�Q�A�VA�S�A�S�A�ZA�XA�S�A�VA�ZA�XA�S�A�XA�ZA�VA�VA�ZA�XA�VA�ZA�XA�VA�ZA�ZA�VA�XA�\)A�XA�XA�ZA�\)A�ZA�VA�\)A�\)A�XA�XA�\)A�\)A�VA�VA�XA�VA�S�A�XA�\)A�VA�XA�^5A�\)A�VA�ZA�XA�Q�A�XA�XA�VA�S�A�XA�XA�Q�A�Q�A�XA�S�A�Q�A�S�A�VA�Q�A�Q�A�XA�S�A�Q�A�VA�XA�S�A�Q�A�VA�S�A�Q�A�VA�VA�M�A�M�A�M�A�E�A�E�A�E�A�?}A�=qA�?}A��A��A�VA�%A�
=A���A��yA��TA���Aϲ-AϬAυA�l�A�M�A��yA�~�A�M�A��A�`BA�5?A�oA���A�v�A��TA�1'A�ȴAʧ�Aʡ�Aʙ�A�t�A�%A��#A���A�ȴA�ƨA���A��;A��HA��/A���AɸRA�ĜA�ȴAɣ�A�I�A��;AȑhA�O�A� �A��A�ĜA�~�A�VA�5?A�VA��`AƮA�v�A�I�A�=qA��A��A���A�ƨAź^Aŧ�Aŉ7A�`BA�1'A��A��/A�ƨA���A�$�A�1A�-A�M�A�K�A�O�A�S�A�ZA�VA�M�A�C�A�(�A�oA��yA��HA§�A�dZA��A�ȴA�7LA���A��DA�p�A�hsA�M�A�$�A�-A�%A��mA���A���A�bNA�5?A�"�A� �A��A�VA�1A�A�  A���A��A��A��A��FA��uA�\)A�9XA�JA���A���A�bNA�bA�7LA�&�A�bA��#A���A���A��DA�t�A�33A�A�%A��`A���A��\A��7A�~�A�l�A�VA�VA�G�A�1'A�$�A��A�A��A��;A��#A���A���A���A���A���A�ƨA��wA���A���A���A�r�A�E�A���A��A�|�A�?}A�bA�VA�1A���A��A��;A��9A�G�A��FA�?}A�ȴA��7A�ZA�G�A�=qA�7LA�/A�/A�1'A��A�JA�A���A��A��A��yA��#A���A���A���A�A��FA���A��uA��A�x�A�p�A�^5A�O�A�;dA��A��A��A�ĜA���A��A�dZA�VA�A�A�(�A��A�A��HA�ƨA��-A��PA�x�A�bNA�9XA���A��HA��A���A�ZA�A�ĜA��+A�$�A��RA�p�A� �A���A��mA��
A�ƨA���A��9A���A�ZA���A��;A��jA���A�z�A�A�A�VA��`A�ƨA�p�A��A�ƨA�XA�%A���A�l�A�7LA��A���A���A���A�z�A�^5A�K�A�1'A��A�1A���A��HA��
A��FA�~�A�G�A�A�A�?}A��A�
=A��`A��A��uA�\)A�S�A�K�A�O�A�G�A�E�A�C�A�=qA�9XA�7LA�-A�
=A��A��A��A�/A��A���A���A�v�A�VA�G�A�5?A���A��RA�33A��A�|�A�1'A���A�A���A��hA���A�hsA�5?A�oA��A���A��A��A���A��PA��7A��A�z�A�v�A�l�A�`BA�A�A��A�VA��mA�ĜA�&�A�r�A��A��A�A���A�p�A�-G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                    A�A�1A�A��A�-A�1'A�1'A�1'A�/A�/A�-A�5?A�7LA�33A�-A�+A�&�A�+A�(�A� �A��A� �A�-A�/A�-A�33A�7LA�;dA�=qA�?}A�=qA�?}A�=qA�=qA�?}A�?}A�A�A�C�A�C�A�E�A�C�A�G�A�K�A�K�A�K�A�M�A�M�A�O�A�M�A�Q�A�Q�A�S�A�S�A�XA�XA�ZA�ZA�VA�XA�S�A�S�A�S�A�A�A��AΑhA�ĜA�
=A�ȴA�{A�v�A�ffA�t�A�+A�ZA��mA�bA��A��+A��A�\)A��yA���A�=qA�z�A�/A��A���A�%A�/A�XA��`A���A���A�dZA�7LA�/A�l�A��PA�9XA�-A��mA��;A�p�A�t�A�A�A�$�A�$�A���A�I�A�Q�A��9A�^5A��/A���A��A�A�A��TA�5?A���A�\)A�p�A��A�K�A�
=A��A�t�A�(�AhsA~  Ax��Au��Arr�Aol�Ak7LAi�hAf�Ac+A_VAZ��AXJAU�AS%AR��AR~�ARVAR�AQ��AQO�AP�+AN�AIAF��ADn�AC
=A?��A;�A6~�A5;dA4�A2v�A1\)A0�A0z�A0A/&�A,��A)dZA&VA$r�A#hsA"=qA!�A!��A!�A!K�A!%A �A��A33A�jA��A�AbAƨAz�A�A�Az�Ar�A�\A�hAE�A�
A5?A�AbAA�A�mA��A�FAVA(�A�7A
{A�!A��AK�A�jA��A��A�A�A��AhsAO�A�A��AbAK�A9XA�;A ��@��@��D@��y@��#@���@���@��@�A�@��P@��@���@�M�@�7L@�V@�Q�@��@��@�b@�;d@��@�w@��@�@�/@�z�@�  @㝲@�+@�ȴ@���@���@�^5@��T@ݡ�@�G�@��/@���@۝�@�S�@�v�@��@�&�@���@�r�@�o@���@�Q�@�o@�J@��@�Q�@υ@���@��@���@�-@�`B@�/@�r�@ˮ@�C�@��@ɲ-@�&�@ȼj@�bN@�b@�l�@�=q@ř�@ēu@�t�@�33@�o@�@�-@��#@��^@�x�@���@�Z@�ƨ@�;d@�@���@��!@�~�@��T@���@��@�9X@�  @��;@���@��R@��R@���@�~�@�E�@�$�@��#@���@�`B@�G�@��@�1'@�  @��@��
@��;@�ƨ@��@��@���@��P@��P@�dZ@�+@��R@�-@���@���@���@�1'@��;@���@�l�@���@�{@��-@��@�/@�&�@�V@���@���@���@��@�1'@��;@��w@���@�t�@�\)@�
=@���@�$�@��@���@�@��^@���@�x�@�G�@�&�@���@�A�@� �@� �@��@���@���@���@��P@�"�@��y@��!@���@�E�@��@���@���@���@�p�@�X@�?}@�&�@�V@���@��`@��/@���@���@�r�@��F@�K�@��@���@���@���@�7L@��/@���@���@��D@�Q�@��
@�S�@�"�@���@���@�M�@�J@���@�`B@��/@�1@���@���@���@���@���@���@��P@��@�33@��@��\@�~�@�~�@�^5@��@���@�hs@�O�@��@���@�I�@��@���@��;@���@��w@��@���@��@�ȴ@���@��+@�v�@�?}@���@�Ĝ@��9@��9@���@��@�j@� �@���@��;@��F@��@�o@��R@�=q@��@���@���@��@�X@��`@��j@�Z@�1@�  @��m@��;@���@���@���@�K�@�ȴ@���@�v�@�ff@�ff@�^5@�V@�=q@�=q@�-@�$�@�J@���@�@��^@��@�`B@�/@���@��w@���@�n�@�n�@�n�@�ff@�5?@�J@���@���@�hs@��@��D@�b@��w@�S�@���@�~�@�E�@�$�@�J@���@��@���@��7@��@�hs@�?}@���@��@��u@�z�@�bN@�Q�@�9X@��@��@���@�
=@�{@���@�p�@�`B@�?}@�&�@���@���@��9@��u@�Z@� �@��m@���@�+@��R@�n�@�V@�5?@�-@�J@���@�O�@��@��/@���@�A�@�@|�@+@~ȴ@}�@|�/@{ƨ@z�!@z�!@z��@z�\@zJ@y��@y��@y&�@y�@y�@y&�@y&�@y&�@y�@y%@x��@x�`@x��@xĜ@x��@x��@x�u@xr�@xA�@x  @w|�@v�+@v5?@vV@v��@w
=@u�T@t�@tz�@tZ@t9X@t9X@t�@s��@sƨ@s�@s"�@r��@q��@q%@p�@p �@o|�@n$�@mO�@l��@lj@kdZ@j�!@jn�@j=q@j�@i��@i�#@i�^@i��@i��@i��@i�7@i�7@iG�@h��@h�@hA�@h1'@h �@g��@f��@f$�@e�@c"�@bn�@b-@a�@a�7@a7L@a%@`��@`�`@`�9@`Q�@`b@`  @_�@_�w@_��@_l�@_K�@_
=@^�+@]�@\j@\I�@[�m@[33@Z��@ZM�@Z=q@Z=q@Z=q@Z-@Z�@Z�@ZJ@Y�@Yhs@Y7L@X��@X��@Xr�@X �@W�;@W�@W\)@V�@V�+@Vff@Vff@Vff@V{@V{@U�T@U��@U@U�-@Up�@T��@SS�@R�\@Q�#@Q7L@PĜ@O\)@N�R@N�+@Nff@NV@N$�@M��@M�T@Lz�@KS�@J�@J��@J��@J��@J��@J�!@Jn�@JJ@I��@IX@I�@H�9@HbN@Hb@F��@E@E�@EO�@E/@E�@D��@D�@D9X@C�@B��@BM�@B-@B�@A��@A�@A�#@A�^@A��@A��@A��@Ahs@@��@?��@?l�@?;d@?�@?
=@?
=@>�y@>�@>�@>�R@>�R@>v�@>E�@>{@=�h@=�@<��@<�D@<z�@<z�@<j@<(�@<(�@;t�@;@:�H@:��@:��@:�\@:^5@:M�@:-@9�@9��@9&�@8�`@8��@8r�@8A�@8A�@8b@7�@7
=@6�R@65?@5�h@5`B@5O�@4�@4j@3��@3@2n�@2-@1�^@0�`@0��@0��@0��@0�u@0r�@0r�@0A�@0b@0  @/�P@/�@.�y@.V@.E�@.5?@.$�@-�@-��@-�-@-�h@-p�@-O�@-V@,��@,�@,�@,j@,I�@+��@+C�@+33@+o@*�@*��@*��@*=q@)��@( �@'�@'��@'�@'�@'��@'��@'��@'|�@'+@'�@'�@'�@'�@'�@'
=@&�R@&{@%��@%@%��@%p�@$��@$�j@$�D@#��@"~�@!��@!hs@ �9@ b@|�@K�@�@ȴ@��@V@$�@�@��@@�-@�-@p�@`B@O�@`B@?}@�D@��@�@�^@��@��@��@��@��@�7@x�@G�@7L@��@�9@�u@bN@1'@�@�w@�P@l�@;d@+@+@+@+@+@+@��@��@V@{@�T@��@��@�h@�@�@p�@`B@O�@/@�@V@�@��@�j@�@�D@1@�F@�@dZ@33@o@��@��@��@��@��@��@�\@�\@�\@~�@n�@n�@^5@M�@M�@-@J@�#@�^@��@�@��@�9@�9@�9@��@�u@bN@Q�@Q�@Q�@A�@ �@��@��@��@l�G�O�A��A�
=A���A�A�
=A�A�%A�A�  A�bA�(�A�$�A�/A�1'A�(�A�33A�33A�/A�33A�1'A�33A�33A�-A�1'A�33A�-A�1'A�/A�/A�1'A�-A�5?A�9XA�7LA�9XA�;dA�5?A�9XA�;dA�5?A�7LA�9XA�/A�1'A�5?A�33A�5?A�-A�(�A�-A�+A�(�A�-A�+A�+A�-A�-A�&�A�-A�+A� �A�(�A�(�A�(�A�&�A�-A�&�A�(�A�1'A�-A�+A�/A�$�A�/A�-A�$�A�&�A�&�A� �A� �A�(�A� �A��A� �A��A��A��A�"�A��A�"�A� �A� �A�"�A� �A��A�$�A�$�A� �A��A��A�/A�+A�33A�33A�(�A�-A�33A�-A�-A�/A�1'A�-A�-A�/A�-A�(�A�/A�/A�+A�/A�/A�-A�33A�-A�-A�5?A�33A�7LA�5?A�33A�5?A�7LA�33A�1'A�7LA�7LA�1'A�5?A�5?A�33A�7LA�9XA�7LA�7LA�9XA�;dA�7LA�;dA�;dA�7LA�;dA�;dA�9XA�9XA�=qA�;dA�9XA�?}A�;dA�;dA�?}A�;dA�9XA�=qA�?}A�9XA�?}A�=qA�=qA�A�A�=qA�=qA�A�A�?}A�=qA�A�A�?}A�;dA�=qA�A�A�;dA�=qA�?}A�=qA�;dA�=qA�?}A�;dA�;dA�?}A�?}A�A�A�E�A�?}A�;dA�?}A�=qA�9XA�=qA�=qA�9XA�;dA�=qA�9XA�=qA�A�A�C�A�?}A�A�A�?}A�;dA�?}A�A�A�=qA�;dA�?}A�?}A�;dA�A�A�?}A�;dA�A�A�?}A�=qA�A�A�=qA�A�A�G�A�C�A�?}A�A�A�?}A�;dA�C�A�?}A�=qA�A�A�C�A�=qA�?}A�A�A�=qA�A�A�A�A�=qA�A�A�C�A�?}A�=qA�C�A�C�A�?}A�C�A�E�A�A�A�C�A�G�A�A�A�C�A�C�A�?}A�E�A�E�A�A�A�G�A�E�A�A�A�A�A�E�A�A�A�E�A�C�A�A�A�;dA�E�A�I�A�C�A�C�A�G�A�E�A�C�A�E�A�G�A�A�A�C�A�G�A�C�A�A�A�E�A�G�A�A�A�A�A�E�A�G�A�E�A�C�A�I�A�K�A�E�A�G�A�M�A�K�A�G�A�K�A�M�A�G�A�I�A�M�A�M�A�I�A�I�A�M�A�K�A�G�A�K�A�M�A�K�A�I�A�O�A�K�A�I�A�M�A�M�A�I�A�I�A�O�A�M�A�I�A�M�A�O�A�M�A�K�A�M�A�Q�A�O�A�K�A�I�A�M�A�O�A�O�A�K�A�M�A�Q�A�M�A�M�A�O�A�O�A�K�A�Q�A�O�A�M�A�M�A�Q�A�Q�A�M�A�K�A�Q�A�K�A�O�A�Q�A�O�A�Q�A�S�A�O�A�Q�A�S�A�Q�A�O�A�S�A�S�A�O�A�Q�A�S�A�O�A�Q�A�VA�Q�A�O�A�S�A�S�A�O�A�S�A�VA�Q�A�Q�A�VA�S�A�O�A�Q�A�VA�S�A�S�A�ZA�XA�S�A�VA�ZA�XA�S�A�XA�ZA�VA�VA�ZA�XA�VA�ZA�XA�VA�ZA�ZA�VA�XA�\)A�XA�XA�ZA�\)A�ZA�VA�\)A�\)A�XA�XA�\)A�\)A�VA�VA�XA�VA�S�A�XA�\)A�VA�XA�^5A�\)A�VA�ZA�XA�Q�A�XA�XA�VA�S�A�XA�XA�Q�A�Q�A�XA�S�A�Q�A�S�A�VA�Q�A�Q�A�XA�S�A�Q�A�VA�XA�S�A�Q�A�VA�S�A�Q�A�VA�VA�M�A�M�A�M�A�E�A�E�A�E�A�?}A�=qA�?}A��A��A�VA�%A�
=A���A��yA��TA���Aϲ-AϬAυA�l�A�M�A��yA�~�A�M�A��A�`BA�5?A�oA���A�v�A��TA�1'A�ȴAʧ�Aʡ�Aʙ�A�t�A�%A��#A���A�ȴA�ƨA���A��;A��HA��/A���AɸRA�ĜA�ȴAɣ�A�I�A��;AȑhA�O�A� �A��A�ĜA�~�A�VA�5?A�VA��`AƮA�v�A�I�A�=qA��A��A���A�ƨAź^Aŧ�Aŉ7A�`BA�1'A��A��/A�ƨA���A�$�A�1A�-A�M�A�K�A�O�A�S�A�ZA�VA�M�A�C�A�(�A�oA��yA��HA§�A�dZA��A�ȴA�7LA���A��DA�p�A�hsA�M�A�$�A�-A�%A��mA���A���A�bNA�5?A�"�A� �A��A�VA�1A�A�  A���A��A��A��A��FA��uA�\)A�9XA�JA���A���A�bNA�bA�7LA�&�A�bA��#A���A���A��DA�t�A�33A�A�%A��`A���A��\A��7A�~�A�l�A�VA�VA�G�A�1'A�$�A��A�A��A��;A��#A���A���A���A���A���A�ƨA��wA���A���A���A�r�A�E�A���A��A�|�A�?}A�bA�VA�1A���A��A��;A��9A�G�A��FA�?}A�ȴA��7A�ZA�G�A�=qA�7LA�/A�/A�1'A��A�JA�A���A��A��A��yA��#A���A���A���A�A��FA���A��uA��A�x�A�p�A�^5A�O�A�;dA��A��A��A�ĜA���A��A�dZA�VA�A�A�(�A��A�A��HA�ƨA��-A��PA�x�A�bNA�9XA���A��HA��A���A�ZA�A�ĜA��+A�$�A��RA�p�A� �A���A��mA��
A�ƨA���A��9A���A�ZA���A��;A��jA���A�z�A�A�A�VA��`A�ƨA�p�A��A�ƨA�XA�%A���A�l�A�7LA��A���A���A���A�z�A�^5A�K�A�1'A��A�1A���A��HA��
A��FA�~�A�G�A�A�A�?}A��A�
=A��`A��A��uA�\)A�S�A�K�A�O�A�G�A�E�A�C�A�=qA�9XA�7LA�-A�
=A��A��A��A�/A��A���A���A�v�A�VA�G�A�5?A���A��RA�33A��A�|�A�1'A���A�A���A��hA���A�hsA�5?A�oA��A���A��A��A���A��PA��7A��A�z�A�v�A�l�A�`BA�A�A��A�VA��mA�ĜA�&�A�r�A��A��A�A���A�p�A�-G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                    ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B_B�B�BYB�B�B+B�B�B�BYB�B�B�B�B�B�B�B_B+B+B�B%BYB�B�B�B�B%B�B�BYBYB%B%B%B�B�B�B�BSB�B�BSB�B�BSB�B�B�B�B�BSBSBB�B�BSB�BSB�B�B�B�B?�Bl�B��B�B�B��B�BB�B �B�.B�B��B��B�8B�B�HB�.B<jBP}BR�BS[BT�BQ�BOvBiDBEBGBXEBI�B:^B7B<jB6zB33B(XB�BbB�rB�B��B�zB��B�xBgB]�BV9BOBE�B4�B+kBB
��B
��B
��B
��B
��B
v�B
v�B
r|B
n/B
qAB
iDB
c�B
R B
:�B
1�B
"�B
oB	�AB	��B	�RB	��B	�B	��B	�B	��B	{JB	z�B	y�B	x�B	v`B	t�B	o�B	m)B	`�B	U�B	I�B	H�B	D�B	C-B	7LB	1�B	/�B	1'B	+kB	)�B	(XB	&LB	%�B	)�B	+B	(XB	)�B	$�B	(XB	&B	$�B	$@B	$B	"4B	�B	#nB	%FB	$tB	)�B	($B	)*B	)*B	$�B	 'B	�B	�B	1B	+B	xB	�B	
rB	�B	%B	�B	�B	�B	�B	"4B	%FB	 �B	"hB	)�B	0!B	5�B	:�B	;�B	:�B	:�B	<�B	?}B	?HB	?B	>�B	>BB	?}B	>B	<�B	;0B	9XB	>BB	=<B	5�B	+�B	-�B	*0B	+B	+�B	,�B	*�B	)�B	*0B	+6B	.�B	3hB	6�B	=B	=�B	;dB	?HB	J�B	M6B	QB	C�B	D3B	C-B	C-B	DgB	C�B	EmB	I�B	OBB	P}B	O�B	P�B	QB	RTB	S�B	T,B	T�B	XB	YB	[�B	[�B	\�B	aHB	dZB	hsB	jB	l�B	p�B	r|B	v+B	x8B	xB	xlB	z�B	~�B	�B	�%B	��B	�~B	�.B	�B	��B	�1B	�B	�!B	��B	�CB	��B	��B	�0B	��B	�B	��B	�UB	�[B	B	ÖB	�mB	ɺB	�HB	��B	�EB	�B	��B	�B	��B	ޞB	�HB	� B	��B	�&B	��B	�iB	�ZB	��B	�`B	�B	�rB	��B	��B	�B	�PB	�]B	��B
  B
 �B
�B
�B
B
AB
AB
AB
�B
�B
B
�B
GB
{B
B
�B
1B

rB
�B
JB
B
�B
VB
�B
B
{B
�B
$B
�B
_B
7B
OB
$�B
%FB
%�B
&LB
'RB
'�B
(�B
+B
,=B
,�B
,�B
,�B
-B
-B
-�B
-�B
.B
.}B
/�B
/OB
/B
/B
/�B
/OB
/B
/�B
/�B
0UB
/�B
0!B
1'B
2-B
1�B
1�B
2�B
2�B
2�B
2�B
2�B
33B
33B
3hB
3hB
3hB
3�B
4B
6B
7B
7LB
7�B
8�B
:*B
:�B
;�B
;0B
;0B
<B
;�B
<�B
=<B
=�B
=�B
=�B
>B
>�B
?}B
?}B
@�B
B'B
B[B
B[B
B[B
B'B
B'B
B[B
B[B
B'B
C-B
C�B
DgB
DgB
D3B
DgB
D�B
E�B
E�B
E�B
F?B
GzB
G�B
G�B
G�B
HB
HKB
HKB
HKB
H�B
IRB
I�B
I�B
I�B
IRB
NB
M�B
M�B
NB
M�B
N<B
N<B
N�B
O�B
OvB
OvB
PB
O�B
Q�B
Q�B
QNB
QNB
Q�B
Q�B
Q�B
R B
S[B
S&B
T�B
T�B
T�B
UgB
UgB
UgB
VB
U�B
V�B
W�B
W�B
W�B
XB
XB
XB
XB
XB
XB
XB
XB
XyB
X�B
XyB
XyB
X�B
X�B
YB
YKB
\�B
]�B
]�B
]�B
]�B
]�B
]�B
^B
^jB
^5B
^�B
_B
_�B
`B
`BB
aHB
a|B
bB
b�B
b�B
b�B
b�B
bNB
c B
cTB
c B
c�B
c�B
d�B
d�B
d�B
e,B
e`B
d�B
e,B
e,B
d�B
e,B
e�B
gB
g�B
g�B
g�B
h
B
h>B
iB
iB
iyB
i�B
jB
jB
kQB
kB
lWB
l�B
m)B
l�B
l�B
l�B
l�B
m�B
n/B
n�B
n/B
n�B
o5B
o�B
o�B
o�B
o�B
p;B
qB
r|B
r|B
rGB
r|B
rGB
sB
r�B
sB
sMB
sB
sB
r�B
sB
r�B
r�B
r�B
sB
sB
sB
sB
sB
sB
sMB
sMB
sMB
sMB
s�B
t�B
t�B
t�B
s�B
s�B
t�B
u%B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
uZB
wfB
w�B
wfB
wfB
x8B
y�B
zB
zB
z�B
|B
{�B
|PB
|PB
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|PB
|B
|B
|PB
|�B
|�B
|�B
|�B
}�B
~(B
~]B
~]B
�uB
�B
�B
�uB
��B
��B
��B
�B
�GB
�GB
�{B
��B
��B
��B
�B
�MB
�MB
�B
��B
��B
�_B
��B
�%B
��B
��B
�_B
��B
�_B
��B
�_B
��B
��B
��B
��B
��B
�fB
�fB
��B
��B
�7B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�DB
��B
��B
�B
��B
��B
��B
�B
�PB
��B
��B
��B
��B
�\B
��B
�(B
�\B
�(B
��B
�\B
�(B
� B
��B
��B
��B
��B
��B
��B
��B
�B
�:B
�:B
�oB
��B
��B
�B
�@B
�B
�MB
�MB
��B
��B
��B
��B
��B
�B
��B
�YB
��B
��B
��B
��B
��B
�+B
�+B
�+B
�+B
�+B
��B
��B
��B
�B
�B
�7B
�7B
�B
�kB
�7B
�7B
�kB
�B
��B
�7B
��B
��B
�	B
�qB
�qB
�=B
�qB
�qB
��B
�qB
��B
��B
�B
�B
�~B
�~B
��B
��B
��B
�OB
��B
�!B
�VB
�!B
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
�hB
��B
��B
��B
�@B
�tB
��B
��B
�zB
��B
�zB
�zB
��B
�zB
��B
��B
��B
��B
��B
��B
�RB
�B
�B
��B
��B
��B
��B
��B
��B
��B
�XB
�$B
�$B
�$B
�XB
�XB
�*B
�*B
��B
��B
��B
��B
��B
�0B
��B
��B
�B
�B
�B
�B
�=B
�B
�B
�B
�qB
�qB
�qB
�qB
��B
�qB
�qB
��B
�wB
�B
�CB
�CB
�wB
��B
��B
�wB
��B
��B
��B
�!B
��B
��B
�-B
�aB
��B
��B
�hB
�hB
��B
�B
�9B
�B
�9B
�B
�nB
�9B
�9B
�B
��B
��B
��B
�LB
��B
��B
��B
��B
��B
��B
��B
��B
�B
��B
�RB
��B
��B
��B
��B
�$B
�XB
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
��B
��B
�0B
�0B
�dB
�dB
�dB
�dB
�dB
�dB
��B
��B
��B
��B
��B
�B
�B
��B
�B
��B
��B
��B
��B
�B
�B
�qB
�qB
�qB
�qB
�qB
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
�B
�B
�BB
�BB
�B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
��B
�B
�B
�}B
��B
�}B
��B
��B�B	�B	B�B�BB�B�B
�B�B1B1B�B�B�BB�B1B%B�B+BYB	BYB�B	7B_BYB%BBYBMB�B_BYB�B+BYBB_B%B�B�B�BSB+B�B�B�B%B%B�B%B�B�BYBYB1BYB�B�B�BSB_B�B�B�B�B�B�BB�B�B�B%B_B�B+B1BfB�B�BfB�B�BfB�B+BfB%B�B1B�B�B�B+BYB�B	BYB�B�B�B�B	7B�B�B�B�BSB�B+B�B�B�B�B%B�B_B�B%B�B�B�B�B�B�B�B�B�B�B�B%B�BBMBYBB�B%B�BMB�B�B%B�B+B%B%B�B�BYB�B�B�B%B+BSB+B�BSB�B+BYB�B�BSB%BYB�B+B+BSBYB+B�BYB�B�B�B�B�B�B�B�B�B�B�B_B�B�B�B�B�B�B�B�B�B%BYB�B�B�B_B+BSB�B+BSB�B�B%BSB�B_B�B%B_BSB�B_B�B�B+BB�B�B�B�B�B�BYB+B�B�B�B�BSB+BYB�B_B%B�B_B�BB�B1BB�B�B�B�B�B%B�B�BBB�BB�B�BB�B�BSBB�B�BMB�B�B�B�BYBYB�B�B�B�BSB�B%B�B%B�BSB�B�B�B�B�BB�BSB�B�B�B�BBYBSB�B�B�B�BMB%BYBMB�B�B�BMB�B%B�B�B%B�BSBYB%B�BBYBB�B�BYB%B�BBYB�B�BMB�B�B�BMB%B�B�B�B�B�BB�BYB�B�B�BYBB�BB�B�B%B�B�B�B�B�B�B�BB�B�B�BYB�B�B%B�BSB�B�B�B�B�B�B�B�B�BYB�BSBYBMB�BYB�BBSBYB�B�BYB�BB�B%BB�B�B�B�B%B�BMB�B�B�BMBBYB�BB�B%B�BB�B�B�B�B�BSBMB%BSB�B�B�B�B�B�B�B�B�BYB�BB�B�BB�B�B�B�BYB�BMBB�B�B�BB�B�BMB�B�B�B�B�B{B%B�B�B�B�B�B�B	B�B�BB�B�B�B�B�B_BB%B�B�B{B B�B)_B �B"�B*eB7�BB[BS�BN�BQBLdBQ�B^Bu�Bw�BsBs�Bs�Bx�B�iB�_B��B�B��B��B�B�=B��B��B��B��B��B�UB�EBܒB�NB��B�B�B��B�B�8B�B�B�;B�5B�B��B�/B�%B�lB�B��B�B�TB�B��B�B�B
�BDB�B
=B	lB
�B�B	�B�BB�BxB$B.B�B�BDB�B�B��B�xB��B��B�rBB;B%BB�B�B�PB�>B�JB��B�B;B 4B �BB�B	�BYB�BSB�B_BGB�BSB�B��B�
B��B�B��B��B��B��B� B� B��B�B�]B�>B��B�B�B�B��B��B�&B��B�2B��B�
B�8B�B�B�B�B��B��B�B�vB�BB�pB�B�`B�8B��B��B��B;B �B��B�B�B
rB�B�B-B@BM6BNpBS�BR�BO�BN�BOvBP�BPHBN�BS&BS&BR�BP�BR�BR�BQ�BT�BS[BQ�BP�BS�BT�BT,BS&BS�BS�BT,BS�BR�BT�BZQB[�BR BU�BU�BP�BR�BR BQNBV9BQBM�BQ�BTaBM6BK�BH�BL�BS�BR BR�BTaBo�Bk�Bk�BhsBl�Bp;Bc�BT�B_�BAUB=qB<jB<�B:^B7�B5?BD�BE�BB[BT�BL�BXyB[�BXBT�BR Ba�BVB[�BW
BVBXyBI�BL�BEmBC-BB'B@�B?�B:*B9�B:�B;0B8RB7LB7�B5�B9�B8�B;dB3hB33B;0B:�B@�BA�B<jB>wB9�B9�B6zB8�B8B6B5tB6zB5?B4nB4�B2-B>�B4B:�B5tB)�B&�B-�B(XB&B'RB$B-�B+�B �B)�B	B�B�B@B�BBeB$BuB�B7BIB�B��B�JB�B��B��B��B�2B�2B��B��B�GB��B�vB�B��B��B�dB��B�gB��B�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                    G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                    G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No good data in profile due to conductivity sensor failure after strong salinity drift;                                                                                                                                                                         SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No good data in profile due to conductivity sensor failure after strong salinity drift;                                                                                                                                                                         202104292026552021042920265520210429202655202104292026552021042920265520210429202655SI  SI  ARFMARFM                                                                                                                                                2021020720321920210207203219IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021021800311120210218003111QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021021800311120210218003111QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2021042910194120210429101941IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2021042920270320210429202703IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOW  OW  V1.0V1.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2021042920270320210429202703IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2021042920270320210429202703IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                