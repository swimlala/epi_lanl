CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2019-07-04T08:12:32Z creation; 2023-04-26T19:14:28Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.5   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20190704081232  20230426191428  5905275 5905275 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               6   6AA  AOAO7316_008644_054                 7316_008644_054                 2C  2C  DD  SOLO_II                         SOLO_II                         8644                            8644                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @�ʓ�K��@�ʓ�K��11  @�ʓƧ�@�ʓƧ�@)�%��|@)�%��|�c�6e��c�6e�11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?��@�@B�\@�  @�G�@��R@޸RA ��A��A ��A+�A@  A_\)A\)A�  A�  A��A�  AϮA߮A�A��B�B�
B  B�
B(  B/�
B7�
B@  BHQ�BP  BW�
B`(�Bh  Bo�
Bw�
B�B��B�  B�{B�  B�  B��B�  B�{B�  B�ffB��B�  B�  B�  B�  B�  B�  B�  B�  B�{B�  B�{B�  B�  B�  B�  B�  B�{B�  B�  B�  C   C��C�C  C  C
  C��C��C
=C{C
=C��C  C  C  C��C��C"  C#��C&
=C(
=C*  C,  C.{C0
=C2  C4  C6  C8
=C:{C<
=C=��C?�CA��CD  CF  CH  CJ  CL  CN  CP  CR
=CT{CV
=CX  CY��C\  C^  C`  Cb  Cd  Cf
=Cg��Ci��Cl
=Cn
=Cp
=Cr
=Ct  Cu��Cx
=Cz  C{��C~
=C�C�C�
=C�
=C�C�  C�  C�  C�C�
=C�
=C�  C�  C���C���C���C���C���C���C���C�  C�C�
=C�C�  C�C�  C���C�  C�C�  C���C�  C�C�
=C�  C���C���C�  C�C�
=C�C���C�C�C�  C���C���C���C�  C�  C�  C�C�C�C�
=C���C��C���C�C�C�  C�  C���C�  C�
=C�C�  C���C���C���C�  C�
=C�  C���C�  C�  C�C�C���C�  C�C�  C�  C�C���C���C���C�  C�C�C�  C�  C���C���C�C�  C���C�C�  C���C���C�  C�C�
=C�  C���C���C�  C�  C�
=C�C�C�  C���C���C�C�\C�C�C�  C�C�  C���C���C�  C�C�  C���D }qD �qD}qD  D� D  D� D�D��D  D� D  D}qD  D}qD�qD}qD	  D	��D
  D
� D
�qD� DD�D�D� D  D��DD��D  D� D  D��D  D� D�qD� D  D��D�D}qD�qD}qD�qD� D�D��D  D� D�D� D  D��DD��D�qD}qD�qDz�D  D��D �D ��D!�D!}qD!��D"}qD#  D#}qD#�qD$� D%  D%}qD%�qD&�D'�D'� D(  D(� D)  D)� D*  D*}qD*�qD+��D,�D,}qD,�qD-}qD-�qD.z�D/  D/�D0  D0� D1  D1}qD2  D2��D3  D3��D4  D4}qD5  D5� D6  D6}qD7  D7��D8  D8� D9D9��D:�D:��D;D;��D;�qD<}qD<�qD=z�D=��D>� D?�D?� D?�qD@� DA�DA� DA�qDB� DC�DC��DDDD� DE  DE� DE�qDFz�DF�qDG� DG�qDH� DI  DI� DJ  DJ}qDK  DK��DK�qDL}qDM  DM}qDM�qDN}qDO  DO� DP  DP��DQ�DQ� DR�DR��DS  DS� DT�DT��DU  DU}qDV  DV��DW�DW� DX  DX��DY�DY� DZ  DZ}qDZ��D[}qD\  D\� D\�qD]� D^  D^� D_�D_��D`�D`��Da  Da� Db  Db� Dc  Dc}qDc�qDd}qDe  De}qDe��Df� Dg�Dg}qDh  Dh��Di�Di� Dj  Dj��Dk  Dk}qDl  Dl� Dm�Dm� Dm�qDn}qDn�qDo��Dp�Dp}qDp�qDq� Dr  Dr� Ds  Ds}qDs�qDt��DuDu��Dv�Dv��Dw�Dw��Dw�qDx� Dy�Dy� Dz  Dz� D{  D{}qD{�qD|}qD}  D}� D~  D~� D~�qD� D�qD�>�D�~�D���D���D�>�D�~�D���D�  D�AHD�� D���D�  D�>�D�}qD�� D�  D�@ D�� D��HD�HD�@ D�~�D�� D���D�>�D�~�D�� D�  D�>�D�~�D��qD��qD�>�D�� D��HD��D�@ D�~�D�� D�HD�AHD�� D��HD�HD�>�D�~�D�� D�  D�@ D��HD��HD�HD�AHD��HD�� D���D�>�D�� D��HD�HD�@ D�� D���D���D�>�D�� D�� D�  D�AHD��HD���D��qD�@ D�� D���D���D�>�D�� D�� D�HD�@ D�~�D���D�  D�AHD�� D���D�HD�B�D��HD�� D�HD�@ D�~�D��HD�  D�>�D��HD�D�HD�@ D�~�D���D�  D�>�D�}qD�� D�HD�AHD�~�D�� D���D�=qD�~�D�� D�HD�@ D�~�D�� D���D�@ D�� D���D�HD�@ D�~�D�� D�  D�AHD�� D���D�HD�B�D���D��HD�  D�>�D�� D���D���D�@ D�~�D���D�  D�@ D�~�D���D���D�@ D�� D�D�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�@ D�� D��HD�HD�AHD���D�� D�  D�AHD�� D�� D�HD�@ D��HD��HD���D�@ D���D��HD���D�>�D�� D�D�HD�@ D�� D���D�HD�C�D���D�D��D�B�D���D�D��D�AHD�~�D�� D�  D�@ D��HD��HD�  D�=qD�~�D�� D�HD�B�D���D�D�HD�@ D�� D�� D���D�=qD�� D���D��qD�AHD���D�� D��qD�=qD�~�D�� D�  D�@ D��HD��HD��D�@ D�~�D��HD�  D�AHD�� D�� D�  D�@ D�� D��HD�HD�AHD�� D�� D���D�@ D�� D�� D�HD�AHD��HD�� D�  D�@ D�~�D���D�  D�AHD�� D�� D���D�>�D�~�D¾�D�  D�@ DÀ D�� D���D�>�DĀ D�� D���D�>�Dŀ D�� D���D�>�Dƀ Dƾ�D�  D�@ Dǀ D�� D���D�>�D�~�D�� D�  D�AHDɂ�D��HD�  D�AHDʂ�D�� D��qD�@ D�~�D˾�D�  D�@ D́HD̾�D�  D�AHD̀ D�� D�HD�@ D�}qDξ�D�  D�>�DρHD�� D�  D�@ D�}qDо�D�HD�@ D�~�D�� D�HD�>�D�}qD�� D�HD�AHDӀ D�� D�HD�@ DԀ D�� D���D�@ DՁHD��HD�HD�AHD�~�DֽqD���D�=qD�~�D׾�D���D�=qD�}qDؾ�D���D�>�Dـ D�� D�HD�AHDځHD��HD�  D�=qD�~�D�� D���D�=qD܀ D��HD�HD�B�D݁HD�� D�HD�@ D�~�D�� D�  D�>�D�~�D��HD�  D�@ D�� DྸD�HD�@ D�HD��HD��qD�=qD� D�� D�  D�AHD� D�D��D�AHD䂏D�� D�  D�@ D�HD�D�  D�>�D�~�D�� D�  D�>�D�~�D�� D���D�@ D� D��HD�  D�>�D� D�� D�HD�>�D� D��HD�  D�>�D� D�� D�HD�B�D�HD�� D���D�@ D킏D��HD�HD�@ D�~�D��HD�  D�>�D� DﾸD���D�AHD���D�D��D�AHD�HD��HD��D�@ D�~�D�� D�  D�@ D�HD��HD�  D�@ D� D�� D�  D�AHD�~�D�� D�  D�>�D�� D�� D�HD�AHD�� D��HD�HD�@ D��HD�� D���D�@ D��HD��HD�  D�.D�n>�G�>�?B�\?�  ?���?��?��H@
=q@(�@333@G�@Y��@k�@�  @�=q@�33@�(�@��@�\)@�Q�@�G�@�=q@�z�@�  @���@��@��HA33A�A(�A  AA=qA�RA"�\A'
=A+�A0��A4z�A8��A>{AB�\AFffAJ�HAN�RAS33AXQ�A\(�A`��Ae�Aj=qAn�RAq�AuAz�HA�  A�=qA�(�A�{A���A��HA���A��RA�G�A��A�p�A�\)A���A�z�A�ffA�Q�A��HA�p�A�\)A���A��
A�ffA���A��\A���A�
=A��A�(�A�{A�Q�A\A��A�\)A�G�A��
A�{AУ�A�=qA���A�
=Aٙ�A��
A�A�  A�\A���A�RA���A�A�{A�Q�A�\A�z�A�
=A���A�(�A�ffB Q�Bp�B�HB  B�B=qB�B��B
{B33Bz�BB
=B(�BG�B�RB  B�B{B\)B��B�B33BQ�Bp�B�\B�
B!G�B"ffB#�B$��B&=qB'�B(��B)B+
=B,z�B-B/
=B0(�B1G�B2�RB4(�B5G�B6ffB7�B9G�B:�\B;�
B<��B>=qB?�BA�BBffBC�BE�BF�\BH  BIp�BJ�RBL  BMp�BO
=BP��BQ�BS33BT��BV{BW�BY�BZ�\B[�B]�B^�RB`Q�BaBc
=BdQ�BeBg33Bh��Bj{Bk�Bl��Bn{Bo�Bp��BrffBs�Bt��Bv=qBw�By�Bz�RB|(�B}G�B~�\B�  B��RB�p�B�(�B���B�p�B�  B��RB�\)B�{B���B��B�=qB��HB��B�(�B���B��B�z�B�33B��B��\B�33B�  B���B��B�=qB���B���B�Q�B���B�B�z�B�G�B�  B���B�G�B�  B��RB��B�=qB��HB��B�=qB�
=B�B�z�B�33B��B�z�B�33B��
B�z�B��B�B�ffB���B�p�B��
B�=qB��\B��HB�33B��B�B��B�{B�(�B�Q�B�z�B��RB��HB�
=B��B�G�B�p�B��B��B�(�B�Q�B�z�B��\B���B���B�G�B��B��B��
B�  B�(�B�ffB���B���B��B�G�B�p�B��B��B�{B�ffB���B���B��HB��B�\)B��B�  B�{B�ffB���B��RB���B�33B��B�B�{B�Q�B�z�B��RB���B�33B��B�B�{B�Q�B���B��HB��B�\)B���B��B�=qB��\B��HB�33B�p�B���B��B�Q�B���B���B�G�B��B��
B�{B�ffB��RB�
=B�\)B�B�{B�Q�B���B��HB��B��B��
B�(�B�z�B��RB�
=B�G�B���B��B�=qB\B��HB�33B�p�B�B�  B�Q�Bď\B��HB��BŅB��
B�(�B�z�BƸRB�
=B�G�BǅB��
B�(�B�z�B���B��B�p�B�B�  B�Q�Bʏ\B���B��B�\)B˙�B��B�(�B̏\B��HB��B�\)BͮB�  B�=qB�z�B���B�
=B�G�BυB��
B�(�B�ffBиRB���B�G�BхB��
B�{B�ffBҸRB���B�G�B�p�BӮB��B�(�B�ffBԸRB�
=B�G�Bՙ�B��B�=qB�z�BָRB�
=B�G�Bי�B��
B�{B�ffBأ�B���B�33BمB��B�(�B�z�B���B�
=B�\)Bۙ�B��
B�(�B�ffBܣ�B���B�G�B݅B��
B�{B�ffB޸RB�
=B�\)B߮B�  B�Q�B��B���B�G�B�B��B�(�B�z�B���B�
=B�\)B�B�  B�ffB�RB�
=B�p�B�B�(�B�z�B���B��B�B��
B�(�B�z�B��HB�G�B陚B�  B�ffB�RB��B�B��
B�=qB��B���B�p�B�B�(�B�\B���B�\)B�B�(�B��\B���B�\)B��
B�Q�B�RB�33B�B�  B�z�B���B�\)B�B�=qB��RB�33B��B�(�B��\B�
=B��B�  B�ffB��HB�\)B��
B�Q�B���B�33B�B�(�B���B��B���C 
=C G�C �\C C  C=qC�CC  C=qC�CC  CG�C�CC
=CG�C�CC
=CG�C�\C��C
=CQ�C�\C��C{CQ�C��C�
C�CffC��C�C	(�C	z�C	�RC
  C
G�C
�C
�
C�CffC��C�C33Cz�CC  C=qC�C��C{CQ�C�\C�
C{C\)C��C�HC(�CffC�C��C=qC�CC{C\)C��C�HC(�Cp�C�C��C33Cp�C�RC  CG�C�\C�
C�CffC�C��C33Cz�CC  CG�C�\C��C{CQ�C��C�HC�C\)C��C�C33Cp�C�RC  CG�C�\C�
C�C\)C��C�HC�C\)C��C�HC(�Cp�C�C   C G�C �C ��C!  C!G�C!�\C!��C"�C"ffC"�C"��C#33C#p�C#�C#��C$33C$�C$��C%{C%Q�C%�\C%��C&{C&\)C&��C&�C'(�C'\)C'��C'�HC((�C(p�C(�RC(��C)(�C)p�C)C*
=C*G�C*�C*C+  C+Q�C+��C+�
C,{C,Q�C,�\C,�
C-(�C-ffC-��C-�
C.�C.\)C.��C.�C/(�C/\)C/��C/�C033C0p�C0��C0�C133C1z�C1�C1�C2(�C2z�C2C2��C333C3z�C3C4
=C4G�C4�C4��C5{C5\)C5��C5�HC6�C6p�C6�RC6��C733C7z�C7C8
=C8Q�C8�\C8�
C9(�C9ffC9��C9�C:33C:�C:��C;  C;G�C;�\C;�HC<�C<\)C<��C<��C==qC=z�C=�RC>  C>Q�C>��C>�C?(�C?ffC?�RC@
=C@Q�C@��C@�
CA(�CAz�CACB
=CBG�CB�\CB�CC33CCz�CC�RCD
=CDffCD�RCD��CE=qCE�CE�HCF33CFp�CF�RCG
=CG\)CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                     ?��@�@B�\@�  @�G�@��R@޸RA ��A��A ��A+�A@  A_\)A\)A�  A�  A��A�  AϮA߮A�A��B�B�
B  B�
B(  B/�
B7�
B@  BHQ�BP  BW�
B`(�Bh  Bo�
Bw�
B�B��B�  B�{B�  B�  B��B�  B�{B�  B�ffB��B�  B�  B�  B�  B�  B�  B�  B�  B�{B�  B�{B�  B�  B�  B�  B�  B�{B�  B�  B�  C   C��C�C  C  C
  C��C��C
=C{C
=C��C  C  C  C��C��C"  C#��C&
=C(
=C*  C,  C.{C0
=C2  C4  C6  C8
=C:{C<
=C=��C?�CA��CD  CF  CH  CJ  CL  CN  CP  CR
=CT{CV
=CX  CY��C\  C^  C`  Cb  Cd  Cf
=Cg��Ci��Cl
=Cn
=Cp
=Cr
=Ct  Cu��Cx
=Cz  C{��C~
=C�C�C�
=C�
=C�C�  C�  C�  C�C�
=C�
=C�  C�  C���C���C���C���C���C���C���C�  C�C�
=C�C�  C�C�  C���C�  C�C�  C���C�  C�C�
=C�  C���C���C�  C�C�
=C�C���C�C�C�  C���C���C���C�  C�  C�  C�C�C�C�
=C���C��C���C�C�C�  C�  C���C�  C�
=C�C�  C���C���C���C�  C�
=C�  C���C�  C�  C�C�C���C�  C�C�  C�  C�C���C���C���C�  C�C�C�  C�  C���C���C�C�  C���C�C�  C���C���C�  C�C�
=C�  C���C���C�  C�  C�
=C�C�C�  C���C���C�C�\C�C�C�  C�C�  C���C���C�  C�C�  C���D }qD �qD}qD  D� D  D� D�D��D  D� D  D}qD  D}qD�qD}qD	  D	��D
  D
� D
�qD� DD�D�D� D  D��DD��D  D� D  D��D  D� D�qD� D  D��D�D}qD�qD}qD�qD� D�D��D  D� D�D� D  D��DD��D�qD}qD�qDz�D  D��D �D ��D!�D!}qD!��D"}qD#  D#}qD#�qD$� D%  D%}qD%�qD&�D'�D'� D(  D(� D)  D)� D*  D*}qD*�qD+��D,�D,}qD,�qD-}qD-�qD.z�D/  D/�D0  D0� D1  D1}qD2  D2��D3  D3��D4  D4}qD5  D5� D6  D6}qD7  D7��D8  D8� D9D9��D:�D:��D;D;��D;�qD<}qD<�qD=z�D=��D>� D?�D?� D?�qD@� DA�DA� DA�qDB� DC�DC��DDDD� DE  DE� DE�qDFz�DF�qDG� DG�qDH� DI  DI� DJ  DJ}qDK  DK��DK�qDL}qDM  DM}qDM�qDN}qDO  DO� DP  DP��DQ�DQ� DR�DR��DS  DS� DT�DT��DU  DU}qDV  DV��DW�DW� DX  DX��DY�DY� DZ  DZ}qDZ��D[}qD\  D\� D\�qD]� D^  D^� D_�D_��D`�D`��Da  Da� Db  Db� Dc  Dc}qDc�qDd}qDe  De}qDe��Df� Dg�Dg}qDh  Dh��Di�Di� Dj  Dj��Dk  Dk}qDl  Dl� Dm�Dm� Dm�qDn}qDn�qDo��Dp�Dp}qDp�qDq� Dr  Dr� Ds  Ds}qDs�qDt��DuDu��Dv�Dv��Dw�Dw��Dw�qDx� Dy�Dy� Dz  Dz� D{  D{}qD{�qD|}qD}  D}� D~  D~� D~�qD� D�qD�>�D�~�D���D���D�>�D�~�D���D�  D�AHD�� D���D�  D�>�D�}qD�� D�  D�@ D�� D��HD�HD�@ D�~�D�� D���D�>�D�~�D�� D�  D�>�D�~�D��qD��qD�>�D�� D��HD��D�@ D�~�D�� D�HD�AHD�� D��HD�HD�>�D�~�D�� D�  D�@ D��HD��HD�HD�AHD��HD�� D���D�>�D�� D��HD�HD�@ D�� D���D���D�>�D�� D�� D�  D�AHD��HD���D��qD�@ D�� D���D���D�>�D�� D�� D�HD�@ D�~�D���D�  D�AHD�� D���D�HD�B�D��HD�� D�HD�@ D�~�D��HD�  D�>�D��HD�D�HD�@ D�~�D���D�  D�>�D�}qD�� D�HD�AHD�~�D�� D���D�=qD�~�D�� D�HD�@ D�~�D�� D���D�@ D�� D���D�HD�@ D�~�D�� D�  D�AHD�� D���D�HD�B�D���D��HD�  D�>�D�� D���D���D�@ D�~�D���D�  D�@ D�~�D���D���D�@ D�� D�D�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�@ D�� D��HD�HD�AHD���D�� D�  D�AHD�� D�� D�HD�@ D��HD��HD���D�@ D���D��HD���D�>�D�� D�D�HD�@ D�� D���D�HD�C�D���D�D��D�B�D���D�D��D�AHD�~�D�� D�  D�@ D��HD��HD�  D�=qD�~�D�� D�HD�B�D���D�D�HD�@ D�� D�� D���D�=qD�� D���D��qD�AHD���D�� D��qD�=qD�~�D�� D�  D�@ D��HD��HD��D�@ D�~�D��HD�  D�AHD�� D�� D�  D�@ D�� D��HD�HD�AHD�� D�� D���D�@ D�� D�� D�HD�AHD��HD�� D�  D�@ D�~�D���D�  D�AHD�� D�� D���D�>�D�~�D¾�D�  D�@ DÀ D�� D���D�>�DĀ D�� D���D�>�Dŀ D�� D���D�>�Dƀ Dƾ�D�  D�@ Dǀ D�� D���D�>�D�~�D�� D�  D�AHDɂ�D��HD�  D�AHDʂ�D�� D��qD�@ D�~�D˾�D�  D�@ D́HD̾�D�  D�AHD̀ D�� D�HD�@ D�}qDξ�D�  D�>�DρHD�� D�  D�@ D�}qDо�D�HD�@ D�~�D�� D�HD�>�D�}qD�� D�HD�AHDӀ D�� D�HD�@ DԀ D�� D���D�@ DՁHD��HD�HD�AHD�~�DֽqD���D�=qD�~�D׾�D���D�=qD�}qDؾ�D���D�>�Dـ D�� D�HD�AHDځHD��HD�  D�=qD�~�D�� D���D�=qD܀ D��HD�HD�B�D݁HD�� D�HD�@ D�~�D�� D�  D�>�D�~�D��HD�  D�@ D�� DྸD�HD�@ D�HD��HD��qD�=qD� D�� D�  D�AHD� D�D��D�AHD䂏D�� D�  D�@ D�HD�D�  D�>�D�~�D�� D�  D�>�D�~�D�� D���D�@ D� D��HD�  D�>�D� D�� D�HD�>�D� D��HD�  D�>�D� D�� D�HD�B�D�HD�� D���D�@ D킏D��HD�HD�@ D�~�D��HD�  D�>�D� DﾸD���D�AHD���D�D��D�AHD�HD��HD��D�@ D�~�D�� D�  D�@ D�HD��HD�  D�@ D� D�� D�  D�AHD�~�D�� D�  D�>�D�� D�� D�HD�AHD�� D��HD�HD�@ D��HD�� D���D�@ D��HD��HD�  D�.G�O�>�G�>�?B�\?�  ?���?��?��H@
=q@(�@333@G�@Y��@k�@�  @�=q@�33@�(�@��@�\)@�Q�@�G�@�=q@�z�@�  @���@��@��HA33A�A(�A  AA=qA�RA"�\A'
=A+�A0��A4z�A8��A>{AB�\AFffAJ�HAN�RAS33AXQ�A\(�A`��Ae�Aj=qAn�RAq�AuAz�HA�  A�=qA�(�A�{A���A��HA���A��RA�G�A��A�p�A�\)A���A�z�A�ffA�Q�A��HA�p�A�\)A���A��
A�ffA���A��\A���A�
=A��A�(�A�{A�Q�A\A��A�\)A�G�A��
A�{AУ�A�=qA���A�
=Aٙ�A��
A�A�  A�\A���A�RA���A�A�{A�Q�A�\A�z�A�
=A���A�(�A�ffB Q�Bp�B�HB  B�B=qB�B��B
{B33Bz�BB
=B(�BG�B�RB  B�B{B\)B��B�B33BQ�Bp�B�\B�
B!G�B"ffB#�B$��B&=qB'�B(��B)B+
=B,z�B-B/
=B0(�B1G�B2�RB4(�B5G�B6ffB7�B9G�B:�\B;�
B<��B>=qB?�BA�BBffBC�BE�BF�\BH  BIp�BJ�RBL  BMp�BO
=BP��BQ�BS33BT��BV{BW�BY�BZ�\B[�B]�B^�RB`Q�BaBc
=BdQ�BeBg33Bh��Bj{Bk�Bl��Bn{Bo�Bp��BrffBs�Bt��Bv=qBw�By�Bz�RB|(�B}G�B~�\B�  B��RB�p�B�(�B���B�p�B�  B��RB�\)B�{B���B��B�=qB��HB��B�(�B���B��B�z�B�33B��B��\B�33B�  B���B��B�=qB���B���B�Q�B���B�B�z�B�G�B�  B���B�G�B�  B��RB��B�=qB��HB��B�=qB�
=B�B�z�B�33B��B�z�B�33B��
B�z�B��B�B�ffB���B�p�B��
B�=qB��\B��HB�33B��B�B��B�{B�(�B�Q�B�z�B��RB��HB�
=B��B�G�B�p�B��B��B�(�B�Q�B�z�B��\B���B���B�G�B��B��B��
B�  B�(�B�ffB���B���B��B�G�B�p�B��B��B�{B�ffB���B���B��HB��B�\)B��B�  B�{B�ffB���B��RB���B�33B��B�B�{B�Q�B�z�B��RB���B�33B��B�B�{B�Q�B���B��HB��B�\)B���B��B�=qB��\B��HB�33B�p�B���B��B�Q�B���B���B�G�B��B��
B�{B�ffB��RB�
=B�\)B�B�{B�Q�B���B��HB��B��B��
B�(�B�z�B��RB�
=B�G�B���B��B�=qB\B��HB�33B�p�B�B�  B�Q�Bď\B��HB��BŅB��
B�(�B�z�BƸRB�
=B�G�BǅB��
B�(�B�z�B���B��B�p�B�B�  B�Q�Bʏ\B���B��B�\)B˙�B��B�(�B̏\B��HB��B�\)BͮB�  B�=qB�z�B���B�
=B�G�BυB��
B�(�B�ffBиRB���B�G�BхB��
B�{B�ffBҸRB���B�G�B�p�BӮB��B�(�B�ffBԸRB�
=B�G�Bՙ�B��B�=qB�z�BָRB�
=B�G�Bי�B��
B�{B�ffBأ�B���B�33BمB��B�(�B�z�B���B�
=B�\)Bۙ�B��
B�(�B�ffBܣ�B���B�G�B݅B��
B�{B�ffB޸RB�
=B�\)B߮B�  B�Q�B��B���B�G�B�B��B�(�B�z�B���B�
=B�\)B�B�  B�ffB�RB�
=B�p�B�B�(�B�z�B���B��B�B��
B�(�B�z�B��HB�G�B陚B�  B�ffB�RB��B�B��
B�=qB��B���B�p�B�B�(�B�\B���B�\)B�B�(�B��\B���B�\)B��
B�Q�B�RB�33B�B�  B�z�B���B�\)B�B�=qB��RB�33B��B�(�B��\B�
=B��B�  B�ffB��HB�\)B��
B�Q�B���B�33B�B�(�B���B��B���C 
=C G�C �\C C  C=qC�CC  C=qC�CC  CG�C�CC
=CG�C�CC
=CG�C�\C��C
=CQ�C�\C��C{CQ�C��C�
C�CffC��C�C	(�C	z�C	�RC
  C
G�C
�C
�
C�CffC��C�C33Cz�CC  C=qC�C��C{CQ�C�\C�
C{C\)C��C�HC(�CffC�C��C=qC�CC{C\)C��C�HC(�Cp�C�C��C33Cp�C�RC  CG�C�\C�
C�CffC�C��C33Cz�CC  CG�C�\C��C{CQ�C��C�HC�C\)C��C�C33Cp�C�RC  CG�C�\C�
C�C\)C��C�HC�C\)C��C�HC(�Cp�C�C   C G�C �C ��C!  C!G�C!�\C!��C"�C"ffC"�C"��C#33C#p�C#�C#��C$33C$�C$��C%{C%Q�C%�\C%��C&{C&\)C&��C&�C'(�C'\)C'��C'�HC((�C(p�C(�RC(��C)(�C)p�C)C*
=C*G�C*�C*C+  C+Q�C+��C+�
C,{C,Q�C,�\C,�
C-(�C-ffC-��C-�
C.�C.\)C.��C.�C/(�C/\)C/��C/�C033C0p�C0��C0�C133C1z�C1�C1�C2(�C2z�C2C2��C333C3z�C3C4
=C4G�C4�C4��C5{C5\)C5��C5�HC6�C6p�C6�RC6��C733C7z�C7C8
=C8Q�C8�\C8�
C9(�C9ffC9��C9�C:33C:�C:��C;  C;G�C;�\C;�HC<�C<\)C<��C<��C==qC=z�C=�RC>  C>Q�C>��C>�C?(�C?ffC?�RC@
=C@Q�C@��C@�
CA(�CAz�CACB
=CBG�CB�\CB�CC33CCz�CC�RCD
=CDffCD�RCD��CE=qCE�CE�HCF33CFp�CF�RCG
=CG\)CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                     @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�9G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AߑhAߕ�Aߕ�Aߕ�Aߗ�Aߗ�AߍPA߃A߃A߇+AߋDAߋDA߉7A߃A�~�A�bNA�9XA�oA���A��
AދDAݴ9A��Aܩ�A�l�A��Aڙ�A�A�A�ȴA�{A�jA��#A�
=A̕�A��yA�S�Aʙ�A��mA�  A�A�A�VA��AƃA�p�A�VA���A��;A�5?A��#A���A�t�A�A��A�=qA���A�jA��yA�;dA�|�A���A��-A��jA��A�1A�z�A�A�ZA��FA��A��+A��A���A�$�A�z�A�ƨA���A��;A��A�t�A��+A��9A��HA�7LA�VA�ZA�M�Ay?}At��Aq��An9XAhAb�HA[S�AU�AM�
AI33AF1AB^5A>��A<��A;?}A;�A:ffA7&�A5�A3�mA2�uA1�FA/p�A-��A,ȴA-\)A.�DA.�9A.9XA-�A-;dA,  A*�RA)�;A)+A(z�A(ZA(�jA)33A)\)A)S�A)/A(�RA(JA';dA&��A&ffA%��A%A%
=A$r�A$5?A#C�A#�A#A"�A"�+A"1'A!�^A �/A ^5A��A��At�A�yA��A��A�\A{A  A�A��A�-A�hA+A�9A�hA�+A�AA��A�A��A5?AA;dA�A~�A�#A�hA�A�jA��A�uA-A�^At�A/A�A��AI�A�-A;dA�/A��A=qAJA��A�A�A��A��A��AƨA�A�!AbNA$�A�A�hA�A\)AC�A+AA�A�Ar�A(�A�^Al�AO�A"�A
ĜA
bNA	ƨA	oAȴA�AffA �A�mA�A~�AZA{A�FA�AVA�jA=qA��At�AS�A�AjA-AI�A��AA�^A�hAVA z�@��
@�|�@��@���@��m@��y@�5?@��-@��@� �@��F@�=q@�G�@���@�1@�ƨ@���@�@�@��@�@���@��@�K�@���@�@�1'@���@�F@�@�C�@�"�@�@��@�J@�Ĝ@��@��@�|�@�+@��@�/@䛦@�  @�dZ@�33@��y@�E�@��@�@���@��D@�Q�@�ƨ@�C�@���@�~�@�V@�J@݉7@���@�z�@��m@�
=@�v�@�$�@ف@���@أ�@�Q�@��@�;d@��@���@�Z@ԋD@�Ĝ@Ԭ@ԣ�@�ƨ@�|�@�S�@�33@�n�@с@�?}@��`@�  @�K�@ΰ!@Χ�@�~�@��@ͩ�@�?}@�&�@�V@�Ĝ@̃@�A�@��@�
=@ʏ\@�^5@�~�@�@��y@ʸR@�~�@�-@���@Ɂ@�X@�Ĝ@�j@�|�@�$�@š�@�X@�V@���@��@��@ģ�@ě�@�bN@�9X@�  @þw@�dZ@�o@�@��@�/@�p�@��@�Q�@�b@��
@�K�@��R@�M�@��T@��^@�p�@��/@��9@���@�1'@��
@�"�@�
=@���@�J@�`B@���@�A�@��w@�;d@��H@���@�^5@���@�p�@��@�I�@�1@��;@��P@�S�@�"�@��@�n�@�@��#@��@�`B@�7L@�z�@�(�@��m@��@��@�V@��T@��7@�Z@��;@�K�@��H@��@��R@�E�@���@��@�Ĝ@��@�j@�9X@�b@�ƨ@��P@�;d@���@���@��#@��-@�7L@��@�bN@���@�33@��@�V@�5?@��@��@��@�Ĝ@� �@��
@��w@���@�S�@��y@���@�~�@�^5@�{@���@�V@�j@�1'@�ƨ@�;d@���@��+@�ff@�$�@��#@���@�7L@��@� �@��@�33@��@��\@�ff@�E�@��@��T@��^@�x�@�&�@�Ĝ@��@�r�@�1'@�b@�  @��;@��@�\)@�;d@��@��y@�~�@�^5@��@�O�@���@��u@�Z@�1@��F@�S�@�"�@��y@��!@���@�~�@�v�@�^5@�=q@���@���@�`B@��@��`@��u@��@�bN@� �@���@��y@�n�@�5?@��T@���@���@��@�`B@�/@���@���@��j@��9@�r�@�9X@�(�@��@���@��@�S�@�o@���@��+@��@��-@��7@�&�@��@�Z@���@�t�@�S�@�33@��@���@�~�@�M�@�{@���@���@�`B@�/@���@��u@�Z@�A�@�1'@��@l�@+@�@~�R@~@|�@|j@{�m@{S�@z��@zn�@z�@y�^@yx�@x��@xr�@w�@wl�@vȴ@vv�@v5?@u`B@tI�@s�
@s33@r��@r=q@q�@q��@qx�@pĜ@pb@o
=@n��@nE�@m��@mO�@l�@l9X@k�F@k��@k"�@j��@jn�@jJ@i�#@i��@i%@hr�@g�@g|�@g\)@f��@fff@f@e�h@d�@d9X@c��@c33@b��@b^5@b�@a��@a7L@`Ĝ@`Q�@_�w@_+@^�R@^��@^E�@^{@]`B@\�@\Z@\9X@[�
@[�@[t�@[C�@Z�!@Z=q@Y�#@XĜ@X  @W��@W|�@Wl�@Wl�@W;d@V��@V��@Vff@V$�@U��@U�@T�j@TI�@S�F@S@R��@Q�^@Q%@P��@Pr�@P1'@O|�@O+@Nv�@M�@MV@L��@Lz�@L(�@Kƨ@K��@K@J�\@I�@I��@IX@H�`@H  @Gl�@G�@F�@F�+@FE�@E�-@E?}@D��@D�@Dz�@Dj@DZ@D(�@Cƨ@C�@C33@B�H@B��@Bn�@B-@A�7@A�@@�`@@��@@�@@Q�@?�@>�y@>$�@=��@=��@=p�@=�@<�/@<�@<�D@<z�@<j@<�@;o@:�\@:~�@:^5@:-@:�@:J@9�@9�#@9��@9��@8Ĝ@81'@7�@7|�@6��@6ȴ@6��@6��@6ff@6$�@5�T@5@5O�@5�@5V@4�@4�j@4�D@4j@3ƨ@3C�@2�@2�!@2n�@1��@1�^@1�7@17L@1�@1%@0��@0�u@0Q�@/�;@/�P@/\)@.��@.��@.ff@.E�@.E�@.5?@.{@-@-�h@-p�@-?}@-V@,�@,�@,�D@,j@,Z@,I�@,(�@,1@+��@+C�@+33@*�H@*n�@)��@)��@)X@)&�@)�@)�@)%@)%@(��@(�`@(�`@(��@(r�@(  @(b@(  @'�;@'�w@'�@'l�@'K�@';d@'+@&�@&��@&V@&{@%��@%`B@%/@$��@$�@$�@$j@$�@#�F@#��@#dZ@#"�@"�H@"�\@"-@"�@!�#@!�7@ �9@ Q�@  �@   @��@�w@�@�P@�P@�P@l�@\)@\)@K�@K�@;d@
=@�@�R@5?@{@�@�-@�h@�@�@�j@Z@��@�
@�F@�@dZ@"�@��@��@^5@�@�@�^@�^@�^@�^@��@�7@7L@�`@r�@ �@  @�@�;@��@�@K�@�@�+@V@�T@�h@p�@/@V@��@�j@�D@z�@(�@��@�m@�m@�m@�
@��@�@t�@dZ@33@o@@�@�@�@��@n�@�@��@�7@7L@%@��@�`@��@��@�u@�@Q�@  @�w@�@�P@\)@+@�y@�R@ff@E�@$�@$�@{@�@�T@�-@�h@`B@?}@/@�@V@��@�/@��@(�@�m@�
@ƨ@�F@�@dZ@S�@
�@
��@
��@
�\@
�\AߓuAߕ�AߑhAߑhAߓuAߕ�Aߕ�AߑhAߕ�Aߗ�Aߕ�AߑhAߓuAߙ�Aߕ�Aߙ�Aߗ�Aߙ�Aߛ�Aߙ�Aߕ�Aߗ�Aߙ�AߋDA߃A߁A߅A߇+A߃A߃A߁A߃A߃A߃A߅AߍPAߏ\A߇+A߉7AߍPAߑhA߉7A߅A߇+AߋDAߛ�Aߙ�A�~�A߁A߅A߃A�|�A�z�A߃A߇+A߇+A߅A߃A߅A�|�A�~�A�p�A�x�A�~�AߋDA߇+A�~�A�~�A�hsA�l�A߃A�`BA�;dA�-A��A�/A�K�A�=qA�=qA�?}A�A�A�E�A�5?A�A���A���A���A���A���A���A���A���A��A��A��A��A��HA���A���A���A�ȴA�ĜA�ƨA޼jA޲-Aޛ�A�ffA�9XA�+A��A��TAݲ-Aݡ�A݅A�`BA�=qA�JA��A��yA��
A�Aܺ^AܸRAܶFAܬAܥ�Aܡ�Aܡ�Aܗ�A܉7A�t�A�ffA�`BA�`BA�^5A�VA�C�A�;dA�-A�1A��A���A۬Aۏ\A� �A�;dA���A�p�A���AؓuA�7LA�
=A�A���A��`A�x�A��TA���A���A�"�AՅA�?}A���A�t�A�1A���A�M�AѼjA��
A�ZA��TAϧ�A�5?AΑhA�A͉7A�v�A�33A�A�bA�JA�JA�1A�A��yA̼jA̝�A�ffA�A�A�(�A�oA���A��#A���Aˣ�A˃A�jA�XA�C�A�1'A��A���A��TAʺ^A�O�A� �A�1A��A��`A��HA��;A��;A��
Aɡ�A���Aȴ9Aȗ�AȍPAȇ+A�~�A�r�A�G�A��A��yAǩ�Aǉ7A�S�A�5?A�+A��A�bA���A��A��TA��;A�ĜAƕ�AƏ\AƃA�n�A��Aţ�A�~�A�l�A�S�A�9XA�"�A��A��A�JA�A���A��Ać+A�Aá�A�|�A�hsA�{A��A���A�ȴA¾wA®A�A�jA�;dA�/A�+A�$�A�"�A��A��A��A�{A�oA�bA�{A�{A�{A�oA�bA�bA�bA�
=A�1A��A��mA���A��!A���A���A���A��uA��hA��DA��A�r�A�ZA�?}A�33A�/A��A�JA�  A���A��A��A��mA��/A��
A���A���A���A���A�ȴA�A��wA��RA��-A��A���A���A��\A��A�t�A�`BA�O�A�A�A�+A�bA��A�ĜA���A��A�jA�K�A�/A�{A���A��;A�ƨA��A���A��A�~�A�|�A�r�A�ffA�^5A�E�A�7LA�&�A��A�JA��HA���A�A��9A���A��\A�z�A�dZA�S�A�I�A�;dA�+A��A��A�oA�
=A�  A���A���A���A��A��A��A��TA��#A���A���A��wA��FA���A���A��A�p�A�VA�;dA��A���A�1'A��mA���A���A��jA���A�v�A�`BA�M�A�A�A�9XA�/A�-A�"�A��A��A�JA��A��wA�|�A� �A��
A���A��\A�|�A�hsA�VA�G�A�?}A�+A�&�A�{A�oA�1A���A��A��mA��HA��/A���A��!A���A��PA�|�A�l�A�^5A�A�A�7LA�1'A�+A� �A���A���A�t�A�"�A��A��/A��jA��A�O�A�/A��mA���A��A���A��uA��A�l�A�VA�=qA���A���A�t�A�=qA�(�A��A�bA�A���A��A���A��FA���A�p�A�JA�ĜA���A�`BA�&�A��9A�dZA�=qA�oA��mA��9A�|�A�C�A�bA��A�ĜA��^A��!A���A���A�|�A�Q�A�;dA�/A�&�A��A�oA�A��A�ȴA��!A�ZA��A���A��A��;A��\A�ffA���A�|�A�I�A�%A���A��A��-A�v�A� �A��RA��+A�ZA�S�A�M�A�9XA���A���A�`BA�5?A� �A�1A��A���A�O�A�A���A���A��PA�|�A�p�A�ffA�XA�K�A�?}A�+A�%A��TA��A�dZA�-A���A��jA���A��uA��7A�~�A�v�A�XA��A���A�{A�A��+A�JA��#A���A��PA�`BA�(�A�  A��;A���A��wA��\A�S�A� �A��A���A�A�t�A�7LA�"�A�oA�A��A���A�E�A�"�A��A�ȴA��PA�bA���A���A��+A��A�VA��`A��A��A��FA�t�A�;dA�+A�(�A�&�A��A�1A��A���A���A�t�A�`BA�O�A�E�A�-A���A��A��#A��uA��A���A���A�z�A�/A�%A��HA��-A�^5A��;A�ffA�1'A���A�ĜA��A�I�A��A��A�A�r�A���A�x�A��TA�1'A�t�A�A���A���A�5?A��A��RA�r�A�M�A��A���A���A��A�v�A�M�A�5?A�&�A�VA�ƨA�jA�+A�ƨA�bNA�
=A�/A��+A��RA��#A�9XA���A�%A�7LA~��A}�TA}&�A|bAz�`Azn�Ay�FAx��Aw�TAwt�Av��AvbNAu33At�RAtjAt�AtbAtbAt  As��As;dAr�Aqt�Aq%Ap�!Ap�DApbNAp  Ao�Ao&�An�jAmoAk��Ak�Aj1'Ai\)Ah�RAg�mAg`BAf�jAe��Ae�Ad��Ac�TAc`BAbv�Ab1'Aa�#Aa
=A^�RA]�TA]XA\=qA[hsAZr�AX�DAW��AWx�AV��AU�AU�AU�AT�jATbNASp�AQ�APANQ�AL��AL�uALI�AK��AK��AK;dAJ�AI�#AIAH^5AG�AGx�AGXAF��AE�AEƨAEAE�^AE�-AE�hAD�yAD$�AC"�AB��AA��A@$�A?�;A?��A?S�A>��A>�+A>ZA>9XA>JA=�
A=��A=&�A<�/A<ZA<�A;�#A;��A;p�A;`BA;O�A;C�A;�A;oA;VA;VA;
=A;%A;
=A;oA;/A;O�A;hsA;XA;"�A:��A9��A8��A8��A8 �A7��A7dZA6�A6�uA6v�A6M�A6 �A5��A5�7A5XA5�A5%A5%A4�/A4v�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                     AߑhAߕ�Aߕ�Aߕ�Aߗ�Aߗ�AߍPA߃A߃A߇+AߋDAߋDA߉7A߃A�~�A�bNA�9XA�oA���A��
AދDAݴ9A��Aܩ�A�l�A��Aڙ�A�A�A�ȴA�{A�jA��#A�
=A̕�A��yA�S�Aʙ�A��mA�  A�A�A�VA��AƃA�p�A�VA���A��;A�5?A��#A���A�t�A�A��A�=qA���A�jA��yA�;dA�|�A���A��-A��jA��A�1A�z�A�A�ZA��FA��A��+A��A���A�$�A�z�A�ƨA���A��;A��A�t�A��+A��9A��HA�7LA�VA�ZA�M�Ay?}At��Aq��An9XAhAb�HA[S�AU�AM�
AI33AF1AB^5A>��A<��A;?}A;�A:ffA7&�A5�A3�mA2�uA1�FA/p�A-��A,ȴA-\)A.�DA.�9A.9XA-�A-;dA,  A*�RA)�;A)+A(z�A(ZA(�jA)33A)\)A)S�A)/A(�RA(JA';dA&��A&ffA%��A%A%
=A$r�A$5?A#C�A#�A#A"�A"�+A"1'A!�^A �/A ^5A��A��At�A�yA��A��A�\A{A  A�A��A�-A�hA+A�9A�hA�+A�AA��A�A��A5?AA;dA�A~�A�#A�hA�A�jA��A�uA-A�^At�A/A�A��AI�A�-A;dA�/A��A=qAJA��A�A�A��A��A��AƨA�A�!AbNA$�A�A�hA�A\)AC�A+AA�A�Ar�A(�A�^Al�AO�A"�A
ĜA
bNA	ƨA	oAȴA�AffA �A�mA�A~�AZA{A�FA�AVA�jA=qA��At�AS�A�AjA-AI�A��AA�^A�hAVA z�@��
@�|�@��@���@��m@��y@�5?@��-@��@� �@��F@�=q@�G�@���@�1@�ƨ@���@�@�@��@�@���@��@�K�@���@�@�1'@���@�F@�@�C�@�"�@�@��@�J@�Ĝ@��@��@�|�@�+@��@�/@䛦@�  @�dZ@�33@��y@�E�@��@�@���@��D@�Q�@�ƨ@�C�@���@�~�@�V@�J@݉7@���@�z�@��m@�
=@�v�@�$�@ف@���@أ�@�Q�@��@�;d@��@���@�Z@ԋD@�Ĝ@Ԭ@ԣ�@�ƨ@�|�@�S�@�33@�n�@с@�?}@��`@�  @�K�@ΰ!@Χ�@�~�@��@ͩ�@�?}@�&�@�V@�Ĝ@̃@�A�@��@�
=@ʏ\@�^5@�~�@�@��y@ʸR@�~�@�-@���@Ɂ@�X@�Ĝ@�j@�|�@�$�@š�@�X@�V@���@��@��@ģ�@ě�@�bN@�9X@�  @þw@�dZ@�o@�@��@�/@�p�@��@�Q�@�b@��
@�K�@��R@�M�@��T@��^@�p�@��/@��9@���@�1'@��
@�"�@�
=@���@�J@�`B@���@�A�@��w@�;d@��H@���@�^5@���@�p�@��@�I�@�1@��;@��P@�S�@�"�@��@�n�@�@��#@��@�`B@�7L@�z�@�(�@��m@��@��@�V@��T@��7@�Z@��;@�K�@��H@��@��R@�E�@���@��@�Ĝ@��@�j@�9X@�b@�ƨ@��P@�;d@���@���@��#@��-@�7L@��@�bN@���@�33@��@�V@�5?@��@��@��@�Ĝ@� �@��
@��w@���@�S�@��y@���@�~�@�^5@�{@���@�V@�j@�1'@�ƨ@�;d@���@��+@�ff@�$�@��#@���@�7L@��@� �@��@�33@��@��\@�ff@�E�@��@��T@��^@�x�@�&�@�Ĝ@��@�r�@�1'@�b@�  @��;@��@�\)@�;d@��@��y@�~�@�^5@��@�O�@���@��u@�Z@�1@��F@�S�@�"�@��y@��!@���@�~�@�v�@�^5@�=q@���@���@�`B@��@��`@��u@��@�bN@� �@���@��y@�n�@�5?@��T@���@���@��@�`B@�/@���@���@��j@��9@�r�@�9X@�(�@��@���@��@�S�@�o@���@��+@��@��-@��7@�&�@��@�Z@���@�t�@�S�@�33@��@���@�~�@�M�@�{@���@���@�`B@�/@���@��u@�Z@�A�@�1'@��@l�@+@�@~�R@~@|�@|j@{�m@{S�@z��@zn�@z�@y�^@yx�@x��@xr�@w�@wl�@vȴ@vv�@v5?@u`B@tI�@s�
@s33@r��@r=q@q�@q��@qx�@pĜ@pb@o
=@n��@nE�@m��@mO�@l�@l9X@k�F@k��@k"�@j��@jn�@jJ@i�#@i��@i%@hr�@g�@g|�@g\)@f��@fff@f@e�h@d�@d9X@c��@c33@b��@b^5@b�@a��@a7L@`Ĝ@`Q�@_�w@_+@^�R@^��@^E�@^{@]`B@\�@\Z@\9X@[�
@[�@[t�@[C�@Z�!@Z=q@Y�#@XĜ@X  @W��@W|�@Wl�@Wl�@W;d@V��@V��@Vff@V$�@U��@U�@T�j@TI�@S�F@S@R��@Q�^@Q%@P��@Pr�@P1'@O|�@O+@Nv�@M�@MV@L��@Lz�@L(�@Kƨ@K��@K@J�\@I�@I��@IX@H�`@H  @Gl�@G�@F�@F�+@FE�@E�-@E?}@D��@D�@Dz�@Dj@DZ@D(�@Cƨ@C�@C33@B�H@B��@Bn�@B-@A�7@A�@@�`@@��@@�@@Q�@?�@>�y@>$�@=��@=��@=p�@=�@<�/@<�@<�D@<z�@<j@<�@;o@:�\@:~�@:^5@:-@:�@:J@9�@9�#@9��@9��@8Ĝ@81'@7�@7|�@6��@6ȴ@6��@6��@6ff@6$�@5�T@5@5O�@5�@5V@4�@4�j@4�D@4j@3ƨ@3C�@2�@2�!@2n�@1��@1�^@1�7@17L@1�@1%@0��@0�u@0Q�@/�;@/�P@/\)@.��@.��@.ff@.E�@.E�@.5?@.{@-@-�h@-p�@-?}@-V@,�@,�@,�D@,j@,Z@,I�@,(�@,1@+��@+C�@+33@*�H@*n�@)��@)��@)X@)&�@)�@)�@)%@)%@(��@(�`@(�`@(��@(r�@(  @(b@(  @'�;@'�w@'�@'l�@'K�@';d@'+@&�@&��@&V@&{@%��@%`B@%/@$��@$�@$�@$j@$�@#�F@#��@#dZ@#"�@"�H@"�\@"-@"�@!�#@!�7@ �9@ Q�@  �@   @��@�w@�@�P@�P@�P@l�@\)@\)@K�@K�@;d@
=@�@�R@5?@{@�@�-@�h@�@�@�j@Z@��@�
@�F@�@dZ@"�@��@��@^5@�@�@�^@�^@�^@�^@��@�7@7L@�`@r�@ �@  @�@�;@��@�@K�@�@�+@V@�T@�h@p�@/@V@��@�j@�D@z�@(�@��@�m@�m@�m@�
@��@�@t�@dZ@33@o@@�@�@�@��@n�@�@��@�7@7L@%@��@�`@��@��@�u@�@Q�@  @�w@�@�P@\)@+@�y@�R@ff@E�@$�@$�@{@�@�T@�-@�h@`B@?}@/@�@V@��@�/@��@(�@�m@�
@ƨ@�F@�@dZ@S�@
�@
��@
��@
�\G�O�AߓuAߕ�AߑhAߑhAߓuAߕ�Aߕ�AߑhAߕ�Aߗ�Aߕ�AߑhAߓuAߙ�Aߕ�Aߙ�Aߗ�Aߙ�Aߛ�Aߙ�Aߕ�Aߗ�Aߙ�AߋDA߃A߁A߅A߇+A߃A߃A߁A߃A߃A߃A߅AߍPAߏ\A߇+A߉7AߍPAߑhA߉7A߅A߇+AߋDAߛ�Aߙ�A�~�A߁A߅A߃A�|�A�z�A߃A߇+A߇+A߅A߃A߅A�|�A�~�A�p�A�x�A�~�AߋDA߇+A�~�A�~�A�hsA�l�A߃A�`BA�;dA�-A��A�/A�K�A�=qA�=qA�?}A�A�A�E�A�5?A�A���A���A���A���A���A���A���A���A��A��A��A��A��HA���A���A���A�ȴA�ĜA�ƨA޼jA޲-Aޛ�A�ffA�9XA�+A��A��TAݲ-Aݡ�A݅A�`BA�=qA�JA��A��yA��
A�Aܺ^AܸRAܶFAܬAܥ�Aܡ�Aܡ�Aܗ�A܉7A�t�A�ffA�`BA�`BA�^5A�VA�C�A�;dA�-A�1A��A���A۬Aۏ\A� �A�;dA���A�p�A���AؓuA�7LA�
=A�A���A��`A�x�A��TA���A���A�"�AՅA�?}A���A�t�A�1A���A�M�AѼjA��
A�ZA��TAϧ�A�5?AΑhA�A͉7A�v�A�33A�A�bA�JA�JA�1A�A��yA̼jA̝�A�ffA�A�A�(�A�oA���A��#A���Aˣ�A˃A�jA�XA�C�A�1'A��A���A��TAʺ^A�O�A� �A�1A��A��`A��HA��;A��;A��
Aɡ�A���Aȴ9Aȗ�AȍPAȇ+A�~�A�r�A�G�A��A��yAǩ�Aǉ7A�S�A�5?A�+A��A�bA���A��A��TA��;A�ĜAƕ�AƏ\AƃA�n�A��Aţ�A�~�A�l�A�S�A�9XA�"�A��A��A�JA�A���A��Ać+A�Aá�A�|�A�hsA�{A��A���A�ȴA¾wA®A�A�jA�;dA�/A�+A�$�A�"�A��A��A��A�{A�oA�bA�{A�{A�{A�oA�bA�bA�bA�
=A�1A��A��mA���A��!A���A���A���A��uA��hA��DA��A�r�A�ZA�?}A�33A�/A��A�JA�  A���A��A��A��mA��/A��
A���A���A���A���A�ȴA�A��wA��RA��-A��A���A���A��\A��A�t�A�`BA�O�A�A�A�+A�bA��A�ĜA���A��A�jA�K�A�/A�{A���A��;A�ƨA��A���A��A�~�A�|�A�r�A�ffA�^5A�E�A�7LA�&�A��A�JA��HA���A�A��9A���A��\A�z�A�dZA�S�A�I�A�;dA�+A��A��A�oA�
=A�  A���A���A���A��A��A��A��TA��#A���A���A��wA��FA���A���A��A�p�A�VA�;dA��A���A�1'A��mA���A���A��jA���A�v�A�`BA�M�A�A�A�9XA�/A�-A�"�A��A��A�JA��A��wA�|�A� �A��
A���A��\A�|�A�hsA�VA�G�A�?}A�+A�&�A�{A�oA�1A���A��A��mA��HA��/A���A��!A���A��PA�|�A�l�A�^5A�A�A�7LA�1'A�+A� �A���A���A�t�A�"�A��A��/A��jA��A�O�A�/A��mA���A��A���A��uA��A�l�A�VA�=qA���A���A�t�A�=qA�(�A��A�bA�A���A��A���A��FA���A�p�A�JA�ĜA���A�`BA�&�A��9A�dZA�=qA�oA��mA��9A�|�A�C�A�bA��A�ĜA��^A��!A���A���A�|�A�Q�A�;dA�/A�&�A��A�oA�A��A�ȴA��!A�ZA��A���A��A��;A��\A�ffA���A�|�A�I�A�%A���A��A��-A�v�A� �A��RA��+A�ZA�S�A�M�A�9XA���A���A�`BA�5?A� �A�1A��A���A�O�A�A���A���A��PA�|�A�p�A�ffA�XA�K�A�?}A�+A�%A��TA��A�dZA�-A���A��jA���A��uA��7A�~�A�v�A�XA��A���A�{A�A��+A�JA��#A���A��PA�`BA�(�A�  A��;A���A��wA��\A�S�A� �A��A���A�A�t�A�7LA�"�A�oA�A��A���A�E�A�"�A��A�ȴA��PA�bA���A���A��+A��A�VA��`A��A��A��FA�t�A�;dA�+A�(�A�&�A��A�1A��A���A���A�t�A�`BA�O�A�E�A�-A���A��A��#A��uA��A���A���A�z�A�/A�%A��HA��-A�^5A��;A�ffA�1'A���A�ĜA��A�I�A��A��A�A�r�A���A�x�A��TA�1'A�t�A�A���A���A�5?A��A��RA�r�A�M�A��A���A���A��A�v�A�M�A�5?A�&�A�VA�ƨA�jA�+A�ƨA�bNA�
=A�/A��+A��RA��#A�9XA���A�%A�7LA~��A}�TA}&�A|bAz�`Azn�Ay�FAx��Aw�TAwt�Av��AvbNAu33At�RAtjAt�AtbAtbAt  As��As;dAr�Aqt�Aq%Ap�!Ap�DApbNAp  Ao�Ao&�An�jAmoAk��Ak�Aj1'Ai\)Ah�RAg�mAg`BAf�jAe��Ae�Ad��Ac�TAc`BAbv�Ab1'Aa�#Aa
=A^�RA]�TA]XA\=qA[hsAZr�AX�DAW��AWx�AV��AU�AU�AU�AT�jATbNASp�AQ�APANQ�AL��AL�uALI�AK��AK��AK;dAJ�AI�#AIAH^5AG�AGx�AGXAF��AE�AEƨAEAE�^AE�-AE�hAD�yAD$�AC"�AB��AA��A@$�A?�;A?��A?S�A>��A>�+A>ZA>9XA>JA=�
A=��A=&�A<�/A<ZA<�A;�#A;��A;p�A;`BA;O�A;C�A;�A;oA;VA;VA;
=A;%A;
=A;oA;/A;O�A;hsA;XA;"�A:��A9��A8��A8��A8 �A7��A7dZA6�A6�uA6v�A6M�A6 �A5��A5�7A5XA5�A5%A5%A4�/A4v�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                     ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�?B��B�?BŢB�B��BƨB��BŢB��B�B�mB�9B�B��B��B�[B�-B��B�3B�B��B	
�B	B	�B	!�B	8�B	M�B	U2B	[WB	Z�B	cTB	s�B	|�B	}�B	��B	��B	�wB	�;B	�ZB
VB
B
$@B
4�B
5tB
_�B
��B
��B
��B
��B
�pB
�BB�B�B*�B=<B^5Bl�B�%B�7B��B��B�	B�'B��B��B��B��B��B�Bu�Bj�BR�BC�B2�B�B�B
�B
��B
��B
�YB
X�B
;�B
B	ԕB	��B	��B	~�B	n�B	TaB	>wB	,qB	~B��B��B��B��B�B�B�MB�B	�B	7B	eB	'�B	2�B	-�B	xB	'�B	=qB	YB	y>B	�%B	�@B	��B	�qB	�!B	�qB	�4B	��B	�B	�B	�FB	�B	�-B	��B	ĜB	�0B	ӏB	چB	��B	�B	�B	�iB	�B	�DB	�B
B

rB
�B
�B
B
�B
 B
�B
.B
\B
\B
VB
�B
�B
eB
'�B
,�B
.}B
4nB
4�B
5tB
9�B
:*B
;0B
5�B
0UB
*eB
-CB
,�B
-CB
.IB
)�B
&LB
$�B
&�B
+6B
&LB
&�B
#�B
$B
#:B
"hB
$�B
$B
%zB
'B
'B
'RB
(�B
)�B
(�B
'�B
+B
1[B
1�B
2�B
6FB
:�B
;0B
9$B
?B
OBB
IRB
F?B
GB
HB
JXB
K)B
JXB
J�B
K)B
K�B
L0B
K�B
K)B
MB
K�B
K�B
L0B
K�B
K�B
K^B
I�B
HKB
HB
F�B
G�B
I�B
HB
GzB
F�B
E�B
C�B
DgB
C-B
A�B
AUB
?B
=<B
;dB
8�B
8RB
9$B
8RB
6�B
:�B
:^B
9XB
9�B
:�B
9XB
8�B
5tB
7B
7LB
3�B
,�B
*�B
(�B
(�B
'�B
&�B
&B
#�B
!�B
 �B
!�B
 �B
�B
&�B
*�B
)_B
(�B
($B
)�B
)*B
%B
"4B
#:B
#�B
#�B
$tB
'�B
($B
(XB
($B
'�B
&�B
$�B
$tB
#�B
#�B
!�B
!bB
 \B
 \B
�B
�B
 �B
VB
�B
 �B
�B
!B
�B
~B
�B
xB
�B
�B
�B
�B
~B
�B
xB
�B
�B
�B
eB
�B
�B
�B
7B
+B
MB
oB
hB
�B
�B
�B
�B
B
uB
oB
�B
4B
.B
�B
.B
�B
�B
~B
~B
~B
JB
~B
PB
B
B
"B
\B
.B
hB
�B
�B
�B
�B
B
$B
$B
B
B
B
�B
�B
FB
B
�B
PB
~B
�B
PB
B
�B
oB
hB
�B
�B
@B
B
�B
oB
�B
hB
B
�B
uB
B
{B
�B
{B
�B
MB
�B
�B
MB
�B
$B
�B
�B
�B
�B
�B
�B
�B
YB
�B
_B
�B
�B
�B
_B
_B
�B
kB
�B
�B
B
1B
B
B
�B
�B
CB
CB
B
B
CB
B
�B
B
=B
	B
qB
�B
�B
qB
CB
IB
CB
IB
CB
�B
B
�B
B
~B
B
�B
IB
B
B
�B
xB
CB
~B
�B
xB
CB
�B
�B
�B
OB
�B
!B
�B
�B
 'B
 �B
!�B
!�B
"�B
#B
#:B
#:B
%zB
&�B
&�B
&�B
&LB
&LB
&LB
'B
&�B
'RB
'RB
'B
&�B
&�B
'RB
'RB
'�B
'�B
'�B
'�B
($B
(�B
(�B
)_B
*0B
)�B
*0B
*0B
*�B
*�B
*�B
+kB
+�B
+kB
,=B
,B
,=B
,=B
,qB
,�B
,�B
,�B
,�B
-CB
-�B
-CB
.�B
.�B
/�B
/�B
/�B
0!B
0�B
0�B
1'B
1[B
1�B
1[B
1[B
1'B
1'B
1'B
1'B
1�B
1�B
1'B
1'B
1�B
2�B
3�B
3�B
4�B
5tB
4�B
4nB
5?B
5B
4�B
4�B
5B
6B
6FB
6B
6B
6�B
7�B
7�B
7B
7�B
7LB
7B
7�B
7�B
7�B
7�B
7�B
7�B
7B
7�B
6zB
6B
7�B
7�B
8B
7�B
8�B
8B
7�B
8B
8B
8�B
8�B
8�B
8�B
9XB
9�B
9�B
9�B
9�B
9�B
:�B
:�B
:*B
:�B
;0B
;�B
<B
<jB
=<B
=qB
=qB
=�B
=�B
>B
>BB
>wB
>�B
>�B
?�B
?�B
?�B
@�B
A B
A B
A�B
A�B
A�B
A�B
A�B
A�B
B'B
B�B
C�B
C�B
C�B
D�B
EB
EB
F?B
FB
FtB
GB
GB
GzB
H�B
H�B
H�B
IB
IRB
IRB
I�B
IRB
J#B
I�B
I�B
J�B
J�B
K)B
J�B
K�B
K�B
K�B
K�B
L�B
L�B
M6B
MjB
MjB
M�B
M�B
M�B
NB
NB
OB
N�B
N�B
N�B
OvB
OBB
OBB
OvB
O�B
O�B
PB
P�B
Q�B
QNB
QNB
QNB
QB
QNB
Q�B
Q�B
Q�B
Q�B
R�B
S&B
S&B
S�B
T,B
U2B
T�B
V�B
VmB
VmB
VmB
V�B
W
B
W
B
W�B
X�B
YB
YKB
YKB
YB
Y�B
YKB
ZQB
Z�B
Z�B
Z�B
Z�B
[WB
[�B
\)B
\)B
\]B
\]B
\�B
\�B
]dB
]�B
]�B
^B
]�B
]�B
^jB
_;B
_pB
_�B
`BB
_�B
`B
_�B
`�B
`�B
`vB
`BB
`�B
`vB
aHB
a�B
b�B
bNB
b�B
b�B
c B
c B
c�B
c�B
c�B
c�B
c�B
e,B
e�B
f2B
ffB
f�B
gB
gB
g8B
g8B
gB
gB
h
B
h
B
g�B
h
B
hsB
h�B
h�B
h�B
iDB
iyB
iyB
iyB
jB
jB
jB
jB
jKB
jKB
jB
j�B
j�B
j�B
kQB
kQB
k�B
lWB
l�B
l�B
l�B
l�B
m�B
ncB
m�B
ncB
ncB
n�B
o5B
oiB
o�B
o�B
o�B
o�B
o�B
pB
p;B
p;B
poB
poB
p�B
qB
qAB
qvB
qAB
qvB
qvB
qvB
rB
rGB
rB
rGB
r�B
sB
sMB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u%B
u%B
u%B
u%B
u�B
u�B
u�B
u�B
v+B
v`B
v`B
v�B
v`B
v�B
w2B
wfB
xB
xlB
xlB
x�B
x�B
y	B
yrB
yrB
y�B
z�B
{B
{�B
{�B
{�B
|B
|PB
|PB
|PB
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}"B
}"B
|�B
}�B
}VB
}VB
}�B
}�B
}�B
~(B
~]B
~�B
~�B
~�B
.B
~�B
~�B
.B
�B
�B
�B
�4B
��B
��B
��B
��B
�iB
��B
��B
�B
�;B
��B
��B
��B
��B
��B
��B
��B
�AB
��B
��B
��B
��B
�{B
��B
��B
��B
��B
�B
�MB
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�SB
�SB
��B
��B
��B
��B
��B
�SB
�%B
��B
��B
��B
��B
�_B
�+B
�_B
�_B
�_B
��B
��B
��B
�1B
�fB
�fB
�1B
�fB
�B
�B
�B
��B
�lB
�	B
�lB
�	B
��B
�=B
�rB
��B
��B
�rB
�=B
��B
�B
��B
�rB
�DB
��B
��B
�~B
�B
��B
�JB
��B
��B
��B
�B
��B
��B
��BŢB��B�tB�?B�BĜB�?B��B�tB�B�?B��B�?B�B�B�B��B�B�3B�mB�B�B�9BʌB��B�tB�B��BǮB��B�EB��BĜBŢB��B�9B��B��BŢB�?B�-BŢB�B�tB�gB��BŢB��BŢBĜB��B�B�B�aB�3B�3B�9BŢB��B�EB��BȴB��B�aB�'B��B�zB��B�wB�3B�?B��B�wB� B�9B��B�B�gB�aB�aB�'B��B��B��B��B��B��B��B��B� B��B�'B�-B�[B��B��B��B�zB��B�3B�tB��B�gBƨB�B�dB��B��B��B�KB��B�B��B�B�B�"B	fB	
�B	B	~B	�B	4B	bB	bB	B	@B	�B	�B	uB	�B	�B	7B	�B	eB	�B	�B	!�B	!-B	!bB	&LB	$@B	(XB	xB	 \B	0!B	E�B	D�B	IB	GB	N�B	U�B	OB	J�B	IB	L�B	ZB	R�B	L0B	I�B	ffB	O�B	RTB	U�B	R B	dZB	cTB	\�B	ZQB	m�B	R�B	X�B	U2B	^�B	n/B	a�B	Y�B	\�B	sMB	m�B	s�B	s�B	s�B	v`B	w�B	cB	|�B	}�B	}�B	zxB	y>B	y�B	|PB	}�B	~�B	�SB	��B	�YB	�B	��B	�%B	��B	�rB	��B	�4B	��B	��B	��B	��B	��B	�wB	�}B	�B	�B	�pB	��B	�DB	�WB	�B	��B	�B	��B	�TB	�DB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
~B
"hB
 �B
"�B
%�B
4�B
<6B
1'B
1�B
4B
6zB
6B
49B
4nB
5tB
6zB
6B
>BB
I�B
c B
n�B
n/B
o B
��B
� B
�4B
��B
��B
��B
�B
��B
�"B
��B
��B
��B
��B
��B
��B
��B
��B
�:B
��B
�hB
�hB
��B
�uB
�B
�B
�YB
��B
�B
��B
�VB
�\B
��B
��B
��B
��B
�!B
�B
��B
��B
��B
�_B
�wB
�B
�UB
�B
��B
��B
��B
�qB
��B
�HB
��B
��B
��B
�[B
��B
��B
B
�aB
�gB
�9B
�tB
�B
�B
ɺB
�)B
̘B
ΥB
ӏB
ԕB
�mB
�yB
چB
��B
�B
�&B
�B
�B
�"B
�/B
�B
�GB
��B
��B
�	B
�DB
��B
��B
�	B
�B
��B
�B
�.B �B�BBAB�BYB�B�B	�B�B�B�BbB(B�B4B�B:BBuB�B�B�B�B�B�BB�B�B�BB$BSB$B�B�B�B�B�B�B,=B7�B6�B0UB/�B2aB5?B5�B6�B7LB8B6�B9$B6zB9$B5?B6�B7�B:^B@BF�BS&BT�BW
BR BT�BW�BX�B[WB[�B_;B_pBbBc Bd�Bg�Bi�BkBj�BjKBm�Bm�Bo5BncBo5BncBm]BsBj�Bi�Bi�Bi�By�B�PB��B�B��B�B�GB�YB��B��B��B��B��B��B��B�AB�MB�MB��B��B��B�~B��B��B��B��B�DB��B�lB�VB�JB��B��B�IB��B�hB�oB��B��B�:B�B�B��B�qB�xB��B�B��B��B�eB�eB��B�+B��B��B��B�B�eB��B��B�_B�7B��B��B��B�~B�zB�=B��B�tB��B�}B�	B��B�qB��B��B��B��B�kB��B�B�B�B��B�.B��B��B�7B��B�_B�+B�B�!B�VB��B�uB��B�VB��B��B�7B��B��B�_B��B�JB�B�(B�uB��B�VB��B�1B�%B��B��B�4B��B��B��B��BxlB.B�Bx�B�uBxB|�B{Bs�Bs�BoiBp;BqBq�Bk�Bk�Bs�BsBm�B\�BV�BW
BT�BS�B]�BQ�BMjBNpBIBR�BK^BF�B?�B<�B9XBDgB@OBHKB3�B9$B,=B+6B!�B�B�B�B \BIBxB�B�B4BFB:B4B&LBhB�B�B �B
��B
�B
��B
��B
��B
�ZB
�B
�B
��B
�mB
�}B
��B
ȴB
�KB
�UB
�B
��B
�zB
��B
�B
��B
�}B
�CB
��B
�B
�@B
z�B
w�B
i�B
iB
`BB
XyB
Y�B
PB
G�B
FB
IB
D3B
;dB
8�B
6B
?B
33B
,qB
0UB
�B
%zB
%�B
PB
�B
 4B	��B	��B	�B	�vB	�B	�B	�hB	�^B	�nB	��B	��B	�FB	��B	�{B	��B	��B	��B	��B	��B	��B	�uB	��B	��B	��B	��B	�B	�B	z�B	v�B	o�B	o�B	r�B	n�B	h�B	n�B	��B	e�B	]dB	]dB	[#B	T,B	U�B	JXB	NpB	OvB	B�B	D�B	>�B	A�B	<�B	2aB	0�B	B�B	UgB	-CB	(XB	9$B	&�B	-CB	%FB	�B	�B	�B	 B	_B	�B	�B��B	�B	�B	
�B	MB��B�B� B�)B��B�B��B��B�	B��B�B�B��B	JB�|B�"B�"B�B�B��B�B��B�B��B	B��B��B�B�vB�)B�B� B��B�cB�/B�B�8B�B�B��B�B�ZB�B�vB�|B��B��B��B�ZB��B�8B��B��B�lB�2B��B	�B	_B	1B	
rB	'�B	�B	kB	B		B	�B	 \B	B	�B	�B	SB	�B	xB	7B	=B	�B	�B	B	"hG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                     B�5B��B�OBŽB�@B�TB�B��BōB��B�BŕB�rB�?BńB��B�EB��B��B�7B��B�B	&B	}B	AB	*�B	EnB	V�B	dLB	ngB	h�B	hcB	v�B	��B	�iB	� B	��B	��B	�NB	�UB
�B
B
*%B
7UB
<(B
e�B
��B
�7B
��B
�mB
֫B
��B�B�BXB3�BF�Bc�B|�B�B�vB��B�FB�HB�kB�B�XB��B�-B�UB��B��Bt�B]�BO�B8�B&YB3B
�NB
�B
��B
��B
bTB
NlB
<aB	��B	�B	�IB	��B	�CB	flB	V_B	@�B	&B	�B�MB�JB	�B��B��B�IB�CB	^B	 B	3B	,�B	6B	5_B	#!B	*vB	;�B	U�B	x�B	��B	��B	�'B	��B	�pB	�cB	��B	�B	��B	��B	��B	��B	�AB	�uB	�uB	ΌB	�gB	�"B	�^B	��B	�B	��B	�B	�nB	��B
�B

�B
LB
�B
gB
�B
�B
�B
�B
�B
/B
9B
�B
�B
�B
)TB
,�B
.�B
5TB
4�B
6B
;0B
<B
?B
9�B
2eB
+.B
-�B
-�B
/UB
0�B
+�B
(B
%ZB
)"B
-mB
'�B
(�B
$fB
$wB
#�B
#�B
&:B
%B
&}B
(B
(UB
(�B
*�B
+�B
)�B
(]B
,�B
1�B
2B
3B
6}B
;�B
<�B
7DB
?B
Q�B
JaB
GcB
G�B
H�B
K|B
KuB
J�B
J�B
K�B
L!B
LpB
LB
L�B
NB
M'B
L�B
L�B
LVB
MWB
L�B
K�B
J�B
I#B
GdB
H�B
J�B
IB
J/B
H�B
FIB
D�B
E�B
DB
C�B
B�B
@�B
?\B
<B
9�B
:B
:�B
9"B
6�B
;�B
;(B
9�B
:�B
<nB
;gB
:�B
6RB
8>B
9�B
6�B
.�B
,%B
)�B
*)B
)_B
'�B
(�B
%�B
"�B
"	B
"'B
!�B
 )B
'wB
+�B
)�B
*|B
)�B
+fB
,B
'B
"�B
#�B
$SB
$;B
$�B
'�B
(rB
)B
)]B
)�B
'�B
%GB
%hB
%�B
%+B
"�B
"�B
!�B
!qB
�B
 �B
!�B
 B
�B
!�B
 �B
�B
�B
�B
�B
B
:B
vB
�B
�B
uB
B
B
�B
�B
B
�B
�B
?B
iB
�B
�B
B
�B
 B
RB
�B
+B
CB
�B
�B
�B
IB
�B
�B
`B
�B
B
�B
�B
�B
=B
(B
CB
�B
ZB
�B
�B
�B
�B
B
yB
OB
RB

B
?B
�B
�B
�B
�B
�B
�B
�B
B
�B
B
MB
B
<B
�B
�B
LB
�B
�B
AB
�B
�B
�B
�B
B
�B
�B
RB
NB
!B
xB
	B
gB
�B
�B
B
JB
�B
�B
�B
wB
�B
�B
}B
�B
B
�B
�B
�B
B
iB
�B
�B
zB
�B
�B

B
�B
_B
:B
�B
�B
�B
vB
BB
:B
B

B
pB
�B
�B
~B
.B
�B
�B
�B
TB
B
�B
LB
qB
CB
^B
B
eB
/B
�B
�B
]B
B
UB
1B
�B
qB
�B
XB
)B
�B
�B
/B
�B
,B
�B
�B
B
-B
B
 8B
 DB
 B
 �B
!�B
"�B
#+B
#^B
#=B
#{B
#�B
&7B
&�B
'B
'B
&�B
'/B
'wB
(NB
'9B
('B
(UB
'�B
'JB
'B
'�B
'�B
(8B
(_B
(�B
)B
)BB
)�B
)�B
*B
*�B
*DB
*�B
*�B
*�B
+B
+xB
, B
+�B
+�B
,�B
,MB
,fB
,�B
,�B
-?B
-"B
-$B
-GB
.B
.5B
.0B
/�B
/�B
/�B
/�B
0�B
0�B
1uB
1]B
1�B
1�B
1�B
1�B
1sB
1^B
1rB
1�B
1�B
2IB
2}B
1�B
1�B
2(B
3JB
4^B
4�B
6,B
6`B
5B
5B
5�B
5%B
5B
5B
5pB
6wB
6�B
6EB
64B
7cB
7�B
7�B
7�B
8B
7�B
7~B
8B
8�B
83B
8�B
8�B
8B
7�B
8�B
6�B
7B
8�B
83B
8kB
8�B
8�B
8wB
8QB
8�B
8�B
8�B
9=B
9TB
9`B
:)B
:4B
:(B
9�B
:B
:�B
;	B
:�B
:�B
;~B
<<B
<YB
<�B
<�B
=�B
=�B
=�B
>B
>&B
>�B
>�B
>�B
?6B
?�B
@8B
@B
@�B
A�B
A�B
A�B
BB
BB
A�B
BB
BB
BvB
B�B
C�B
DqB
D(B
DtB
E!B
EsB
E�B
F�B
F0B
F�B
GvB
GhB
G�B
H�B
H�B
I&B
I�B
I�B
I�B
I�B
I�B
J�B
JZB
JmB
K,B
K�B
K{B
K�B
L�B
LEB
LDB
LtB
MB
MBB
M�B
M�B
M�B
NBB
N B
N&B
NHB
N�B
O�B
O-B
OB
O=B
O�B
O]B
O�B
PB
PUB
PUB
Q!B
QtB
Q�B
QtB
QbB
QWB
QPB
Q�B
Q�B
Q�B
R8B
RsB
S=B
S�B
S�B
T[B
T�B
U�B
U�B
W�B
V�B
V�B
V�B
W{B
WlB
W�B
X�B
Y[B
Y�B
YtB
Y�B
Y�B
Y�B
Y�B
Z�B
[%B
[B
[B
[B
\1B
\SB
\~B
\pB
\�B
\�B
][B
]lB
]�B
]�B
]�B
^B
]�B
^B
^�B
_�B
_�B
`/B
`�B
`B
`ZB
`~B
aB
`�B
`�B
`�B
`�B
aB
b
B
bpB
b�B
b�B
b�B
c
B
cfB
cWB
c�B
c�B
c�B
c�B
d�B
e�B
e�B
fWB
f�B
f�B
gB
g(B
gMB
gPB
gFB
g�B
h�B
hUB
hLB
h�B
h�B
h�B
h�B
iB
i�B
i�B
i�B
i�B
jHB
j-B
j<B
jKB
jB
jzB
j�B
khB
k>B
k1B
k�B
k�B
lB
l�B
l�B
mB
l�B
l�B
n
B
n�B
nqB
n�B
n�B
o0B
o�B
o�B
o�B
o�B
o�B
o�B
p"B
p;B
pbB
pqB
p�B
p�B
qB
q4B
qcB
q�B
qXB
q�B
q�B
q�B
rtB
raB
rjB
r�B
s#B
smB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
tB
t�B
t�B
t�B
t�B
t�B
u
B
u4B
uGB
u:B
u?B
uzB
u�B
u�B
u�B
v4B
vpB
v�B
v�B
v�B
v�B
wB
w�B
w�B
x+B
x�B
x�B
x�B
x�B
yhB
y�B
y�B
z:B
{yB
{�B
{�B
|B
|B
|1B
|eB
|qB
|UB
|�B
|�B
|�B
|�B
}B
|�B
}B
}"B
}UB
}PB
}nB
}�B
}~B
}�B
}�B
}�B
~&B
~�B
~�B
&B
B
~�B
_B
!B
AB
�B
�B
�B
�B
�gB
��B
��B
��B
��B
��B
��B
��B
�]B
��B
��B
��B
��B
��B
��B
�B
�?B
��B
��B
�B
�RB
��B
��B
��B
�B
��B
�(B
�MB
�gB
��B
�B
��B
��B
��B
��B
�B
�AB
�4B
�6B
��B
�wB
��B
��B
��B
��B
��B
��B
�zB
�JB
�B
�B
�+B
�rB
�AB
�uB
��B
�tB
��B
��B
�B
�rB
�|B
��B
�hB
��B
�DB
�;B
�VB
��B
��B
�B
��B
�'B
��B
�nB
��B
�B
��B
��B
�TB
��B
�#B
�B
��B
��B
��B
��B
��B
�,B
�B
�nB
��B
�B
�B
�?B
��B
��G�O�BŢB��B�tB�?B�BĜB�?B��B�tB�B�?B��B�?B�B�B�B��B�B�3B�mB�B�B�9BʌB��B�tB�B��BǮB��B�EB��BĜBŢB��B�9B��B��BŢB�?B�-BŢB�B�tB�gB��BŢB��BŢBĜB��B�B�B�aB�3B�3B�9BŢB��B�EB��BȴB��B�aB�'B��B�zB��B�wB�3B�?B��B�wB� B�9B��B�B�gB�aB�aB�'B��B��B��B��B��B��B��B��B� B��B�'B�-B�[B��B��B��B�zB��B�3B�tB��B�gBƨB�B�dB��B��B��B�KB��B�B��B�B�B�"B	fB	
�B	B	~B	�B	4B	bB	bB	B	@B	�B	�B	uB	�B	�B	7B	�B	eB	�B	�B	!�B	!-B	!bB	&LB	$@B	(XB	xB	 \B	0!B	E�B	D�B	IB	GB	N�B	U�B	OB	J�B	IB	L�B	ZB	R�B	L0B	I�B	ffB	O�B	RTB	U�B	R B	dZB	cTB	\�B	ZQB	m�B	R�B	X�B	U2B	^�B	n/B	a�B	Y�B	\�B	sMB	m�B	s�B	s�B	s�B	v`B	w�B	cB	|�B	}�B	}�B	zxB	y>B	y�B	|PB	}�B	~�B	�SB	��B	�YB	�B	��B	�%B	��B	�rB	��B	�4B	��B	��B	��B	��B	��B	�wB	�}B	�B	�B	�pB	��B	�DB	�WB	�B	��B	�B	��B	�TB	�DB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
~B
"hB
 �B
"�B
%�B
4�B
<6B
1'B
1�B
4B
6zB
6B
49B
4nB
5tB
6zB
6B
>BB
I�B
c B
n�B
n/B
o B
��B
� B
�4B
��B
��B
��B
�B
��B
�"B
��B
��B
��B
��B
��B
��B
��B
��B
�:B
��B
�hB
�hB
��B
�uB
�B
�B
�YB
��B
�B
��B
�VB
�\B
��B
��B
��B
��B
�!B
�B
��B
��B
��B
�_B
�wB
�B
�UB
�B
��B
��B
��B
�qB
��B
�HB
��B
��B
��B
�[B
��B
��B
B
�aB
�gB
�9B
�tB
�B
�B
ɺB
�)B
̘B
ΥB
ӏB
ԕB
�mB
�yB
چB
��B
�B
�&B
�B
�B
�"B
�/B
�B
�GB
��B
��B
�	B
�DB
��B
��B
�	B
�B
��B
�B
�.B �B�BBAB�BYB�B�B	�B�B�B�BbB(B�B4B�B:BBuB�B�B�B�B�B�BB�B�B�BB$BSB$B�B�B�B�B�B�B,=B7�B6�B0UB/�B2aB5?B5�B6�B7LB8B6�B9$B6zB9$B5?B6�B7�B:^B@BF�BS&BT�BW
BR BT�BW�BX�B[WB[�B_;B_pBbBc Bd�Bg�Bi�BkBj�BjKBm�Bm�Bo5BncBo5BncBm]BsBj�Bi�Bi�Bi�By�B�PB��B�B��B�B�GB�YB��B��B��B��B��B��B��B�AB�MB�MB��B��B��B�~B��B��B��B��B�DB��B�lB�VB�JB��B��B�IB��B�hB�oB��B��B�:B�B�B��B�qB�xB��B�B��B��B�eB�eB��B�+B��B��B��B�B�eB��B��B�_B�7B��B��B��B�~B�zB�=B��B�tB��B�}B�	B��B�qB��B��B��B��B�kB��B�B�B�B��B�.B��B��B�7B��B�_B�+B�B�!B�VB��B�uB��B�VB��B��B�7B��B��B�_B��B�JB�B�(B�uB��B�VB��B�1B�%B��B��B�4B��B��B��B��BxlB.B�Bx�B�uBxB|�B{Bs�Bs�BoiBp;BqBq�Bk�Bk�Bs�BsBm�B\�BV�BW
BT�BS�B]�BQ�BMjBNpBIBR�BK^BF�B?�B<�B9XBDgB@OBHKB3�B9$B,=B+6B!�B�B�B�B \BIBxB�B�B4BFB:B4B&LBhB�B�B �B
��B
�B
��B
��B
��B
�ZB
�B
�B
��B
�mB
�}B
��B
ȴB
�KB
�UB
�B
��B
�zB
��B
�B
��B
�}B
�CB
��B
�B
�@B
z�B
w�B
i�B
iB
`BB
XyB
Y�B
PB
G�B
FB
IB
D3B
;dB
8�B
6B
?B
33B
,qB
0UB
�B
%zB
%�B
PB
�B
 4B	��B	��B	�B	�vB	�B	�B	�hB	�^B	�nB	��B	��B	�FB	��B	�{B	��B	��B	��B	��B	��B	��B	�uB	��B	��B	��B	��B	�B	�B	z�B	v�B	o�B	o�B	r�B	n�B	h�B	n�B	��B	e�B	]dB	]dB	[#B	T,B	U�B	JXB	NpB	OvB	B�B	D�B	>�B	A�B	<�B	2aB	0�B	B�B	UgB	-CB	(XB	9$B	&�B	-CB	%FB	�B	�B	�B	 B	_B	�B	�B��B	�B	�B	
�B	MB��B�B� B�)B��B�B��B��B�	B��B�B�B��B	JB�|B�"B�"B�B�B��B�B��B�B��B	B��B��B�B�vB�)B�B� B��B�cB�/B�B�8B�B�B��B�B�ZB�B�vB�|B��B��B��B�ZB��B�8B��B��B�lB�2B��B	�B	_B	1B	
rB	'�B	�B	kB	B		B	�B	 \B	B	�B	�B	SB	�B	xB	7B	=B	�B	�B	B	"hG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                     <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<ZF%<#�
<~�N<��<p��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<,� <#�
<��<Oq<T�<|��<#�
<���<rp <;�O<,��<#�
<#�
<T�<1��<?޿<6�<@��<O�<#�
<#�
<d�<Y�}<a�X<�wT<���<*V<��<�<���<��X<)��<`�><��<��*<üv<�3T<��<���<G�T<V<]<Py�<#�
<#�
<#�
<#�
<7q�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261914182023042619141820230426191418202304261914182023042619141820230426191418SI  SI  ARFMARFM                                                                                                                                                2019070408123220190704081232IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019071407025920190714070259QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019071407025920190714070259QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2020010906572820200109065728IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619142120230426191421IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619142120230426191421IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619142120230426191421IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                