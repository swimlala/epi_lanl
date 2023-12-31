CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-05-12T14:00:54Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  7p   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8P   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8X   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8\   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  8`   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  8�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  9,   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    9l   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           9t   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    9�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            9�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           9�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           9�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    9�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        ;�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [`   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  c@   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ɠ   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  р   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � `   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 7�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � ?�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � _@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � g    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �\   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �d   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �l   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �t   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �8   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �@   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �H   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20230512140054  20230512140054  5905275 5905275 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO7316                            7316                            2B  2B  AA  SOLO_II                         SOLO_II                         8644                            8644                            SBE602 11Jan18                  SBE602 11Jan18                  853 853 @�($�"4C@�($�"4C11  @�($��� @�($��� @,!9C��@,!9C���c�xB0���c�xB0��11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AB  ?�  @   @@  @�G�@�  @��R@�  A   A  A ��A+�A?\)A^�RA�  A�  A�  A��A��A�  A�  A�  B (�B(�B(�B  B�
B((�B0Q�B8(�B?�
BG�
BO�
BW�
B`(�BhQ�Bp  Bx  B�{B�{B�(�B�(�B�{B��B�  B�{B�(�B�  B��B�  B�  B��B�  B�  B�  B�  B�{B�  B�{B�  B�{B�{B�  B�ffB�33B�B��
B��B�{B��C   C  C  C  C  C
  C  C  C  C  C
=C  C
=C
=C��C  C   C"  C$  C%��C'�C)�HC+�C-��C0  C2  C4
=C6  C8  C9��C;��C>
=C@  CB  CD  CF  CH  CJ  CK��CN
=CP
=CR
=CT
=CV  CX  CZ
=C\  C]��C`
=Cb  Cd  Cf
=Ch
=Cj
=Ck��Cm��Cp
=Cr
=Cs��Cu�Cw�Cy�C{�C~  C�  C�  C�C�  C���C���C�  C�  C�C�C���C���C�  C�  C�  C���C�  C�  C�  C�C�C�C�  C�  C�  C���C�  C�  C�  C�  C�  C�C�C�C�  C���C���C�  C�
=C�
=C�  C���C�  C�C�  C�  C�  C�C�C�C�C�C�C���C���C�C�C�  C�
=C�C�  C���C�  C�
=C�C�  C�  C�  C���C���C���C�  C���C���C�C���C�  C�C�  C�  C���C���C�C�  C�  C�
=C�  C���C�  C�  C�  C�  C���C���C���C�  C�C�  C���C�  C�  C���C�C�  C�  C���C���C�  C�  C�  C�  C�C���C���C���C���C�  C�C�C�C�C�C���C���C���C���C���C���D   D }qD ��D}qD��Dz�D�qD��D�D��D  D� D  D}qD  D��D  D}qD��D	z�D
  D
�D�D��D  D� D�D��D�qD� D  D}qD�qD� D�D� D��D}qD�D� D��D� D�D}qD�qDz�D��D��D�D� D  D� D  D� D  D}qD�qD}qD�D� D  D��D�D��D �D � D!  D!� D"  D"}qD"�qD#z�D#��D$� D$�qD%z�D&  D&� D'  D'�D(�D(��D)  D)� D*D*� D+  D+�D,�D,� D-�D-�D.�D.� D/�D/��D0  D0�D1�D1� D2D2��D3  D3}qD3�qD4��D5  D5� D6  D6}qD7  D7}qD7��D8� D9�D9� D9�qD:� D;�D;� D;�qD<}qD=  D=�D>  D>}qD?  D?��D@D@��D@�qDA}qDB�DB}qDB�qDC� DD�DD��DE  DE}qDE�qDF� DG  DG� DH�DH��DI�DI� DJ  DJ��DK�DK��DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ��DRDR� DR�qDS� DT  DT}qDU  DU� DV�DV��DW  DW� DX  DX� DY  DY��DZ  DZ}qDZ�qD[}qD\  D\��D]  D]� D^  D^� D_  D_� D`  D`��Da  Da}qDa�qDb� Dc�Dc��Dd�Dd��De  Dez�De��Df}qDg  Dg� Dg�qDh� Dh��Di}qDj�Dj� Dk�Dk��Dk�qDlz�Dl�qDm}qDn  Dn��Do  Do}qDp�Dp� Dq  Dq� Dq��Dr}qDs  Ds}qDt  Dt� Du  Du� Dv  Dv��Dw�Dw��Dx�Dx� Dx�qDy� Dy�qDzz�Dz��D{z�D|  D|� D}  D}��D}�qD~� D�D}qD�  D�AHD�� D���D���D�>�D��HD��HD��qD�@ D��HD�� D���D�>�D�~�D��HD�HD�>�D�� D�� D�  D�@ D�� D�� D�  D�@ D��HD�� D���D�>�D�� D��HD�  D�>�D�� D��HD�  D�@ D�� D���D�  D�@ D�� D�� D�  D�B�D��HD�� D��D�AHD�� D���D���D�@ D�� D���D�  D�AHD���D�� D���D�@ D���D��HD���D�AHD�� D�� D���D�>�D�� D��HD�  D�>�D��HD���D��qD�@ D��HD�D�HD�@ D��HD��HD�  D�>�D�}qD���D�  D�>�D�� D�� D�  D�@ D�~�D�� D�  D�@ D�� D�� D�  D�@ D�~�D���D���D�>�D�� D��HD�  D�>�D�� D�� D�  D�AHD�� D�� D�HD�AHD��HD�� D���D�AHD�� D���D�  D�AHD��HD�� D��qD�>�D�� D��HD�HD�@ D�� D���D�  D�B�D���D�� D�  D�@ D�~�D���D���D�>�D�~�D��HD�  D�>�D�~�D�� D�  D�>�D��HD��HD���D�AHD��HD��qD���D�>�D�~�D���D���D�>�D�� D�� D�HD�AHD�~�D���D�HD�@ D�� D���D��qD�@ D�� D��qD��qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�� D�� D�HD�AHD�� D�D�  D�>�D�� D��HD��D�@ D�~�D��HD�  D�@ D�� D�� D�  D�@ D��HD��HD�  D�@ D�� D�� D�  D�AHD��HD�D��D�AHD��HD��HD�  D�@ D�� D���D�  D�@ D�� D��HD�  D�>�D�� D��HD���D�AHD���D��HD���D�=qD�~�D��HD��D�AHD�~�D���D�  D�AHD��HD��HD�HD�AHD�� D�� D�  D�@ D�� D�� D�  D�AHD��HD�� D���D�>�D D¾�D�HD�AHDÀ D�� D���D�>�DāHD�� D��qD�@ DŁHD�� D�  D�@ DƁHD�� D�  D�AHDǁHD��HD�  D�@ DȁHDȾ�D���D�=qD�~�D�� D�HD�@ Dʀ D��HD�HD�@ Dˀ D�� D�  D�@ D�~�D̾�D�  D�>�D�~�D��HD�  D�AHD΁HD�� D�  D�@ DρHD��HD�HD�@ DЀ D�� D�  D�@ Dр DѾ�D�  D�@ DҀ D��HD�HD�@ D�}qDӾ�D�  D�AHDԁHD�� D���D�@ DՁHD��HD�  D�>�DցHD�D�HD�@ D�~�D׾�D���D�=qD�~�D�� D���D�>�DفHD�� D���D�@ DځHD�� D�  D�AHDۀ D�� D�  D�>�D܁HD�D�HD�>�D�}qDݾ�D���D�>�Dހ D�� D���D�>�D�~�D�� D���D�=qD�~�D�� D�  D�>�D�HD�� D�  D�@ D₏D��HD�  D�AHD�HD��HD��D�@ D� D�� D���D�>�D� D�� D�  D�AHD�HD澸D���D�>�D� D�� D���D�AHD肏D��HD�  D�@ D� D�� D�HD�AHD�HD꾸D���D�>�D�}qD뾸D�  D�@ D�HD��HD�  D�=qD�~�D�� D���D�>�D� D�� D�  D�@ DD��HD�  D�@ D�~�D�D���D�AHD�HD�D���D�>�D� D��HD���D�=qD�~�D�� D�  D�@ D�HD��HD���D�>�D��HD��HD�  D�>�D�� D��HD�  D�>�D�� D���D��qD�@ D�� D���D���D�>�D��HD�� D���D�1�D�xR?#�
?8Q�?u?�z�?�33?��@   @
=q@��@(��@=p�@O\)@^�R@n{@�  @��@�@�(�@�ff@���@��H@\@˅@�
=@�  @���@��@��HA�\A
=A
�HA  A�A��A{A"�\A'
=A,(�A0��A5�A9��A>{AC33AFffAJ�HAP��AU�AY��A^�RAc33Ah��An{Ar�\Aw�A}p�A���A��
A�{A���A��
A�ffA���A�33A�ffA���A��A�A���A��A�{A���A��HA�A���A�33A�A�Q�A��HA�A�Q�A��HA��A�Q�A�33A��AϮAҏ\A�p�A׮A��A���A߮A�=qA�(�A�
=A陚A�z�A�RA���A�A�ffA���A�33A�B Q�B��B�RB�
BG�B�RB�
B��B
=qB�B�B{B\)B��B{B\)Bz�BB
=BQ�B��B�RB  Bp�B�RB�
B ��B"ffB#�B$��B&{B'\)B(��B*{B+\)B,z�B-�B/\)B0��B1B333B4��B6=qB7\)B8��B:=qB;�B<��B>{B?�B@��BB=qBC\)BD��BFffBG�BH��BJ=qBK�BL��BN=qBO\)BP��BR{BS33BTz�BV{BW\)BXz�BY��B[33B\z�B]��B^�HB`Q�Ba��Bb�RBd  Bep�Bf�RBh  Bh��BjffBk�
Bl��Bn{Bo\)Bp��Bq�Bs
=BtQ�Bu��Bv�HBw�
By�BzffB{�
B}�B~{B\)B�ffB�
=B���B�(�B���B��B�(�B��RB�G�B��B���B�G�B�B�ffB�
=B�B�Q�B��HB��B�=qB���B�G�B��B���B�33B��B�Q�B���B���B�(�B���B�G�B��B��\B�33B��B�=qB��HB���B�(�B���B�G�B��B��\B��B��B�Q�B���B�B�Q�B���B���B�Q�B�
=B���B�(�B��HB���B�(�B��RB�\)B�{B���B�\)B��B�z�B�33B��
B�ffB���B��B�=qB��HB�p�B��B��\B�33B��B�z�B�
=B���B�Q�B�
=B��B�=qB���B���B�Q�B�
=B��B�Q�B���B�B�z�B�33B��
B�z�B�33B��B��RB�G�B��B���B�\)B�(�B���B�p�B�(�B��HBŮB�ffB�
=BǮB�ffB�33B��Bʣ�B�G�B��Ḅ�B�p�B�(�B��HBυB�(�B��HBљ�B�Q�B�
=BӮB�Q�B�
=B�B�z�B��B׮B�Q�B�
=Bٙ�B�Q�B�
=Bۙ�B�(�B���B�p�B�{B���B�p�B�{B��B�G�B�  B�\B�
=B�B�  B�\B���B�G�B�B�B�  B�(�B�ffB�z�B�\B�z�B�z�B��B�RB���B��HB��HB��HB��HB��HB��HB�
=B��B�33B�33B��B��B�33B�33B�\)B�\)B�\)B�G�B�33B�G�B�\)B�\)B�\)B�G�B�33B�G�B�G�B�G�B�G�B��B�
=B��B�33B��B��B���B��HB��HB��HB��HB��HB��HB��HB�RB��B��B��B��B��B��B�\B�z�B�ffB�ffB�ffB�z�B�z�B�z�B�z�B�ffB�ffB�ffB�ffB�z�B�\B�z�B�z�B�z�B�z�B�z�B�\B��B��B�RB�RB�RB��B�RB��HB��HB���B�
=B���B�
=B�
=B��B�33B�G�B�p�B�B癚B�B�B�B��
B��B�{B�(�B�Q�B�Q�B�ffB�z�B�\B��B�RB��HB���B��B�33B�G�B�\)B�p�B�B陚B�B��B�  B�(�B�ffB�z�B��B���B���B��B�33B�\)B�B�B��
B�  B�=qB�z�B�RB��HB�33B�\)B홚B��
B�{B�ffB�\B���B�
=B�G�B�B��
B�{B�ffB�RB�
=B�G�B�p�B��
B�{B�ffB�RB��B�p�B��
B�=qB�\B��HB�G�B���B�  B�Q�B��RB��B��B��B�ffB���B�33B���B��B�Q�B��RB�
=B��B��B�Q�B��RB��B���B�{B�z�B��HB�G�B�C {C G�C z�C �C �HC{CG�Cz�C�C�HC{CQ�C�C�RC�C(�C\)C��C��C
=C=qCp�C�C�HC{CG�Cp�C��C�
C
=CG�Cp�C�C�HC�CQ�C�CC��C�CQ�C�C�RC�HC	�C	G�C	�C	�RC	�HC
�C
Q�C
�C
�RC
�C�C\)C�\CC��C(�C\)C�C�RC�C{C=qCp�C��C��C��C(�C\)C��CC��C(�CQ�C�C�RC�HC{C=qCp�C��C��C��C�CQ�Cz�C��C��C  C(�C\)C�C�RC�C{CG�Cz�C��C�
C  C(�C\)C�\C�RC�C{CG�Cz�C��C��C��C(�C\)C�C�RC�HC
=C=qCffC��C��C  C(�CffC�\CC�C�CQ�Cz�C�C�HC{C=qCp�C��C�
C
=C=qCffC��C�
C
=C=qCz�C��C�HC{CQ�C�CC  C33CffC��C�HC{CQ�C�CC��C 33C p�C ��C �HC!{C!Q�C!�C!C"  C"33C"p�C"��C"�
C#{C#G�C#�C#C#��C$=qC$p�C$�C$�C%(�C%\)C%��C%�HC&�C&ffC&��C&�C'(�C'p�C'�C'�C((�C(p�C(�RC)  C)=qC)�C)��C*
=C*G�C*�\C*�
C+{C+\)C+��C+�C,33C,z�C,C-
=C-\)C-��C-�C.33C.z�C.C/
=C/Q�C/��C/�HC033C0p�C0C1
=C1Q�C1��C1�C233C2�\C2�
C3(�C3p�C3C4
=C4\)C4��C4��C5G�C5��C5�HC6(�C6p�C6�RC7{C7\)C7�C8  C8Q�C8��C8��C9G�C9��C9�HC:33C:z�C:��C;�C;p�C;�
C<(�C<z�C<�
C=�C=z�C=C>{C>ffC>C?�C?p�C?��C@�C@p�C@�RCA
=CAffCACB{CBp�CBCC�CCz�CC��CD{CDffCD�RCE
=CEffCECF�CFp�CFCG{CGffCG�RCH
=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111444441114441414444114444144444114444444444444444444444144444441144444114144411411414111111144111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                             ?�  @   @@  @�G�@�  @��R@�  A   A  A ��A+�A?\)A^�RA�  A�  A�  A��A��A�  A�  A�  B (�B(�B(�B  B�
B((�B0Q�B8(�B?�
BG�
BO�
BW�
B`(�BhQ�Bp  Bx  B�{B�{B�(�B�(�B�{B��B�  B�{B�(�B�  B��B�  B�  B��B�  B�  B�  B�  B�{B�  B�{B�  B�{B�{B�  B�ffB�33B�B��
B��B�{B��C   C  C  C  C  C
  C  C  C  C  C
=C  C
=C
=C��C  C   C"  C$  C%��C'�C)�HC+�C-��C0  C2  C4
=C6  C8  C9��C;��C>
=C@  CB  CD  CF  CH  CJ  CK��CN
=CP
=CR
=CT
=CV  CX  CZ
=C\  C]��C`
=Cb  Cd  Cf
=Ch
=Cj
=Ck��Cm��Cp
=Cr
=Cs��Cu�Cw�Cy�C{�C~  C�  C�  C�C�  C���C���C�  C�  C�C�C���C���C�  C�  C�  C���C�  C�  C�  C�C�C�C�  C�  C�  C���C�  C�  C�  C�  C�  C�C�C�C�  C���C���C�  C�
=C�
=C�  C���C�  C�C�  C�  C�  C�C�C�C�C�C�C���C���C�C�C�  C�
=C�C�  C���C�  C�
=C�C�  C�  C�  C���C���C���C�  C���C���C�C���C�  C�C�  C�  C���C���C�C�  C�  C�
=C�  C���C�  C�  C�  C�  C���C���C���C�  C�C�  C���C�  C�  C���C�C�  C�  C���C���C�  C�  C�  C�  C�C���C���C���C���C�  C�C�C�C�C�C���C���C���C���C���C���D   D }qD ��D}qD��Dz�D�qD��D�D��D  D� D  D}qD  D��D  D}qD��D	z�D
  D
�D�D��D  D� D�D��D�qD� D  D}qD�qD� D�D� D��D}qD�D� D��D� D�D}qD�qDz�D��D��D�D� D  D� D  D� D  D}qD�qD}qD�D� D  D��D�D��D �D � D!  D!� D"  D"}qD"�qD#z�D#��D$� D$�qD%z�D&  D&� D'  D'�D(�D(��D)  D)� D*D*� D+  D+�D,�D,� D-�D-�D.�D.� D/�D/��D0  D0�D1�D1� D2D2��D3  D3}qD3�qD4��D5  D5� D6  D6}qD7  D7}qD7��D8� D9�D9� D9�qD:� D;�D;� D;�qD<}qD=  D=�D>  D>}qD?  D?��D@D@��D@�qDA}qDB�DB}qDB�qDC� DD�DD��DE  DE}qDE�qDF� DG  DG� DH�DH��DI�DI� DJ  DJ��DK�DK��DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ��DRDR� DR�qDS� DT  DT}qDU  DU� DV�DV��DW  DW� DX  DX� DY  DY��DZ  DZ}qDZ�qD[}qD\  D\��D]  D]� D^  D^� D_  D_� D`  D`��Da  Da}qDa�qDb� Dc�Dc��Dd�Dd��De  Dez�De��Df}qDg  Dg� Dg�qDh� Dh��Di}qDj�Dj� Dk�Dk��Dk�qDlz�Dl�qDm}qDn  Dn��Do  Do}qDp�Dp� Dq  Dq� Dq��Dr}qDs  Ds}qDt  Dt� Du  Du� Dv  Dv��Dw�Dw��Dx�Dx� Dx�qDy� Dy�qDzz�Dz��D{z�D|  D|� D}  D}��D}�qD~� D�D}qD�  D�AHD�� D���D���D�>�D��HD��HD��qD�@ D��HD�� D���D�>�D�~�D��HD�HD�>�D�� D�� D�  D�@ D�� D�� D�  D�@ D��HD�� D���D�>�D�� D��HD�  D�>�D�� D��HD�  D�@ D�� D���D�  D�@ D�� D�� D�  D�B�D��HD�� D��D�AHD�� D���D���D�@ D�� D���D�  D�AHD���D�� D���D�@ D���D��HD���D�AHD�� D�� D���D�>�D�� D��HD�  D�>�D��HD���D��qD�@ D��HD�D�HD�@ D��HD��HD�  D�>�D�}qD���D�  D�>�D�� D�� D�  D�@ D�~�D�� D�  D�@ D�� D�� D�  D�@ D�~�D���D���D�>�D�� D��HD�  D�>�D�� D�� D�  D�AHD�� D�� D�HD�AHD��HD�� D���D�AHD�� D���D�  D�AHD��HD�� D��qD�>�D�� D��HD�HD�@ D�� D���D�  D�B�D���D�� D�  D�@ D�~�D���D���D�>�D�~�D��HD�  D�>�D�~�D�� D�  D�>�D��HD��HD���D�AHD��HD��qD���D�>�D�~�D���D���D�>�D�� D�� D�HD�AHD�~�D���D�HD�@ D�� D���D��qD�@ D�� D��qD��qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�� D�� D�HD�AHD�� D�D�  D�>�D�� D��HD��D�@ D�~�D��HD�  D�@ D�� D�� D�  D�@ D��HD��HD�  D�@ D�� D�� D�  D�AHD��HD�D��D�AHD��HD��HD�  D�@ D�� D���D�  D�@ D�� D��HD�  D�>�D�� D��HD���D�AHD���D��HD���D�=qD�~�D��HD��D�AHD�~�D���D�  D�AHD��HD��HD�HD�AHD�� D�� D�  D�@ D�� D�� D�  D�AHD��HD�� D���D�>�D D¾�D�HD�AHDÀ D�� D���D�>�DāHD�� D��qD�@ DŁHD�� D�  D�@ DƁHD�� D�  D�AHDǁHD��HD�  D�@ DȁHDȾ�D���D�=qD�~�D�� D�HD�@ Dʀ D��HD�HD�@ Dˀ D�� D�  D�@ D�~�D̾�D�  D�>�D�~�D��HD�  D�AHD΁HD�� D�  D�@ DρHD��HD�HD�@ DЀ D�� D�  D�@ Dр DѾ�D�  D�@ DҀ D��HD�HD�@ D�}qDӾ�D�  D�AHDԁHD�� D���D�@ DՁHD��HD�  D�>�DցHD�D�HD�@ D�~�D׾�D���D�=qD�~�D�� D���D�>�DفHD�� D���D�@ DځHD�� D�  D�AHDۀ D�� D�  D�>�D܁HD�D�HD�>�D�}qDݾ�D���D�>�Dހ D�� D���D�>�D�~�D�� D���D�=qD�~�D�� D�  D�>�D�HD�� D�  D�@ D₏D��HD�  D�AHD�HD��HD��D�@ D� D�� D���D�>�D� D�� D�  D�AHD�HD澸D���D�>�D� D�� D���D�AHD肏D��HD�  D�@ D� D�� D�HD�AHD�HD꾸D���D�>�D�}qD뾸D�  D�@ D�HD��HD�  D�=qD�~�D�� D���D�>�D� D�� D�  D�@ DD��HD�  D�@ D�~�D�D���D�AHD�HD�D���D�>�D� D��HD���D�=qD�~�D�� D�  D�@ D�HD��HD���D�>�D��HD��HD�  D�>�D�� D��HD�  D�>�D�� D���D��qD�@ D�� D���D���D�>�D��HD�� D���D�1�D�xR?#�
?8Q�?u?�z�?�33?��@   @
=q@��@(��@=p�@O\)@^�R@n{@�  @��@�@�(�@�ff@���@��H@\@˅@�
=@�  @���@��@��HA�\A
=A
�HA  A�A��A{A"�\A'
=A,(�A0��A5�A9��A>{AC33AFffAJ�HAP��AU�AY��A^�RAc33Ah��An{Ar�\Aw�A}p�A���A��
A�{A���A��
A�ffA���A�33A�ffA���A��A�A���A��A�{A���A��HA�A���A�33A�A�Q�A��HA�A�Q�A��HA��A�Q�A�33A��AϮAҏ\A�p�A׮A��A���A߮A�=qA�(�A�
=A陚A�z�A�RA���A�A�ffA���A�33A�B Q�B��B�RB�
BG�B�RB�
B��B
=qB�B�B{B\)B��B{B\)Bz�BB
=BQ�B��B�RB  Bp�B�RB�
B ��B"ffB#�B$��B&{B'\)B(��B*{B+\)B,z�B-�B/\)B0��B1B333B4��B6=qB7\)B8��B:=qB;�B<��B>{B?�B@��BB=qBC\)BD��BFffBG�BH��BJ=qBK�BL��BN=qBO\)BP��BR{BS33BTz�BV{BW\)BXz�BY��B[33B\z�B]��B^�HB`Q�Ba��Bb�RBd  Bep�Bf�RBh  Bh��BjffBk�
Bl��Bn{Bo\)Bp��Bq�Bs
=BtQ�Bu��Bv�HBw�
By�BzffB{�
B}�B~{B\)B�ffB�
=B���B�(�B���B��B�(�B��RB�G�B��B���B�G�B�B�ffB�
=B�B�Q�B��HB��B�=qB���B�G�B��B���B�33B��B�Q�B���B���B�(�B���B�G�B��B��\B�33B��B�=qB��HB���B�(�B���B�G�B��B��\B��B��B�Q�B���B�B�Q�B���B���B�Q�B�
=B���B�(�B��HB���B�(�B��RB�\)B�{B���B�\)B��B�z�B�33B��
B�ffB���B��B�=qB��HB�p�B��B��\B�33B��B�z�B�
=B���B�Q�B�
=B��B�=qB���B���B�Q�B�
=B��B�Q�B���B�B�z�B�33B��
B�z�B�33B��B��RB�G�B��B���B�\)B�(�B���B�p�B�(�B��HBŮB�ffB�
=BǮB�ffB�33B��Bʣ�B�G�B��Ḅ�B�p�B�(�B��HBυB�(�B��HBљ�B�Q�B�
=BӮB�Q�B�
=B�B�z�B��B׮B�Q�B�
=Bٙ�B�Q�B�
=Bۙ�B�(�B���B�p�B�{B���B�p�B�{B��B�G�B�  B�\B�
=B�B�  B�\B���B�G�B�B�B�  B�(�B�ffB�z�B�\B�z�B�z�B��B�RB���B��HB��HB��HB��HB��HB��HB�
=B��B�33B�33B��B��B�33B�33B�\)B�\)B�\)B�G�B�33B�G�B�\)B�\)B�\)B�G�B�33B�G�B�G�B�G�B�G�B��B�
=B��B�33B��B��B���B��HB��HB��HB��HB��HB��HB��HB�RB��B��B��B��B��B��B�\B�z�B�ffB�ffB�ffB�z�B�z�B�z�B�z�B�ffB�ffB�ffB�ffB�z�B�\B�z�B�z�B�z�B�z�B�z�B�\B��B��B�RB�RB�RB��B�RB��HB��HB���B�
=B���B�
=B�
=B��B�33B�G�B�p�B�B癚B�B�B�B��
B��B�{B�(�B�Q�B�Q�B�ffB�z�B�\B��B�RB��HB���B��B�33B�G�B�\)B�p�B�B陚B�B��B�  B�(�B�ffB�z�B��B���B���B��B�33B�\)B�B�B��
B�  B�=qB�z�B�RB��HB�33B�\)B홚B��
B�{B�ffB�\B���B�
=B�G�B�B��
B�{B�ffB�RB�
=B�G�B�p�B��
B�{B�ffB�RB��B�p�B��
B�=qB�\B��HB�G�B���B�  B�Q�B��RB��B��B��B�ffB���B�33B���B��B�Q�B��RB�
=B��B��B�Q�B��RB��B���B�{B�z�B��HB�G�B�C {C G�C z�C �C �HC{CG�Cz�C�C�HC{CQ�C�C�RC�C(�C\)C��C��C
=C=qCp�C�C�HC{CG�Cp�C��C�
C
=CG�Cp�C�C�HC�CQ�C�CC��C�CQ�C�C�RC�HC	�C	G�C	�C	�RC	�HC
�C
Q�C
�C
�RC
�C�C\)C�\CC��C(�C\)C�C�RC�C{C=qCp�C��C��C��C(�C\)C��CC��C(�CQ�C�C�RC�HC{C=qCp�C��C��C��C�CQ�Cz�C��C��C  C(�C\)C�C�RC�C{CG�Cz�C��C�
C  C(�C\)C�\C�RC�C{CG�Cz�C��C��C��C(�C\)C�C�RC�HC
=C=qCffC��C��C  C(�CffC�\CC�C�CQ�Cz�C�C�HC{C=qCp�C��C�
C
=C=qCffC��C�
C
=C=qCz�C��C�HC{CQ�C�CC  C33CffC��C�HC{CQ�C�CC��C 33C p�C ��C �HC!{C!Q�C!�C!C"  C"33C"p�C"��C"�
C#{C#G�C#�C#C#��C$=qC$p�C$�C$�C%(�C%\)C%��C%�HC&�C&ffC&��C&�C'(�C'p�C'�C'�C((�C(p�C(�RC)  C)=qC)�C)��C*
=C*G�C*�\C*�
C+{C+\)C+��C+�C,33C,z�C,C-
=C-\)C-��C-�C.33C.z�C.C/
=C/Q�C/��C/�HC033C0p�C0C1
=C1Q�C1��C1�C233C2�\C2�
C3(�C3p�C3C4
=C4\)C4��C4��C5G�C5��C5�HC6(�C6p�C6�RC7{C7\)C7�C8  C8Q�C8��C8��C9G�C9��C9�HC:33C:z�C:��C;�C;p�C;�
C<(�C<z�C<�
C=�C=z�C=C>{C>ffC>C?�C?p�C?��C@�C@p�C@�RCA
=CAffCACB{CBp�CBCC�CCz�CC��CD{CDffCD�RCE
=CEffCECF�CFp�CFCG{CGffCG�RCH
=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111444441114441414444114444144444114444444444444444444444144444441144444114144411411414111111144111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                             G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�/A�/A�/A�/A�-A�/A�33A�33A�33A�33A�33A�5?A�33A�5?A�5?A�?}A�=qA�?}A�A�A�C�A�C�A�E�A�G�A�G�A�G�A�I�A�I�A�?}A�5?A�5?A�33A�;dA�9XA�5?A�33A�7LA�A�A�=qA�9XA�;dA�7LA�-A�/A�+A��#A�%A��
AЬAБhA��A�ZA�{A�~�A�r�A�G�A��`A�ƨA���AˮA�r�A�bNA���A�1A�z�A�ĜA��DA�I�A�ƨA�~�A�A�A�{A��
A�~�A�C�A��RA�I�A�bNA�n�A���A�C�A��A���A�\)A��PA���A�7LA�r�A�%A��FA�=qA��A~1'A{`BAy�FAv��Aq��Ak"�Ah�DAd�`A_\)A\�`A\=qA["�AY�AX{AV�HAU��AS��AQS�AOl�AM"�AJffAE�FABbNAA�^AA?}A@ĜA@�A<M�A7��A6ĜA5�FA4�uA3x�A2�HA1�mA1oA1C�A1��A1��A1�#A1��A1�-A1C�A1
=A0�/A0��A0ffA/`BA.n�A-A+�wA*�A)��A)7LA(ȴA(bA'��A'�A&VA%hsA$��A$ �A#�PA#G�A"��A!A!\)A!VA bNA��A�PAoAȴAjA�TA��A�7Ap�A�`A�DA{AA
=A�\A�At�A�A�A��A��AM�A�;A��Al�Ap�Ap�A?}A��A �A�A�FA&�A��A�+Ar�AbAA�A�7A�
A7LAVA��A�/A^5A��A��A�AC�A%A��AffA=qA1'A�A��At�A?}A
�yA
I�A	�wA	��A	�7A	G�A��A~�A^5AE�A �A��A�A?}A��A�\AjAI�A=qA�A33A�yA��A��AjAA�A�TAS�A�A
=A��A�HA��A�A  A�A�wAp�A/A ȴA n�@��F@���@�5?@��@�7L@�Ĝ@�z�@�A�@��m@�o@�V@��@��h@���@��@�dZ@��@�ff@��@���@�G�@�j@�A�@� �@���@�ƨ@�+@�@�hs@�@� �@�@�"�@@��^@�?}@�V@��`@���@�@�r�@�(�@���@�-@�9@�Q�@��@�K�@��@�X@�r�@��
@�@�33@�\@��#@�X@���@ߥ�@�ȴ@އ+@�V@��@���@�z�@�I�@۾w@�K�@�
=@���@�V@ف@�G�@���@� �@��m@�dZ@���@�V@�@���@Չ7@��@�A�@Ӿw@��@җ�@��@�%@�A�@�C�@ΰ!@��T@��@���@��`@�j@��@˝�@�C�@���@�n�@�E�@��@ɩ�@�@���@�`B@��@ț�@�z�@�j@��;@ǝ�@�;d@Ɨ�@�n�@�$�@ũ�@�?}@��`@ă@� �@þw@�o@�@��@��T@��^@��7@�&�@�Z@��@�@��\@�M�@��#@�hs@��@���@��`@��@�z�@�bN@�1'@��P@�+@�@�ȴ@�=q@��#@���@�7L@�r�@��
@�K�@�"�@�
=@��!@��@���@�hs@�G�@�O�@�%@�j@�9X@� �@���@�S�@���@�~�@�5?@���@�Ĝ@��D@�Q�@��P@�@���@��R@�V@��#@�p�@��@�1@��
@�t�@�@�^5@���@��-@�hs@��u@� �@���@���@�dZ@��@��!@���@�&�@���@�Ĝ@��9@�r�@�1@���@�\)@��y@���@�n�@��T@��h@�%@��/@��@�j@�(�@��w@�"�@��H@��H@��@���@��#@��7@�x�@�p�@�G�@�%@�Ĝ@�z�@��@��w@��P@�;d@��@��!@�~�@�$�@��-@�`B@���@��@��/@���@��@�Q�@�1'@�(�@�b@���@�@���@�~�@��@��^@�O�@�/@��`@���@�Q�@�9X@�(�@���@�ƨ@��@���@�l�@�dZ@�\)@�33@�@�ȴ@��!@�ff@���@�O�@���@��@���@��
@���@�S�@���@���@��R@��R@���@��\@��+@�ff@�$�@��@�x�@���@�1'@��
@���@��@�S�@�33@�o@��@��@���@���@�^5@�-@���@���@�x�@�/@���@��@��D@�A�@��m@��@�t�@�;d@�o@���@�=q@��@��@��@��@���@�A�@���@���@���@��@�S�@�@�ȴ@���@�v�@��@�@���@��7@�X@�G�@���@��@�;@~��@~�+@~5?@~@}@}�h@}`B@}�@|�@|9X@|�@{��@{��@{"�@zn�@y�@yX@x�`@x�@w�@w��@w�@vE�@u�@u�@u��@u/@t�D@t(�@s�m@s��@s@r�!@r�\@rn�@r^5@r-@q�^@q&�@p �@oK�@n��@n{@m@m/@l��@lz�@lZ@l(�@k�m@k�F@kdZ@k33@k33@j��@jn�@j=q@i��@i7L@i%@h��@hr�@gl�@g
=@f��@fȴ@fV@e��@e�-@e/@d�@dj@d1@cƨ@cS�@b�!@bn�@b�@a�@a�#@a�^@aG�@`Q�@_�;@_�@^��@^@]�@]/@\��@\z�@\Z@\(�@[�
@[��@[S�@Z��@Z�\@Z=q@Y��@Y7L@XbN@W
=@V�y@V�@V�+@V5?@V@U@U�@T�@TI�@T1@S�
@St�@SC�@S"�@R�@RM�@Q�@P��@PĜ@P��@PQ�@P  @O�w@O|�@O
=@N��@N�+@N$�@M�-@M�h@M�@Mp�@MO�@M?}@L��@L�D@K��@K�@K33@J�@JM�@I�@I��@I�7@I7L@HĜ@Hr�@H1'@H  @G�@G\)@F�R@Fff@FV@FE�@F$�@E�h@D��@D��@D�j@DZ@D(�@C�F@C@B��@B�!@B��@B��@B�\@B^5@A��@A�7@A7L@@Ĝ@@Q�@@b@@  @?�@?\)@>�@>5?@=��@=�@=?}@<�/@<I�@;��@;�@;dZ@;S�@;C�@;@9�@9hs@9G�@9�@8�9@8bN@8Q�@8bN@8bN@8bN@8r�@8bN@7�;@7|�@7K�@6��@6��@65?@5��@5��@5/@4�/@4�j@4�@4z�@4j@4(�@4�@3��@3ƨ@3�@3C�@3@2�!@2�\@2=q@1��@1%@0�@0bN@0A�@01'@0  @/�@/�;@/��@/K�@.�y@.��@.5?@.@-@-�h@-/@,��@,1@+ƨ@+��@+t�@+S�@*�H@*�@)�@)�@)�#@)�^@)x�@)G�@)%@(Ĝ@(�u@(Q�@'�;@'�@'\)@'+@&ȴ@&�R@&��@&�+@&V@&@%��@%�@%V@$��@$��@$�D@$j@$(�@#�m@#�@#33@"�@"��@"M�@"J@!��@!7L@ Ĝ@ r�@ Q�@  �@�w@+@��@�+@V@5?@@�@�T@�h@��@��@�j@�j@�@�D@�D@�D@(�@��@�
@S�@o@�@�H@��@��@~�@n�@^5@^5@-@��@�7@�@�`@�9@bN@b@\)@�@�+@V@{@�T@p�@?}@�@�@I�@�
@��@33@o@�@�H@��@�!@n�@=q@-@J@�@�^@hs@&�@%@��@��@�9@��@�u@�@Q�@�@�;@�w@�@��@�P@l�@\)@\)@�@�y@��@V@5?@@�h@�h@p�@`B@?}@V@��@��@��@�D@�D@�DA�-A�1'A�/A�-A�/A�1'A�/A�-A�+A�/A�1'A�1'A�-A�/A�1'A�1'A�-A�-A�-A�(�A�1'A�-A�1'A�5?A�1'A�/A�1'A�5?A�5?A�1'A�1'A�33A�33A�33A�1'A�33A�7LA�7LA�1'A�1'A�7LA�7LA�33A�1'A�7LA�7LA�1'A�/A�1'A�5?A�7LA�33A�33A�5?A�9XA�5?A�33A�5?A�7LA�33A�1'A�1'A�;dA�;dA�C�A�=qA�=qA�A�A�C�A�=qA�;dA�;dA�?}A�A�A�=qA�=qA�=qA�A�A�A�A�?}A�=qA�?}A�C�A�?}A�=qA�A�A�G�A�C�A�A�A�C�A�E�A�E�A�A�A�A�A�E�A�C�A�C�A�?}A�A�A�E�A�G�A�E�A�C�A�G�A�I�A�G�A�C�A�E�A�G�A�I�A�E�A�E�A�I�A�I�A�I�A�E�A�E�A�I�A�I�A�E�A�E�A�I�A�K�A�G�A�E�A�E�A�I�A�K�A�I�A�G�A�I�A�I�A�K�A�G�A�G�A�K�A�K�A�K�A�G�A�G�A�K�A�G�A�C�A�33A�7LA�;dA�33A�1'A�5?A�5?A�33A�/A�33A�;dA�;dA�1'A�/A�/A�/A�5?A�33A�9XA�;dA�;dA�9XA�5?A�;dA�9XA�9XA�=qA�A�A�?}A�5?A�7LA�5?A�1'A�33A�9XA�7LA�/A�/A�5?A�5?A�33A�1'A�5?A�5?A�33A�33A�5?A�;dA�;dA�5?A�7LA�;dA�=qA�A�A�A�A�G�A�I�A�C�A�=qA�?}A�A�A�E�A�?}A�7LA�/A�7LA�5?A�;dA�;dA�7LA�?}A�=qA�A�A�=qA�=qA�C�A�/A�5?A�;dA�5?A�7LA�33A�9XA�9XA�/A�+A�(�A�+A�/A�-A�+A�(�A�+A�/A�33A�1'A�/A�1'A�;dA�;dA�+A��A�"�A�1A��A��;A��;A���A���A��
A��;A�{A�{A�{A�JA���A��A��A��;A���A�A�Aк^AжFAв-AЩ�AУ�AЩ�AП�AЗ�AЛ�AЩ�AЉ7AЅAЃAЃAЁAЃA�;dAϼjAϧ�Aϲ-Aϴ9AϮAϰ!AϺ^A��A�bNA�{A͛�A�;dA̙�Ả7ÁA̅A�~�A�z�A�|�A�~�A�~�A�z�A�v�A�r�A�jA�jA�ffA�^5A�Q�A�K�A�A�A��A���A��`A��TA��mA��A��
A���A�ƨA�ĜA�ĜA�ĜA�ĜA���A˾wA˼jA˾wA���A���A˺^AˮA˧�A˟�A˛�A˗�Aˉ7A˃A�n�A�`BA�I�A��A�A�ĜA�XA�
=A�z�A��A���Aȝ�A�+A�v�A�;dA��A��;A�x�A��A���A�-A�ȴA�I�A�p�A�ffA�+A���A���A��wA��A���A���A���A��uA��DA��PA��+A��+A��7A��7A��A��A��A��A��A��A��7A��A��A�~�A��DA��hA��uA���A��!A��-A��RA�A��wA���A��#A��/A��HA��A��A���A���A�"�A� �A� �A�+A�K�A�O�A�Q�A�XA�ZA�XA�VA�Q�A�O�A�K�A�C�A�;dA�-A��A�%A��A��HA���A��wA���A��hA�v�A�\)A�C�A�-A��A�A��A���A���A�r�A�;dA��A��A�"�A���A��RA��A�bNA�%A���A���A�hsA�E�A�5?A�&�A�JA��`A�ƨA���A�n�A�VA�G�A�bA���A��A��A���A�ĜA�l�A�ĜA���A���A���A��!A�A��RA�t�A�^5A�E�A�;dA�(�A�"�A��A�JA�A���A���A���A��`A���A��^A���A��DA�^5A��A��A���A��PA�XA��A���A���A��7A�hsA�;dA��/A�XA�ƨA�S�A�A���A��A�S�A�+A��A�  A���A��A�ȴA�Q�A�1'A��A��A��^A��7A�\)A�5?A���A�ĜA���A�t�A�ZA�A�A�bA���A���A��7A�&�A��HA��jA��DA��A��A��DA���A���A���A���A���A���A���A��hA�~�A�l�A�\)A�\)A�\)A�\)A�ZA�S�A�M�A�G�A�=qA�;dA�5?A�/A�-A�+A�$�A��A��A�{A�VA�
=A�%A�A���A���A��A��yA��HA���A���A���A�ĜA��jA��FA��A���A���A��A�t�A�hsA�\)A�XA�XA�7LA��A���A�7LA�JA�JA��A��`A��`A��;A���A���A�ƨA��jA��^A��A��A���A���A���A���A���A�ZA�VA�?}A�=qA�33A�+A� �A�%A��A��yA��A���A���A�v�A�=qA�1A��;A�ȴA��jA��!A��DA�{A��mA���A�XA��A���A��uA�?}A�$�A�A��
A�ĜA��A���A���A��DA�r�A�bNA�ZA�33A��mA�v�A�%A���A�hsA�VA�O�A�A�A�oA�&�A�|�A���A�
=A��A��A��A��A��mA��TA��;A�ȴA��!A���A���A�v�A�1'A��-A�^5A�I�A�=qA�A��A���A�r�A���A��+A�XA�/A�(�A� �A��A�VA���A��A��yA��;A�ȴA��9A���A�jA�A�A�1A���A�bNA��A�oA��A�v�A�?}A�$�A�oA��mA���A�G�A���A�7LA���A��/A���A���A�S�A��A�  A��A���A��!A��+A�jA���A�`BA��9A�r�A�G�A�
=A��A��A���A��A�n�A�C�A���A���A�I�A�ĜA���A��A�+A���A��A�+A��yA��RA���A��7A�t�A�VA�VA�  A��A���A�r�A�33A�M�A�33A~n�A|�+A|=qA|{A|  A{��A{��A{�wA{�hA{?}Az�`Az�9Az��Az=qAy�Ay��Ay��AydZAy�Ax��Ax~�AxE�Aw��Av��Au��At��Asl�As;dAr�yAq�;Aq�Ap��Ao�TAn{AlM�Aj�Aj-Ai��Ai\)Ai&�Ai%Ah�!Ah~�AhjAh-Ag��AgC�Afr�Ae��AdA�Ab�9Aa�PA`��A`=qA_�wA_?}A^�DA]��A]O�A]�A\�A\��A\�jA\��A\��A\r�A\ZA\E�A\(�A[�;A[A[�A[\)A[�AZ�AZ��AZM�AZJAY�;AY�AYhsAY+AX�AX��AX-AW�mAW�FAW�AW\)AW33AV��AV��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                             A�/A�/A�/A�/A�-A�/A�33A�33A�33A�33A�33A�5?A�33A�5?A�5?A�?}A�=qA�?}A�A�A�C�A�C�A�E�A�G�A�G�A�G�A�I�A�I�A�?}A�5?A�5?A�33A�;dA�9XA�5?A�33A�7LA�A�A�=qA�9XA�;dA�7LA�-A�/A�+A��#A�%A��
AЬAБhA��A�ZA�{A�~�A�r�A�G�A��`A�ƨA���AˮA�r�A�bNA���A�1A�z�A�ĜA��DA�I�A�ƨA�~�A�A�A�{A��
A�~�A�C�A��RA�I�A�bNA�n�A���A�C�A��A���A�\)A��PA���A�7LA�r�A�%A��FA�=qA��A~1'A{`BAy�FAv��Aq��Ak"�Ah�DAd�`A_\)A\�`A\=qA["�AY�AX{AV�HAU��AS��AQS�AOl�AM"�AJffAE�FABbNAA�^AA?}A@ĜA@�A<M�A7��A6ĜA5�FA4�uA3x�A2�HA1�mA1oA1C�A1��A1��A1�#A1��A1�-A1C�A1
=A0�/A0��A0ffA/`BA.n�A-A+�wA*�A)��A)7LA(ȴA(bA'��A'�A&VA%hsA$��A$ �A#�PA#G�A"��A!A!\)A!VA bNA��A�PAoAȴAjA�TA��A�7Ap�A�`A�DA{AA
=A�\A�At�A�A�A��A��AM�A�;A��Al�Ap�Ap�A?}A��A �A�A�FA&�A��A�+Ar�AbAA�A�7A�
A7LAVA��A�/A^5A��A��A�AC�A%A��AffA=qA1'A�A��At�A?}A
�yA
I�A	�wA	��A	�7A	G�A��A~�A^5AE�A �A��A�A?}A��A�\AjAI�A=qA�A33A�yA��A��AjAA�A�TAS�A�A
=A��A�HA��A�A  A�A�wAp�A/A ȴA n�@��F@���@�5?@��@�7L@�Ĝ@�z�@�A�@��m@�o@�V@��@��h@���@��@�dZ@��@�ff@��@���@�G�@�j@�A�@� �@���@�ƨ@�+@�@�hs@�@� �@�@�"�@@��^@�?}@�V@��`@���@�@�r�@�(�@���@�-@�9@�Q�@��@�K�@��@�X@�r�@��
@�@�33@�\@��#@�X@���@ߥ�@�ȴ@އ+@�V@��@���@�z�@�I�@۾w@�K�@�
=@���@�V@ف@�G�@���@� �@��m@�dZ@���@�V@�@���@Չ7@��@�A�@Ӿw@��@җ�@��@�%@�A�@�C�@ΰ!@��T@��@���@��`@�j@��@˝�@�C�@���@�n�@�E�@��@ɩ�@�@���@�`B@��@ț�@�z�@�j@��;@ǝ�@�;d@Ɨ�@�n�@�$�@ũ�@�?}@��`@ă@� �@þw@�o@�@��@��T@��^@��7@�&�@�Z@��@�@��\@�M�@��#@�hs@��@���@��`@��@�z�@�bN@�1'@��P@�+@�@�ȴ@�=q@��#@���@�7L@�r�@��
@�K�@�"�@�
=@��!@��@���@�hs@�G�@�O�@�%@�j@�9X@� �@���@�S�@���@�~�@�5?@���@�Ĝ@��D@�Q�@��P@�@���@��R@�V@��#@�p�@��@�1@��
@�t�@�@�^5@���@��-@�hs@��u@� �@���@���@�dZ@��@��!@���@�&�@���@�Ĝ@��9@�r�@�1@���@�\)@��y@���@�n�@��T@��h@�%@��/@��@�j@�(�@��w@�"�@��H@��H@��@���@��#@��7@�x�@�p�@�G�@�%@�Ĝ@�z�@��@��w@��P@�;d@��@��!@�~�@�$�@��-@�`B@���@��@��/@���@��@�Q�@�1'@�(�@�b@���@�@���@�~�@��@��^@�O�@�/@��`@���@�Q�@�9X@�(�@���@�ƨ@��@���@�l�@�dZ@�\)@�33@�@�ȴ@��!@�ff@���@�O�@���@��@���@��
@���@�S�@���@���@��R@��R@���@��\@��+@�ff@�$�@��@�x�@���@�1'@��
@���@��@�S�@�33@�o@��@��@���@���@�^5@�-@���@���@�x�@�/@���@��@��D@�A�@��m@��@�t�@�;d@�o@���@�=q@��@��@��@��@���@�A�@���@���@���@��@�S�@�@�ȴ@���@�v�@��@�@���@��7@�X@�G�@���@��@�;@~��@~�+@~5?@~@}@}�h@}`B@}�@|�@|9X@|�@{��@{��@{"�@zn�@y�@yX@x�`@x�@w�@w��@w�@vE�@u�@u�@u��@u/@t�D@t(�@s�m@s��@s@r�!@r�\@rn�@r^5@r-@q�^@q&�@p �@oK�@n��@n{@m@m/@l��@lz�@lZ@l(�@k�m@k�F@kdZ@k33@k33@j��@jn�@j=q@i��@i7L@i%@h��@hr�@gl�@g
=@f��@fȴ@fV@e��@e�-@e/@d�@dj@d1@cƨ@cS�@b�!@bn�@b�@a�@a�#@a�^@aG�@`Q�@_�;@_�@^��@^@]�@]/@\��@\z�@\Z@\(�@[�
@[��@[S�@Z��@Z�\@Z=q@Y��@Y7L@XbN@W
=@V�y@V�@V�+@V5?@V@U@U�@T�@TI�@T1@S�
@St�@SC�@S"�@R�@RM�@Q�@P��@PĜ@P��@PQ�@P  @O�w@O|�@O
=@N��@N�+@N$�@M�-@M�h@M�@Mp�@MO�@M?}@L��@L�D@K��@K�@K33@J�@JM�@I�@I��@I�7@I7L@HĜ@Hr�@H1'@H  @G�@G\)@F�R@Fff@FV@FE�@F$�@E�h@D��@D��@D�j@DZ@D(�@C�F@C@B��@B�!@B��@B��@B�\@B^5@A��@A�7@A7L@@Ĝ@@Q�@@b@@  @?�@?\)@>�@>5?@=��@=�@=?}@<�/@<I�@;��@;�@;dZ@;S�@;C�@;@9�@9hs@9G�@9�@8�9@8bN@8Q�@8bN@8bN@8bN@8r�@8bN@7�;@7|�@7K�@6��@6��@65?@5��@5��@5/@4�/@4�j@4�@4z�@4j@4(�@4�@3��@3ƨ@3�@3C�@3@2�!@2�\@2=q@1��@1%@0�@0bN@0A�@01'@0  @/�@/�;@/��@/K�@.�y@.��@.5?@.@-@-�h@-/@,��@,1@+ƨ@+��@+t�@+S�@*�H@*�@)�@)�@)�#@)�^@)x�@)G�@)%@(Ĝ@(�u@(Q�@'�;@'�@'\)@'+@&ȴ@&�R@&��@&�+@&V@&@%��@%�@%V@$��@$��@$�D@$j@$(�@#�m@#�@#33@"�@"��@"M�@"J@!��@!7L@ Ĝ@ r�@ Q�@  �@�w@+@��@�+@V@5?@@�@�T@�h@��@��@�j@�j@�@�D@�D@�D@(�@��@�
@S�@o@�@�H@��@��@~�@n�@^5@^5@-@��@�7@�@�`@�9@bN@b@\)@�@�+@V@{@�T@p�@?}@�@�@I�@�
@��@33@o@�@�H@��@�!@n�@=q@-@J@�@�^@hs@&�@%@��@��@�9@��@�u@�@Q�@�@�;@�w@�@��@�P@l�@\)@\)@�@�y@��@V@5?@@�h@�h@p�@`B@?}@V@��@��@��@�D@�D@�DA�-A�1'A�/A�-A�/A�1'A�/A�-A�+A�/A�1'A�1'A�-A�/A�1'A�1'A�-A�-A�-A�(�A�1'A�-A�1'A�5?A�1'A�/A�1'A�5?A�5?A�1'A�1'A�33A�33A�33A�1'A�33A�7LA�7LA�1'A�1'A�7LA�7LA�33A�1'A�7LA�7LA�1'A�/A�1'A�5?A�7LA�33A�33A�5?A�9XA�5?A�33A�5?A�7LA�33A�1'A�1'A�;dA�;dA�C�A�=qA�=qA�A�A�C�A�=qA�;dA�;dA�?}A�A�A�=qA�=qA�=qA�A�A�A�A�?}A�=qA�?}A�C�A�?}A�=qA�A�A�G�A�C�A�A�A�C�A�E�A�E�A�A�A�A�A�E�A�C�A�C�A�?}A�A�A�E�A�G�A�E�A�C�A�G�A�I�A�G�A�C�A�E�A�G�A�I�A�E�A�E�A�I�A�I�A�I�A�E�A�E�A�I�A�I�A�E�A�E�A�I�A�K�A�G�A�E�A�E�A�I�A�K�A�I�A�G�A�I�A�I�A�K�A�G�A�G�A�K�A�K�A�K�A�G�A�G�A�K�A�G�A�C�A�33A�7LA�;dA�33A�1'A�5?A�5?A�33A�/A�33A�;dA�;dA�1'A�/A�/A�/A�5?A�33A�9XA�;dA�;dA�9XA�5?A�;dA�9XA�9XA�=qA�A�A�?}A�5?A�7LA�5?A�1'A�33A�9XA�7LA�/A�/A�5?A�5?A�33A�1'A�5?A�5?A�33A�33A�5?A�;dA�;dA�5?A�7LA�;dA�=qA�A�A�A�A�G�A�I�A�C�A�=qA�?}A�A�A�E�A�?}A�7LA�/A�7LA�5?A�;dA�;dA�7LA�?}A�=qA�A�A�=qA�=qA�C�A�/A�5?A�;dA�5?A�7LA�33A�9XA�9XA�/A�+A�(�A�+A�/A�-A�+A�(�A�+A�/A�33A�1'A�/A�1'A�;dA�;dA�+A��A�"�A�1A��A��;A��;A���A���A��
A��;A�{A�{A�{A�JA���A��A��A��;A���A�A�Aк^AжFAв-AЩ�AУ�AЩ�AП�AЗ�AЛ�AЩ�AЉ7AЅAЃAЃAЁAЃA�;dAϼjAϧ�Aϲ-Aϴ9AϮAϰ!AϺ^A��A�bNA�{A͛�A�;dA̙�Ả7ÁA̅A�~�A�z�A�|�A�~�A�~�A�z�A�v�A�r�A�jA�jA�ffA�^5A�Q�A�K�A�A�A��A���A��`A��TA��mA��A��
A���A�ƨA�ĜA�ĜA�ĜA�ĜA���A˾wA˼jA˾wA���A���A˺^AˮA˧�A˟�A˛�A˗�Aˉ7A˃A�n�A�`BA�I�A��A�A�ĜA�XA�
=A�z�A��A���Aȝ�A�+A�v�A�;dA��A��;A�x�A��A���A�-A�ȴA�I�A�p�A�ffA�+A���A���A��wA��A���A���A���A��uA��DA��PA��+A��+A��7A��7A��A��A��A��A��A��A��7A��A��A�~�A��DA��hA��uA���A��!A��-A��RA�A��wA���A��#A��/A��HA��A��A���A���A�"�A� �A� �A�+A�K�A�O�A�Q�A�XA�ZA�XA�VA�Q�A�O�A�K�A�C�A�;dA�-A��A�%A��A��HA���A��wA���A��hA�v�A�\)A�C�A�-A��A�A��A���A���A�r�A�;dA��A��A�"�A���A��RA��A�bNA�%A���A���A�hsA�E�A�5?A�&�A�JA��`A�ƨA���A�n�A�VA�G�A�bA���A��A��A���A�ĜA�l�A�ĜA���A���A���A��!A�A��RA�t�A�^5A�E�A�;dA�(�A�"�A��A�JA�A���A���A���A��`A���A��^A���A��DA�^5A��A��A���A��PA�XA��A���A���A��7A�hsA�;dA��/A�XA�ƨA�S�A�A���A��A�S�A�+A��A�  A���A��A�ȴA�Q�A�1'A��A��A��^A��7A�\)A�5?A���A�ĜA���A�t�A�ZA�A�A�bA���A���A��7A�&�A��HA��jA��DA��A��A��DA���A���A���A���A���A���A���A��hA�~�A�l�A�\)A�\)A�\)A�\)A�ZA�S�A�M�A�G�A�=qA�;dA�5?A�/A�-A�+A�$�A��A��A�{A�VA�
=A�%A�A���A���A��A��yA��HA���A���A���A�ĜA��jA��FA��A���A���A��A�t�A�hsA�\)A�XA�XA�7LA��A���A�7LA�JA�JA��A��`A��`A��;A���A���A�ƨA��jA��^A��A��A���A���A���A���A���A�ZA�VA�?}A�=qA�33A�+A� �A�%A��A��yA��A���A���A�v�A�=qA�1A��;A�ȴA��jA��!A��DA�{A��mA���A�XA��A���A��uA�?}A�$�A�A��
A�ĜA��A���A���A��DA�r�A�bNA�ZA�33A��mA�v�A�%A���A�hsA�VA�O�A�A�A�oA�&�A�|�A���A�
=A��A��A��A��A��mA��TA��;A�ȴA��!A���A���A�v�A�1'A��-A�^5A�I�A�=qA�A��A���A�r�A���A��+A�XA�/A�(�A� �A��A�VA���A��A��yA��;A�ȴA��9A���A�jA�A�A�1A���A�bNA��A�oA��A�v�A�?}A�$�A�oA��mA���A�G�A���A�7LA���A��/A���A���A�S�A��A�  A��A���A��!A��+A�jA���A�`BA��9A�r�A�G�A�
=A��A��A���A��A�n�A�C�A���A���A�I�A�ĜA���A��A�+A���A��A�+A��yA��RA���A��7A�t�A�VA�VA�  A��A���A�r�A�33A�M�A�33A~n�A|�+A|=qA|{A|  A{��A{��A{�wA{�hA{?}Az�`Az�9Az��Az=qAy�Ay��Ay��AydZAy�Ax��Ax~�AxE�Aw��Av��Au��At��Asl�As;dAr�yAq�;Aq�Ap��Ao�TAn{AlM�Aj�Aj-Ai��Ai\)Ai&�Ai%Ah�!Ah~�AhjAh-Ag��AgC�Afr�Ae��AdA�Ab�9Aa�PA`��A`=qA_�wA_?}A^�DA]��A]O�A]�A\�A\��A\�jA\��A\��A\r�A\ZA\E�A\(�A[�;A[A[�A[\)A[�AZ�AZ��AZM�AZJAY�;AY�AYhsAY+AX�AX��AX-AW�mAW�FAW�AW\)AW33AV��AV��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                             G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	�B	��B	�8B	�8B	��B	�B	�B	�8B	�8B	�8B	��B	�lB	�lB	��B	��B	��B	�B	�8B	�8B	�lB	�8B	��B	�lB	�8B	�lB	�lB	��B	�fB	�B	��B	�B	�%B	�%B	�B	�MB	�B	�%B	�%B	�B	��B	�|B	�B	�B	�B	��B	�B	�sB	�B	�BB	�pB	��B	�HB	��B	�
B	�AB
�B
�B
B
+B
�B
�B
.�B
-B
H�B
��B
�!B
�}B
�
B
�B�BJB!bB?BF�B>�B;�B8B8RB,=B:B
�2B
רB
��B
�dB
�eB
�JB
rB
RTB
LdB
-CB
YB
�B	�B	�ZB	�#B	B	��B	��B	�'B	�"B	��B	��B	cB	}�B	}�B	z�B	{B	��B	��B	� B	i�B	cTB	Y�B	HKB	E9B	IB	J�B	M�B	ZQB	B'B	B�B	GB	HKB	`B	jB	��B	�UB	�TB	�+B	�B
fB
�B
&LB
8�B
A�B
B�B
E�B
E�B
F?B
@B
?B
=�B
?}B
=�B
>�B
B[B
B�B
DgB
C�B
D�B
EB
F?B
GB
HKB
G�B
JXB
I�B
HKB
IRB
K�B
L�B
OBB
OvB
Q�B
R�B
U2B
U�B
T�B
T�B
YKB
Y�B
Y�B
Z�B
]�B
[�B
\]B
]/B
[�B
]/B
_B
`BB
`BB
`B
bB
`�B
`vB
`�B
aB
a|B
b�B
`B
`B
]�B
]/B
\]B
^�B
_pB
_B
]�B
_�B
Y�B
R�B
R�B
RTB
S&B
S�B
TaB
T�B
S�B
T�B
U2B
T�B
S�B
S�B
R�B
U2B
U2B
T�B
T�B
U2B
T�B
S�B
RTB
Q�B
R�B
Q�B
QB
P�B
PB
P}B
P}B
O�B
O�B
O�B
N�B
N�B
NB
M�B
M�B
N�B
K�B
K)B
K�B
K)B
I�B
J�B
JXB
K�B
J�B
K)B
J�B
J�B
K�B
I�B
I�B
J#B
I�B
IRB
H�B
G�B
H�B
FB
FB
F?B
F�B
E�B
C�B
EB
C�B
C-B
@�B
@B
?HB
?HB
=<B
:�B
;�B
:�B
9�B
9�B
;�B
8�B
7�B
7�B
7LB
6zB
8B
6FB
6zB
6FB
5�B
4�B
4�B
49B
4�B
2�B
2�B
2�B
2aB
2-B
1�B
1'B
0!B
/�B
)�B
*0B
($B
*eB
&�B
#�B
#B
 \B
 �B
 'B
�B
�B
xB
B
�B
CB
qB
	B
�B
B
�B
7B
B
B
eB
eB
qB
�B
�B
�B
�B
$B
�B
�B
�B
�B
�B
qB
xB
�B
�B
=B
�B
�B
�B
�B
{B
B
�B
B
�B
SB
B
�B
B
@B
@B
�B
FB
SB
�B
B
+B
7B
�B
_B
YB
YB
kB
�B
B
~B
�B
�B
�B
�B
VB
OB
B
OB
�B
B
	B
�B
�B
7B
kB
xB
=B
�B
�B
qB
�B
�B
B
B
B
B
B
B
~B
�B
OB
B
�B
�B
�B
�B
�B
�B
!B
�B
OB
B
�B
�B
�B
�B
�B
 'B
!�B
 �B
�B
VB
VB
�B
�B
�B
 'B
 �B
 'B
!B
!bB
!bB
 �B
 �B
 �B
"4B
"�B
#nB
#B
"�B
#B
#nB
"�B
"4B
!�B
!�B
!�B
#�B
#:B
#�B
$�B
$tB
$@B
$�B
%FB
%FB
%�B
%�B
%�B
&�B
&�B
&�B
&�B
($B
'�B
(XB
($B
(�B
)*B
(�B
)_B
)�B
)�B
*�B
+kB
+B
+B
*�B
+B
,�B
,=B
,=B
,=B
,=B
,qB
,=B
,�B
,�B
,�B
,=B
-B
-�B
-�B
.B
.�B
/�B
/�B
0�B
0�B
0�B
0�B
0�B
1�B
0�B
0�B
0�B
1�B
1[B
1'B
1'B
0�B
0�B
0�B
0�B
0�B
0�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
2aB
1�B
1�B
1'B
2aB
33B
2aB
2�B
3�B
4B
4B
4nB
4�B
5tB
5tB
5tB
5tB
5tB
5�B
5tB
5�B
5�B
5�B
7B
8RB
8�B
8�B
8�B
8�B
9$B
9$B
9XB
9�B
9�B
9XB
9�B
9�B
:*B
:^B
:�B
:�B
<B
<6B
=B
=B
=�B
=�B
>B
>B
>BB
>BB
?HB
?HB
?HB
?}B
@B
@�B
@�B
A�B
A�B
A�B
B'B
A�B
B[B
B�B
B�B
CaB
CaB
C�B
DgB
DgB
D�B
D�B
D3B
E9B
E�B
GB
GB
GB
GB
GB
GEB
GzB
GzB
G�B
HKB
H�B
HKB
HKB
H�B
H�B
IRB
IRB
J#B
I�B
I�B
JXB
JXB
J�B
K�B
K^B
K)B
K)B
K�B
L�B
LdB
L�B
MB
MjB
M�B
M�B
M�B
M�B
M�B
NB
NpB
OB
OvB
O�B
PB
P}B
QB
QNB
QNB
QNB
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
RTB
R B
R�B
S&B
S&B
S&B
S&B
S�B
TaB
T,B
T,B
T,B
T�B
T�B
T�B
UgB
U2B
U�B
U�B
U�B
V9B
V�B
V�B
W
B
V�B
V�B
V�B
WsB
XyB
XEB
X�B
YB
Y�B
ZB
ZQB
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[#B
[�B
[�B
[�B
\)B
\)B
]�B
^B
]�B
]�B
^5B
^B
^B
^B
^�B
^�B
_B
_B
_;B
_;B
_;B
_;B
_;B
`BB
`�B
`�B
`�B
`vB
`�B
aB
`�B
aB
a|B
a�B
a|B
bB
bB
bNB
bNB
bNB
bNB
bNB
b�B
b�B
cTB
cTB
c�B
cTB
dZB
d�B
d�B
d�B
d�B
e,B
e`B
e�B
e�B
e�B
ffB
f�B
f�B
f�B
f�B
f�B
gB
f�B
f�B
g8B
g�B
g�B
h
B
h�B
iB
h�B
iB
h�B
h�B
h�B
iyB
iDB
i�B
i�B
i�B
i�B
i�B
i�B
jKB
i�B
iyB
iyB
iyB
iyB
i�B
i�B
jB
jKB
jKB
jKB
jB
jKB
k�B
k�B
k�B
l"B
l�B
m)B
m]B
m�B
m]B
m]B
m]B
m]B
n/B
n/B
n/B
ncB
n�B
n�B
o B
o5B
oiB
oiB
oiB
o�B
o�B
o�B
pB
pB
p;B
p;B
poB
poB
p�B
p�B
p�B
p�B
q�B
q�B
rB
q�B
rB
rB
rB
rB
rB
rGB
r�B
sB
sMB
s�B
s�B
s�B
s�B
tB
t�B
u%B
u%B
uZB
uZB
uZB
v+B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
x8B
x8B
xlB
x�B
x�B
x�B
y	B
y	B
y>B
y�B
yrB
zB
zxB
z�B
z�B
z�B
z�B
{JB
{B
{�B
|B
{�B
|PB
|�B
|�B
}"B
}�B
~(B
~]B
~�B
~�B
~�B
~�B
~�B
cB
cB
cB
�B
cB
�B
�B
��B
��B
��B
��B
��B
�B
��B
��B
�oB
�oB
��B
��B
��B
��B
��B
��B
�GB
�{B
�GB
�{B
�GB
��B
�B
�B
��B
��B
�B
�SB
��B
�%B
��B
��B
��B
��B
��B
��B
��B
�fB
�lB
�lB
��B
��B
�rB
��B
��B
�B
�B
�DB
��B
��B
��B
��B
��B
�B
�~B
�B
�PB
��B
��B
��B
��B
��B
�"B
�VB
�"B
��B
�"B
�"B
�"B
�VB
��B
��B
�VB
��B
�(B
��B
�bB
�bB
��B
��B
�hB
��B
�:B
�oB
��B
��B
��B
�uB
��B
��B
�@B	��B	�fB	�8B	��B	�lB	�fB	��B	��B	��B	��B	�fB	��B	�rB	��B	��B	��B	��B	�	B	�lB	�>B	��B	��B	��B	��B	��B	�>B	��B	�2B	��B	��B	�	B	�B	�fB	��B	�	B	�8B	�fB	�fB	�	B	�>B	�fB	�2B	�	B	�rB	��B	��B	��B	�	B	��B	�B	�fB	�	B	�rB	�lB	�fB	��B	��B	�rB	�fB	��B	�>B	��B	�2B	��B	��B	��B	�>B	��B	�+B	�8B	�	B	�8B	��B	�fB	�8B	�	B	��B	��B	�B	�	B	�>B	��B	��B	�lB	��B	�rB	�fB	��B	�rB	�>B	�fB	��B	�>B	��B	�8B	��B	��B	�	B	�>B	�fB	��B	��B	��B	�8B	��B	��B	��B	�	B	��B	��B	��B	�	B	�8B	�B	��B	�rB	�lB	��B	�fB	��B	�>B	�fB	�`B	��B	��B	��B	�B	�B	��B	�rB	��B	�8B	��B	�lB	�	B	�8B	��B	��B	��B	�>B	��B	�lB	��B	��B	�MB	�B	�B	��B	�B	�B	�TB	�%B	�B	��B	��B	�fB	�B	�B	�B	��B	��B	��B	�vB	�B	��B	�2B	��B	�%B	��B	��B	�B	��B	��B	�B	��B	�B	�B	�B	�B	�B	��B	��B	�B	��B	�B	�B	��B	�B	�TB	�B	�B	�B	��B	�B	��B	�B	��B	��B	��B	��B	��B	��B	�ZB	��B	��B	�TB	�B	�B	��B	�B	�ZB	��B	�|B	�ZB	��B	�B	�2B	��B	�QB	�B	�cB	�B	�B	�B	��B	�B	�B	�;B	��B	�B	�B	�iB	�;B	�vB	�B	�B	�iB	�iB	�;B	��B	�B	�MB	�B	�B	��B	�B	�B	�B	�B	��B	��B	�fB	�DB	�,B	�cB	�WB	�B	�B	�)B	�)B	�QB	�B	�2B	�B	��B	�B	��B	��B	�&B	�B	�vB	��B	��B	�B	�HB	�/B	ߤB	ޞB	�;B	�)B	�#B	�)B	�pB	�mB	�EB	�yB	��B	ԕB	�HB	�2B	��B	�[B	�
B	�B	ݘB	�B	�NB	�B	��B	�8B	��B	�`B	�,B	�B	�8B	�mB	�DB	��B	�B	��B	�B	�oB	�vB	�8B	�rB	��B
{B
1B
DB
B
�B
@B
B
{B
B
B
�B
�B
�B
SB
MB
MB
YB
�B
eB
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
FB
�B
MB
%B
B
�B
�B
"�B
^�B
yrB
#B
SB
+kB
!-B
)�B
+B
,B
4B
N<B
5?B
5tB
4B
3�B
49B
5?B
5�B
6�B
6�B
7LB
8�B
7LB
8RB
7�B
6zB
4�B
6�B
7B
6�B
6zB
7�B
8RB
6�B
8�B
8RB
9XB
6�B
5�B
8�B
2�B
5?B
3hB
5�B
2�B
6B
2aB
/�B
2�B
4B
/OB
4�B
33B
3hB
+6B
0!B
-CB
,�B
)�B
&�B
&�B
'�B
(�B
)�B
*eB
+B
+�B
+kB
+�B
+6B
,qB
-�B
-�B
.�B
/�B
0UB
0!B
/�B
0!B
1�B
2�B
1�B
0�B
.�B
,�B
,=B
/�B
2-B
4B
6�B
=qB
E�B
JXB
�B
K�B
h
B
ffB
d�B
c�B
d&B
e`B
cTB
a�B
aHB
c�B
c�B
b�B
d�B
c�B
_�B
^5B
`�B
_B
b�B
bB
bB
��B
rB
�VB
zxB
|B
|B
t�B
zB
xB
��B
{B
~�B
{B
}VB
cB
}VB
{B
}VB
~]B
z�B
{�B
� B
{JB
{�B
�B
{�B
��B
��B
��B
�B
�oB
��B
�~B
��B
�B
�B
�7B
�DB
��B
��B
��B
��B
��B
�=B
��B
�hB
�B
�B
�kB
��B
��B
��B
�qB
�}B
��B
��B
�B
��B
�XB
��B
�B
��B
�qB
��B
��B
�dB
ÖB
��B
��B
��B
�aB
��B
�,B
ÖB
�tB
�RB
�vB
�gB
�B
��B
��B
�B
��B
�B
�vB
�B
�ZB
�	B
�B
��B
�.B iBB�B�B%B�B	lB
	B	lB
�BB�B~BJB�B�B�BB�B�BMB	B!�B$tB"�B%�B(�B+B2-B4�B:^B?�BD3BC�BD3BC�B@�B?�BFtBI�BMjBM�BS[BC�B@�BB�BA BA BB'B@�B@�B?�B>BB?�B=BB[B:�B<B=<B<�BB[B9�B6�B8RB;�B8�B49B=<B7�B6�B8�B9�B<B<�B?�B:�B4nB2aB/�B/�B9�BA�B4�B5�B0UB<�B?HB6�BIB7�B9�B3hB1�B1�B+�B*�B)�B&B!bB#�B$�B#�B#:B 'BBxB�B�B�B�B�B�B(B
�B
�fB
�ZB
�B
�NB
��B
�|B
�|B
�B
�B
��B
��B
�B
�`B
� B
�EB
�pB
�B
�B
��B
уB
�B
�
B
�&B
�wB
ȴB
�HB
�qB
�<B
�B
��B
��B
�*B
�0B
��B
�6B
�jB
�'B
�dB
�B
��B
�[B
��B
�zB
�B
��B
�4B
�	B
��B
�	B
��B
�xB
�IB
��B
�fB
��B
|�B
�DB
}VB
y�B
v�B
s�B
m�B
l"B
l"B
p;B
o�B
x8B
h
B
Z�B
Y�B
U2B
M6B
J�B
K^B
K^B
H�B
A B
H�B
K�B
K�B
J�B
c�B
Z�B
?�B
AUB
FB
3�B
%B
!�B
7B
_B
 \B
�B
MB
�B
.B
oB
�B
"B
-wB
)_B
 �B
�B	�B	�GB	�/B	�B	�B	��B	�/B	�AB	�B	��B	�8B	�B	�B	�NB	�B	�B	��B	��B	�?B	��B	�5B	�|B	�B	�ZB	��B	�-B	��B	��B	�B	�EB	ĜB	�B	�pB	�B	��B	��B	��B	��B	��B	�bB	�~B	�eB	��B	��B	�YB	�B	�B	�6B	�qB	�1B	��B	�PB	�B	��B	�(B	��B	�GB	�MB	��B	��B	�oB	��B	�B	��B	~]B	}�B	~]B	|�B	.B	cB	~]B	��B	.B	cB	cB	}VB	{�B	|B	~]B	}"B	}"B	��B	�B	}�B	|�B	{B	zDB	zxB	|�B	{G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                             B	�B	�SB	��B	��B	�SB	�B	�B	��B	��B	��B	�SB	�B	�B	�SB	�SB	�LB	�B	��B	��B	�B	��B	�SB	�B	��B	�B	�B	�SB	�B	�:B	�nB	��B	��B	��B	�hB	��B	�hB	��B	��B	�4B	�B	�.B	�\B	�B	�\B	�B	��B	�%B	�:B	��B	�"B	�rB	��B	�{B	�B	��B	�3B
[B
�B
�B
�B
�B
(�B
&�B
B2B
��B
��B
�/B
мB
��B
�:B�BB8�B@�B8]B5KB1�B2B%�B�B
��B
�ZB
�|B
�B
�B
��B
k�B
LB
FB
&�B
B
�B	�=B	�B	��B	�AB	�mB	�dB	��B	��B	{UB	zOB	yB	wqB	w�B	t�B	t�B	��B	|\B	y�B	c_B	]B	S�B	A�B	>�B	B�B	D�B	GQB	TB	;�B	<vB	@�B	A�B	Y�B	c�B	�^B	�B	�B	��B	��B
B
�B
�B
28B
;pB
<AB
?�B
?TB
?�B
9�B
8�B
7�B
9/B
7�B
8�B
<B
<�B
>B
=�B
>�B
>�B
?�B
@�B
A�B
A�B
D
B
C�B
A�B
CB
E�B
FJB
H�B
I(B
K5B
L�B
N�B
O�B
N�B
N�B
R�B
S�B
S�B
T�B
WJB
UrB
VB
V�B
U�B
V�B
X�B
Y�B
Y�B
Y�B
[�B
Z\B
Z(B
Z�B
Z�B
[.B
\4B
Y�B
Y�B
W~B
V�B
VB
XPB
Y"B
X�B
W~B
YVB
SfB
L�B
L;B
LB
L�B
MAB
NB
NGB
M�B
N|B
N�B
NGB
MuB
MAB
LoB
N�B
N�B
N�B
N|B
N�B
N|B
MAB
LB
K�B
L;B
K�B
J�B
JcB
I�B
J/B
J/B
I]B
I]B
I]B
H�B
H�B
G�B
G�B
G�B
H�B
E�B
D�B
EyB
D�B
C�B
D�B
D
B
EDB
D�B
D�B
DsB
DsB
E�B
C�B
ClB
C�B
C�B
CB
B�B
A`B
BfB
?�B
?�B
?�B
@�B
?TB
=�B
>�B
=HB
<�B
:�B
9�B
8�B
8�B
6�B
4�B
5B
4yB
3�B
3sB
5KB
28B
1�B
12B
0�B
0,B
1�B
/�B
0,B
/�B
/�B
.�B
.TB
-�B
.TB
,�B
,�B
,HB
,B
+�B
+vB
*�B
)�B
)�B
#�B
#�B
!�B
$B
 gB
�B
�B
B
�B
�B
pB
jB
*B
�B
�B
�B
#B
�B
�B
�B
RB
�B
�B
�B
B
B
#B
LB
�B
EB
�B
�B
zB
�B
zB
EB
�B
#B
*B
�B
�B
�B
�B
LB
3B
9B
-B
�B
�B
�B
EB
B
�B
[B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
zB
B
B
B
B
�B
�B
0B
^B
dB
dB
6B
B
B
�B
B
�B
�B
�B
RB
RB
�B
B
*B
�B
XB
�B
#B
XB
^B
�B
�B
�B
�B
�B
�B
0B
6B
B
�B
�B
6B
�B
6B
�B
6B
�B
jB
B
�B
jB
6B
jB
�B
�B
�B
HB
�B
pB
B
B
<B
pB
<B
�B
wB
�B
�B
B
B
BB
wB
wB
�B
�B
 B
�B
�B
�B
 B
�B
�B
}B
HB
�B
�B
�B
�B
[B
&B
�B
�B
�B
�B
aB
�B
�B
 3B
 3B
 3B
 �B
!�B
!mB
"
B
!�B
"�B
"�B
"�B
#B
#yB
#�B
$�B
%B
$�B
$�B
$KB
$�B
&WB
%�B
%�B
%�B
%�B
&#B
%�B
&�B
&WB
&�B
%�B
&�B
'^B
'�B
'�B
(�B
)5B
)�B
*�B
*pB
*<B
*pB
*pB
+BB
*�B
*pB
*<B
+�B
+B
*�B
*�B
*<B
*�B
*pB
*pB
*�B
*�B
+BB
+BB
+vB
+�B
+BB
+BB
+BB
+�B
+vB
+�B
,B
+�B
+vB
*�B
,B
,�B
,B
,�B
-�B
-�B
-�B
. B
.�B
/&B
/&B
/&B
/&B
/&B
/ZB
/&B
/�B
/�B
/�B
0�B
2B
28B
28B
2�B
2�B
2�B
2�B
3
B
3>B
3>B
3
B
3�B
3�B
3�B
4B
4yB
4yB
5�B
5�B
6�B
6�B
7WB
7�B
7�B
7�B
7�B
7�B
8�B
8�B
8�B
9/B
9�B
:5B
:�B
;pB
;�B
;�B
;�B
;�B
<B
<�B
<�B
=B
=B
=�B
>B
>B
>NB
>�B
=�B
>�B
?TB
@�B
@�B
@�B
@�B
@�B
@�B
A,B
A,B
A`B
A�B
B2B
A�B
A�B
B2B
B�B
CB
CB
C�B
C�B
C�B
D
B
D
B
D�B
EDB
EB
D�B
D�B
E�B
FJB
FB
FB
F�B
GB
GQB
GQB
GQB
GQB
GQB
G�B
H"B
H�B
I(B
I]B
I�B
J/B
J�B
K B
K B
K B
KiB
KiB
KiB
K�B
KiB
KiB
LB
K�B
LoB
L�B
L�B
L�B
L�B
MAB
NB
M�B
M�B
M�B
N|B
N|B
N|B
OB
N�B
O�B
OMB
OMB
O�B
PSB
PSB
P�B
P�B
P�B
PSB
Q%B
R+B
Q�B
R�B
R�B
SfB
S�B
TB
T8B
T8B
T8B
TlB
T�B
T�B
T�B
U>B
U>B
UrB
U�B
U�B
W~B
W�B
W~B
WJB
W�B
W�B
W�B
W�B
XPB
X�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
Y�B
Z�B
Z\B
Z\B
Z(B
Z�B
Z�B
Z�B
Z�B
[.B
[cB
[.B
[�B
[�B
\ B
\ B
\ B
\ B
\ B
\iB
\iB
]B
]B
]:B
]B
^B
^AB
^AB
^AB
^AB
^�B
_B
_GB
_GB
_GB
`B
`�B
`�B
`�B
`MB
`MB
`�B
`MB
`�B
`�B
aSB
aSB
a�B
bYB
b�B
b�B
b�B
b�B
b�B
b�B
c+B
b�B
c_B
c�B
c�B
c_B
c_B
c�B
c�B
c�B
c+B
c+B
c+B
c+B
c�B
c�B
d1B
c�B
c�B
c�B
c�B
c�B
e7B
elB
elB
e�B
frB
f�B
gB
gCB
gB
gB
gB
gB
g�B
g�B
g�B
hB
hJB
h~B
h�B
h�B
iB
iB
iB
iPB
iPB
i�B
i�B
i�B
i�B
i�B
j!B
j!B
jVB
j�B
j�B
j�B
k\B
k\B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
lbB
l�B
l�B
mhB
mhB
mhB
mhB
m�B
nnB
n�B
n�B
oB
oB
oB
o�B
p{B
p{B
p{B
pFB
p�B
p�B
p�B
qLB
qLB
qLB
q�B
q�B
q�B
rB
rSB
r�B
r�B
r�B
r�B
r�B
sYB
s$B
s�B
t*B
t_B
t�B
t_B
t�B
t�B
u1B
ueB
u�B
u�B
vB
vkB
vkB
v�B
w=B
w�B
xB
xCB
xCB
xwB
x�B
x�B
yB
yB
yB
yIB
yB
yIB
y~B
zOB
zOB
z�B
z�B
z�B
z�B
z�B
z�B
{!B
{!B
{UB
|\B
|�B
|�B
|�B
|�B
|�B
}-B
|�B
}-B
|�B
}bB
}�B
}�B
~3B
~hB
~�B
B
nB
�B
�@B
�@B
�tB
�tB
��B
�FB
�zB
�B
�B
�B
�RB
��B
�$B
�XB
��B
��B
��B
��B
�^B
�^B
�^B
�^B
��B
��B
�0B
��B
�B
�6B
�kB
�kB
��B
��B
��B
�B
��B
��B
��B
��B
��B
�B
�qB
�<B
�B
��B
��B
��B
�B
�B
�IB
�}B
�B
��B
��B
�!B
��B
��B
��B
�'B
��B
��B
��B	�B	�B	��B	�SB	�B	�B	�B	�B	�B	�B	�B	�LB	�$B	�B	�LB	�B	�SB	�B	�B	��B	�LB	�YB	�LB	�B	�B	��B	�B	��B	�LB	�B	�B	�B	�B	�SB	�B	��B	�B	�B	�B	��B	�B	��B	�B	�$B	�B	�LB	�SB	�B	�SB	�B	�B	�B	�$B	�B	�B	�SB	�YB	�$B	�B	�SB	��B	�B	��B	�B	�B	�LB	��B	�B	��B	��B	�B	��B	�LB	�B	��B	�B	�SB	�LB	�B	�B	��B	�B	�B	�B	�B	�$B	�B	�SB	�$B	��B	�B	�LB	��B	�YB	��B	�B	�B	�B	��B	�B	�LB	�B	�B	��B	�B	�SB	�YB	�B	�B	�LB	�YB	�B	��B	�B	�FB	�$B	�B	�{B	�B	�SB	��B	�B	�B	�B	�YB	�YB	�B	�B	�B	�$B	�SB	��B	�{B	�B	�B	��B	�B	�B	�YB	��B	�LB	�B	�_B	�B	��B	�bB	�B	�nB	�4B	��B	�B	��B	��B	�@B	�nB	�B	�bB	��B	��B	�B	�B	�B	�(B	��B	�SB	��B	�B	��B	�B	�uB	�hB	�@B	�B	��B	�B	�hB	�4B	�4B	�4B	�:B	�B	�uB	�\B	�B	��B	�bB	�B	�bB	�B	�4B	�\B	��B	�B	��B	�B	��B	�B	�{B	�B	�B	�xB	�nB	�B	�B	�B	�B	��B	�4B	�B	�hB	�B	�FB	�.B	�B	�B	��B	��B	�@B	�B	�B	�B	�4B	�4B	�bB	�xB	�VB	�bB	��B	�B	�\B	�B	�B	��B	�(B	�bB	�B	�B	�B	��B	�B	��B	��B	�VB	��B	�B	�B	�bB	�=B	�_B	�rB	�rB	�B	��B	��B	�B	�	B	�_B	��B	��B	��B	�B	��B	��B	��B	�uB	�oB	ݣB	ݣB	��B	�cB	�(B	ݣB	ڑB	��B	��B	��B	�VB	�PB	��B	��B	��B	��B	�"B	�B	��B	�+B	юB	�GB	��B	��B	�~B	�B	�B	��B	�JB	ٿB	� B	�oB	��B	��B	��B	�B	��B	�GB	��B	�B	��B	�B	�lB	�xB	�PB	�!B	�(B	��B	�$B	�wB	�-B
�B
�B
�B

}B
�B
�B
-B
�B
�B
gB
9B
tB
B
�B
�B
B
�B
B
�B
tB
?B
tB
?B
tB
OB
nB
OB
�B
�B
EB
�B
�B
�B

IB
3B
�B
XPB
s$B
�B
B
%B
�B
#EB
$�B
%�B
-�B
G�B
.�B
/&B
-�B
-�B
-�B
.�B
/�B
0�B
0�B
0�B
28B
0�B
2B
1�B
0,B
.�B
0`B
0�B
0`B
0,B
1�B
2B
0�B
28B
2B
3
B
0�B
/�B
2mB
,�B
.�B
-B
/ZB
,HB
/�B
,B
)�B
,HB
-�B
)B
.�B
,�B
-B
$�B
)�B
&�B
&WB
#EB
 3B
 3B
!9B
"?B
#EB
$B
$�B
%QB
%B
%QB
$�B
&#B
'^B
'�B
(dB
)jB
*B
)�B
)5B
)�B
+�B
,HB
+vB
*pB
(�B
&WB
%�B
)5B
+�B
-�B
0�B
7#B
?�B
D
B
��B
EDB
a�B
`B
^�B
]�B
]�B
_B
]B
[�B
Z�B
]�B
]oB
\�B
^uB
]�B
Y�B
W�B
Z�B
X�B
\iB
[�B
[�B
��B
k�B
�B
t*B
u�B
u�B
n�B
s�B
q�B
|�B
u1B
xwB
t�B
wB
yB
wB
u1B
wB
xB
t�B
u�B
y�B
t�B
u�B
yIB
u�B
{UB
��B
�XB
~�B
{!B
�LB
�0B
��B
��B
~�B
��B
��B
�gB
��B
��B
��B
��B
��B
�EB
�B
��B
��B
�B
�yB
�KB
�BB
�#B
�/B
�5B
��B
��B
�2B
�
B
�ZB
��B
��B
�#B
��B
�cB
�B
�HB
��B
�pB
��B
�B
�CB
��B
�HB
�&B
�B
�(B
�B
��B
֭B
ڑB
�AB
�B
�JB
�(B
�\B
�B
�B
�1B
�7B
��B
�B
��B
��B
�hB
��B
�nBB�BB�B�BeB0B�B�B�BeB�B
IBaB�B�B�B&B�BaB"?B$�B+�B.�B4B9�B=�B=�B=�B=HB:�B9cB@&BC�BGBG�BMB=HB:�B<AB:�B:�B;�B:jB:5B9cB7�B9cB6�B<B4�B5�B6�B6QB<B3>B0`B2B5KB2�B-�B6�B1gB0�B2�B3�B5�B6�B9cB4EB. B,B)5B)jB3>B;pB.TB/�B*B6QB8�B0�BB�B1�B3>B-B+vB+vB%�B$�B#�B�BBUB�B�B�B�B�B*B
�nB
�hB
��BFBtB tB�B
��B
�B
�B
�iB
� B
ܝB
�.B
�.B
�iB
��B
�~B
�~B
ٿB
�B
��B
��B
�"B
��B
��B
�B
�5B
ƳB
мB
��B
�)B
�fB
��B
�#B
��B
��B
�WB
��B
��B
��B
�KB
��B
�B
��B
�B
��B
�yB
�B
��B
�,B
��B
�gB
��B
��B
��B
��B
��B
�*B
��B
�nB
�B
zOB
vkB
��B
wB
sYB
p�B
m�B
gCB
e�B
e�B
i�B
i�B
q�B
a�B
TlB
SfB
N�B
F�B
D>B
EB
EB
BfB
:�B
B�B
EDB
E�B
D>B
]oB
T�B
9�B
;B
?�B
-�B
�B
HB
�B
B
B
�B
�B
�B
	�B
!B
<B
�B
')B
#B
wB	��B	�\B	��B	��B	�CB	�B	�B	��B	��B	�B	ݣB	��B	��B	�4B	� B	��B	׳B	ًB	�|B	��B	юB	��B	�.B	��B	�B	��B	��B	��B	ħB	��B	��B	�NB	϶B	�"B	��B	��B	��B	��B	�6B	�BB	�B	�0B	�B	�6B	�dB	�B	��B	��B	��B	�#B	��B	��B	�B	��B	��B	��B	�LB	|�B	}�B	{�B	|�B	{!B	zOB	y~B	nB	xB	w=B	xB	v�B	x�B	yB	xB	zOB	x�B	yB	yB	wB	u�B	u�B	xB	v�B	v�B	{�B	z�B	wqB	v7B	u1B	s�B	t*B	v�B	t�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                             G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0061566                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0061566                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 24 11 2022 179 -0.0061566 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 24 11 2022 179 -0.0061566 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230512140054                            20230512140054AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023051214005420230512140054  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023051214005420230512140054QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F83E           783E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023051214005420230512140054QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               