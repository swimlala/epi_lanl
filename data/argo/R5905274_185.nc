CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-04-26T22:32:27Z creation      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20230426223227  20230426223227  5905274 5905274 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO7315                            7315                            2B  2B  AA  SOLO_II                         SOLO_II                         8643                            8643                            SBE602 11Jan18                  SBE602 11Jan18                  853 853 @�!m��@�!m��11  @�!I���@�!I���@0%�M:�@0%�M:��d<�`�d<�`11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AB  AB  ?�=q?��H@@  @�G�@�G�@\@�G�A ��A��A!G�A-p�A?\)A`  A���A�Q�A��A�\)A��AУ�A�Q�A�  A��B  B(�BQ�B   B'�
B/�
B7�
B?�
BH(�BP(�BX  B_�
Bg�
Bp(�Bx(�B�  B�  B�  B�(�B�{B��
B�  B�  B�  B�  B�  B�B��
B�  B�=qB�(�B��B�  B�{B�{B�  B�  B�  B��B��B�{B�=qB�(�B��B�B�(�B�Q�C {C
=C
=C  C  C
{C
=C��C  C  C  C
=C
=C  C  C
=C 
=C"
=C$
=C&  C(  C*
=C,  C.
=C0
=C2  C4  C6  C7�C9��C<
=C>
=C@
=CB
=CC��CE��CH
=CJ
=CK��CM�CO�CQ�HCS��CU��CW�CY��C\  C^  C`  Cb  Cd  Cf
=Ch
=Cj
=Ck��Cm�Co�Cq��Ct
=Cv{Cx
=Cz  C{��C~  C�
=C�C�  C�C�  C�C�  C���C���C�  C�
=C�C���C�C�  C�  C�C�  C�  C�  C���C���C�  C���C��C���C�C�\C�C���C���C���C�C�C�  C�  C�
=C�
=C�  C�  C���C���C���C�  C�C���C�  C�C���C�  C�
=C�  C�  C�  C�  C�  C���C�  C���C�  C�  C�  C�C�  C�  C�C���C���C���C���C�  C�
=C�\C�
=C���C���C���C���C�  C�C�C�C�C���C���C�  C���C���C���C���C�C�
=C���C�  C�  C�  C���C��C�C�\C�C�C�C�  C�  C�  C�C�
=C�  C�  C�  C���C���C�  C���C�
=C�C���C�  C���C���C���C�C�\C�C���C�  C���D   D � D �qD� D�qD� D  D� D  D� D  D��D  D}qD  D� D  D}qD��D	� D
  D
��D
�qD}qD�qDz�D  D� D�Dz�D�qD��D  D}qD�qD��D�D� D�D��D  D� D�qD� D�D� D  D� D�qD� D�qD}qD  Dz�D��Dz�D�qD� D�D� D�qD� D�D��D��D ��D!�D!}qD"  D"}qD"��D#}qD$  D$��D%  D%}qD&  D&}qD'  D'�D'�qD(z�D(��D)}qD)��D*z�D+  D+�D,D,}qD,��D-��D.�D.z�D/  D/�D0D0}qD1  D1��D2  D2� D2�RD3z�D3��D4� D5�D5�D6�D6��D7  D7}qD8  D8�D9�D9� D:  D:� D:��D;��D<D<}qD<��D=}qD>  D>u�D>��D?�D@�D@� DADA}qDA�qDB}qDB�qDC� DC��DD��DE�DE��DF  DF� DGDG� DH�DH��DI�DI� DJ�DJ��DK  DK��DL�DL��DM�DM�DNDN��DN�qDO}qDO�qDP}qDP�qDQ� DRDR}qDR��DS� DT  DT}qDU  DU��DU�qDV}qDV�qDW}qDW�qDXz�DX��DY}qDY�qDZz�D[  D[��D\  D\}qD]�D]��D^  D^}qD_�D_�D`D`� D`��Da��Db�Db}qDc�Dc}qDc��Dd� Dd�qDe}qDf�Df��Dg�Dg�Dh  Dhz�Di�Di� Dj  Dj�DkDk��Dk�qDl� Dm�Dm� Dm�qDn}qDo�Do��Dp  Dp�Dq�Dq}qDr�Dr}qDsDs}qDt  Dt� Dt��Du}qDv  Dv��Dw�Dw�Dx�Dx��Dy�Dy�Dy��Dz}qD{�D{xRD{�qD|��D}D}}qD}�qD~��D�D}qD��D�@ D��HD���D�HD�B�D��HD�� D�  D�AHD��HD���D���D�B�D�~�D���D��D�AHD��HD��HD���D�@ D��HD��HD�  D�B�D��HD��HD�  D�@ D�� D��HD�HD�AHD���D��HD���D�>�D�� D���D�HD�@ D��HD�D�  D�>�D��HD��qD���D�>�D�� D�� D�HD�B�D��HD��HD���D�>�D�� D�� D�HD�AHD�~�D���D�  D�>�D�� D���D�HD�AHD��HD�� D��qD�>�D�~�D�� D���D�@ D��HD�� D���D�>�D�� D���D�HD�B�D���D��HD�  D�@ D���D��HD���D�>�D�~�D���D�  D�@ D�� D���D���D�>�D��HD�� D���D�AHD��HD���D�  D�@ D�� D�� D���D�AHD��HD�� D�  D�AHD���D�� D��qD�@ D�~�D�D�HD�@ D�}qD���D�HD�B�D�� D�� D�  D�@ D��HD��HD���D�>�D�~�D�� D��D�@ D��HD��HD���D�>�D�~�D�� D�HD�>�D�~�D��HD�  D�AHD�~�D�� D�  D�>�D�}qD���D���D�>�D�� D�� D��qD�=qD�~�D��HD��D�>�D��HD���D�  D�>�D�� D��HD�HD�AHD�� D�� D�HD�B�D�� D���D�HD�=qD�� D��HD�  D�AHD�� D��qD��qD�>�D�� D�D��D�B�D�~�D���D�  D�>�D��HD�D�  D�@ D���D��HD�HD�AHD�~�D���D�  D�>�D�� D���D��)D�=qD�|)D�� D�HD�@ D�~�D��qD���D�B�D�~�D��qD�HD�AHD�� D��HD�HD�>�D�~�D�� D�  D�AHD���D��HD�  D�AHD��HD�� D���D�@ D�� D�� D�  D�@ D�}qD�� D��qD�AHD�~�D��HD�  D�AHD�}qD�� D��D�@ D�~�D�D�HD�=qD�}qD��HD���D�@ DÀ Dþ�D�HD�@ D�~�Dľ�D��D�AHDŀ D�D�HD�>�DƂ�Dƾ�D���D�@ Dǂ�DǾ�D���D�AHDȂ�D��HD�HD�AHDɀ Dɾ�D�  D�>�D�~�D��HD�  D�@ DˁHD�� D�  D�=qD�~�D̾�D�HD�AHD͂�D��HD���D�>�D΁HDνqD�  D�@ Dπ DϾ�D�HD�@ DЀ D�� D���D�AHD�~�DѾ�D�HD�B�D҂�D�D�HD�AHD�}qDӾ�D��D�>�DԁHD�� D��D�AHDՁHD��HD���D�@ Dւ�D־�D��qD�=qD׀ D�� D��D�AHD؀ D�� D�  D�AHDـ D�� D�  D�@ DځHDھ�D�  D�AHDہHD۾�D��qD�=qD�}qD�� D�HD�AHD݁HD�� D�HD�B�Dނ�D��HD�HD�@ D߀ D߾�D�  D�AHD�~�DྸD�HD�B�DႏD�D�  D�>�D�}qD⾸D�  D�@ D�~�D�qD���D�>�D� D�D��qD�=qD�HD徸D�  D�AHD�HD�� D�  D�@ D�}qD�� D���D�@ D�~�D��HD��D�=qD�HD��HD�  D�>�D� D��HD��D�@ D� D뾸D��qD�>�D�HD�� D���D�AHD� D��HD��qD�AHD� DD�  D�AHD�~�D�� D���D�@ D�|)D�� D��D�=qD� D��HD�HD�AHD�D��HD�HD�=qD�~�D�� D���D�AHD� D��HD�  D�@ D�}qD���D�  D�@ D�� D���D���D�AHD��HD��HD�HD�=qD�� D�� D���D�@ D��HD�D�HD�@ D�u�>���?8Q�?��?�33?�(�?��H@�@&ff@=p�@Y��@n{@��\@��@�
=@��
@�\)@���@���@�
=@�\@�\)@�
=AG�A�Ap�A33A=qA   A&ffA,��A1G�A7
=A<��AB�\AJ�HAP��AW
=A]p�Ac33Ag�An{As�
A{�A�G�A�z�A��A�=qA���A��A��HA�
=A��\A�A���A��A��RA�=qA��A���A�z�A�  A��A�ffA���A��
A�
=A��A�AУ�A��HA�p�A�\)A��A��
A�{A�  A�=qA���A�
=A���A��HA���A�{A�A��A�A�A�Q�A��\A���A��RB z�BG�B=qB\)BQ�Bp�B�B
=B�
B	�B	�B
�HBQ�Bp�BffB�Bz�B��B{B
=B  B��B=qB�BQ�Bp�B�\B\)B  B�B{B
=B Q�B!��B"�\B#�B$z�B%��B&�\B'�B(Q�B)p�B*=qB+
=B,(�B-�B.=qB/33B0  B1p�B2ffB3�B4��B5��B6�\B7�B8Q�B9p�B:{B;33B<(�B=G�B>{B?33B@Q�BA��BB�\BC�BD��BEp�BF=qBG33BH(�BH��BJ�\BK�BLz�BM��BN�\BO�BPz�BQ�BR{BS33BT  BU�BVffBW�BXz�BYp�BZ�\B[�B\Q�B\��B]�B^�HB_�
BaG�Bb{Bc33Bd(�Bd��Be��Bf�RBg�Bhz�Bip�Bj�\Bk�Bl��BmBn�HBo�
Bp��Bqp�BrffBs33Bt  Bt��Bu�Bv�RBx(�By�By�B{
=B|  B|��B}��B~=qB\)B�  B��\B���B�p�B��
B�z�B�
=B��B�  B�z�B���B��B��B�{B��\B�33B�B�=qB��RB�33B��B��B�ffB���B�G�B��B�z�B���B�p�B�  B�ffB���B��B���B�(�B��RB�33B��B�=qB��\B��HB�p�B��
B�ffB���B�\)B�  B��\B�
=B��B�B�=qB���B�G�B��
B�ffB��RB��B���B�(�B��HB�\)B�B�=qB��RB�p�B�{B��\B���B��B�Q�B���B�G�B�B�ffB��B��B�Q�B��RB�G�B�{B��RB�
=B���B�=qB���B�\)B�  B��\B�p�B�{B�z�B��B��B��\B�
=B���B�Q�B�33B���B�=qB��B�B�=qB���B��
B��\B���B�B��\B��B��B��\B�G�B�B�ffB�\)B�{B�z�B�33B�(�B��HB�\)B�  B��HBř�B�{BƸRBǮB�=qB���BɮB�ffB��HB�B�z�B�
=BͮBΣ�B�G�B��B��HBљ�B�{B���B�B�z�B�
=B�BָRB�33B�{B��HB�\)B�(�B���BۅB�=qB��B�B�ffB�G�B��B�z�B�p�B�(�B�RB�B�Q�B��HB�B��B�
=B��
B�RB�G�B�  B���B�p�B�=qB�33B��B��B�B�  B���B�B�(�B��B��B�ffB�33B�{B��\B�\)B��B�Q�B��B���B��B���B�
=B�G�B�  B�=qB�z�B���B�\)B��B�B�Q�B�ffB��RB�G�B���B�C �C Q�C \)C �\C C �
C ��C=qCffCp�C�RC�
C�C33CQ�Cp�C�RC�HC��C33CffCp�C�RC�HC�C33CG�Cp�C�C��C  C33CG�Cz�C�RCC  C33C=qC�\C��CC  C{CG�C�\C��C�HC{C�CffCp�C��C�C	  C	=qC	ffC	z�C	C	�
C
{C
G�C
Q�C
��C
�RC  C{C33C�C�\C�
C
=C
=CQ�Cz�C�\C�HC��C=qCQ�Cz�CC�
C{CG�C\)C�CC
=C(�CQ�C��C��C��C{CQ�C�C��C�HC��C=qCz�C�C��C�HC{CffCz�CC�
C
=C\)Cp�C�RC�
C��CG�CffC��C�HC��CG�CQ�C��C�
C�C=qCQ�C��C�RC�C=qCG�C��C�RC  C�CG�C��C��C��C{C\)Cp�CC�
C{C\)Cp�CC�
C(�C=qC�\C�C�HC�C33C�C��C�C
=CQ�Cp�C�RC�
C{CQ�CffC�RC�
C�C33Cp�C�CC {C (�C z�C �\C �
C �C!=qC!\)C!��C!��C!�C"=qC"Q�C"��C"�RC#  C#{C#ffC#z�C#��C#�HC${C$\)C$z�C$��C$�
C%�C%=qC%�\C%��C%�HC&�C&=qC&�C&��C&��C'
=C'Q�C'z�C'C'�HC((�C(G�C(�\C(�C)  C){C)\)C)�C)��C)��C*(�C*Q�C*��C*C+  C+=qC+ffC+�C+C,�C,=qC,��C,��C-  C-{C-p�C-�C-�HC-��C.Q�C.ffC.�RC.�
C/(�C/G�C/��C/C0
=C033C0�C0��C0��C1{C1ffC1�C1��C2
=C233C2�C2��C2��C3
=C3ffC3z�C3�
C3��C4Q�C4\)C4�RC4�HC5(�C5\)C5�\C5C6  C6G�C6p�C6C6�
C733C7Q�C7��C7C8{C833C8�\C8��C9  C9
=C9ffC9�\C9C:{C:33C:�C:��C:��C;{C;\)C;�C;�
C;�C<G�C<\)C<�C<�HC={C=\)C=z�C=��C=�C>G�C>ffC>�RC>�
C?�C?\)C?�\C?�HC@  C@\)C@z�C@��C@�CAG�CAffCA�RCA�
CB33CB\)CB�\CB�HCC  CC\)CC�CC��CC�CDG�CDp�CD�CE  CE(�CEz�CE��CE�CF(�CFQ�CF�CF�
CG(�CGG�CG�\CG�
CH  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                  ?�=q?��H@@  @�G�@�G�@\@�G�A ��A��A!G�A-p�A?\)A`  A���A�Q�A��A�\)A��AУ�A�Q�A�  A��B  B(�BQ�B   B'�
B/�
B7�
B?�
BH(�BP(�BX  B_�
Bg�
Bp(�Bx(�B�  B�  B�  B�(�B�{B��
B�  B�  B�  B�  B�  B�B��
B�  B�=qB�(�B��B�  B�{B�{B�  B�  B�  B��B��B�{B�=qB�(�B��B�B�(�B�Q�C {C
=C
=C  C  C
{C
=C��C  C  C  C
=C
=C  C  C
=C 
=C"
=C$
=C&  C(  C*
=C,  C.
=C0
=C2  C4  C6  C7�C9��C<
=C>
=C@
=CB
=CC��CE��CH
=CJ
=CK��CM�CO�CQ�HCS��CU��CW�CY��C\  C^  C`  Cb  Cd  Cf
=Ch
=Cj
=Ck��Cm�Co�Cq��Ct
=Cv{Cx
=Cz  C{��C~  C�
=C�C�  C�C�  C�C�  C���C���C�  C�
=C�C���C�C�  C�  C�C�  C�  C�  C���C���C�  C���C��C���C�C�\C�C���C���C���C�C�C�  C�  C�
=C�
=C�  C�  C���C���C���C�  C�C���C�  C�C���C�  C�
=C�  C�  C�  C�  C�  C���C�  C���C�  C�  C�  C�C�  C�  C�C���C���C���C���C�  C�
=C�\C�
=C���C���C���C���C�  C�C�C�C�C���C���C�  C���C���C���C���C�C�
=C���C�  C�  C�  C���C��C�C�\C�C�C�C�  C�  C�  C�C�
=C�  C�  C�  C���C���C�  C���C�
=C�C���C�  C���C���C���C�C�\C�C���C�  C���D   D � D �qD� D�qD� D  D� D  D� D  D��D  D}qD  D� D  D}qD��D	� D
  D
��D
�qD}qD�qDz�D  D� D�Dz�D�qD��D  D}qD�qD��D�D� D�D��D  D� D�qD� D�D� D  D� D�qD� D�qD}qD  Dz�D��Dz�D�qD� D�D� D�qD� D�D��D��D ��D!�D!}qD"  D"}qD"��D#}qD$  D$��D%  D%}qD&  D&}qD'  D'�D'�qD(z�D(��D)}qD)��D*z�D+  D+�D,D,}qD,��D-��D.�D.z�D/  D/�D0D0}qD1  D1��D2  D2� D2�RD3z�D3��D4� D5�D5�D6�D6��D7  D7}qD8  D8�D9�D9� D:  D:� D:��D;��D<D<}qD<��D=}qD>  D>u�D>��D?�D@�D@� DADA}qDA�qDB}qDB�qDC� DC��DD��DE�DE��DF  DF� DGDG� DH�DH��DI�DI� DJ�DJ��DK  DK��DL�DL��DM�DM�DNDN��DN�qDO}qDO�qDP}qDP�qDQ� DRDR}qDR��DS� DT  DT}qDU  DU��DU�qDV}qDV�qDW}qDW�qDXz�DX��DY}qDY�qDZz�D[  D[��D\  D\}qD]�D]��D^  D^}qD_�D_�D`D`� D`��Da��Db�Db}qDc�Dc}qDc��Dd� Dd�qDe}qDf�Df��Dg�Dg�Dh  Dhz�Di�Di� Dj  Dj�DkDk��Dk�qDl� Dm�Dm� Dm�qDn}qDo�Do��Dp  Dp�Dq�Dq}qDr�Dr}qDsDs}qDt  Dt� Dt��Du}qDv  Dv��Dw�Dw�Dx�Dx��Dy�Dy�Dy��Dz}qD{�D{xRD{�qD|��D}D}}qD}�qD~��D�D}qD��D�@ D��HD���D�HD�B�D��HD�� D�  D�AHD��HD���D���D�B�D�~�D���D��D�AHD��HD��HD���D�@ D��HD��HD�  D�B�D��HD��HD�  D�@ D�� D��HD�HD�AHD���D��HD���D�>�D�� D���D�HD�@ D��HD�D�  D�>�D��HD��qD���D�>�D�� D�� D�HD�B�D��HD��HD���D�>�D�� D�� D�HD�AHD�~�D���D�  D�>�D�� D���D�HD�AHD��HD�� D��qD�>�D�~�D�� D���D�@ D��HD�� D���D�>�D�� D���D�HD�B�D���D��HD�  D�@ D���D��HD���D�>�D�~�D���D�  D�@ D�� D���D���D�>�D��HD�� D���D�AHD��HD���D�  D�@ D�� D�� D���D�AHD��HD�� D�  D�AHD���D�� D��qD�@ D�~�D�D�HD�@ D�}qD���D�HD�B�D�� D�� D�  D�@ D��HD��HD���D�>�D�~�D�� D��D�@ D��HD��HD���D�>�D�~�D�� D�HD�>�D�~�D��HD�  D�AHD�~�D�� D�  D�>�D�}qD���D���D�>�D�� D�� D��qD�=qD�~�D��HD��D�>�D��HD���D�  D�>�D�� D��HD�HD�AHD�� D�� D�HD�B�D�� D���D�HD�=qD�� D��HD�  D�AHD�� D��qD��qD�>�D�� D�D��D�B�D�~�D���D�  D�>�D��HD�D�  D�@ D���D��HD�HD�AHD�~�D���D�  D�>�D�� D���D��)D�=qD�|)D�� D�HD�@ D�~�D��qD���D�B�D�~�D��qD�HD�AHD�� D��HD�HD�>�D�~�D�� D�  D�AHD���D��HD�  D�AHD��HD�� D���D�@ D�� D�� D�  D�@ D�}qD�� D��qD�AHD�~�D��HD�  D�AHD�}qD�� D��D�@ D�~�D�D�HD�=qD�}qD��HD���D�@ DÀ Dþ�D�HD�@ D�~�Dľ�D��D�AHDŀ D�D�HD�>�DƂ�Dƾ�D���D�@ Dǂ�DǾ�D���D�AHDȂ�D��HD�HD�AHDɀ Dɾ�D�  D�>�D�~�D��HD�  D�@ DˁHD�� D�  D�=qD�~�D̾�D�HD�AHD͂�D��HD���D�>�D΁HDνqD�  D�@ Dπ DϾ�D�HD�@ DЀ D�� D���D�AHD�~�DѾ�D�HD�B�D҂�D�D�HD�AHD�}qDӾ�D��D�>�DԁHD�� D��D�AHDՁHD��HD���D�@ Dւ�D־�D��qD�=qD׀ D�� D��D�AHD؀ D�� D�  D�AHDـ D�� D�  D�@ DځHDھ�D�  D�AHDہHD۾�D��qD�=qD�}qD�� D�HD�AHD݁HD�� D�HD�B�Dނ�D��HD�HD�@ D߀ D߾�D�  D�AHD�~�DྸD�HD�B�DႏD�D�  D�>�D�}qD⾸D�  D�@ D�~�D�qD���D�>�D� D�D��qD�=qD�HD徸D�  D�AHD�HD�� D�  D�@ D�}qD�� D���D�@ D�~�D��HD��D�=qD�HD��HD�  D�>�D� D��HD��D�@ D� D뾸D��qD�>�D�HD�� D���D�AHD� D��HD��qD�AHD� DD�  D�AHD�~�D�� D���D�@ D�|)D�� D��D�=qD� D��HD�HD�AHD�D��HD�HD�=qD�~�D�� D���D�AHD� D��HD�  D�@ D�}qD���D�  D�@ D�� D���D���D�AHD��HD��HD�HD�=qD�� D�� D���D�@ D��HD�D�HD�@ D�u�>���?8Q�?��?�33?�(�?��H@�@&ff@=p�@Y��@n{@��\@��@�
=@��
@�\)@���@���@�
=@�\@�\)@�
=AG�A�Ap�A33A=qA   A&ffA,��A1G�A7
=A<��AB�\AJ�HAP��AW
=A]p�Ac33Ag�An{As�
A{�A�G�A�z�A��A�=qA���A��A��HA�
=A��\A�A���A��A��RA�=qA��A���A�z�A�  A��A�ffA���A��
A�
=A��A�AУ�A��HA�p�A�\)A��A��
A�{A�  A�=qA���A�
=A���A��HA���A�{A�A��A�A�A�Q�A��\A���A��RB z�BG�B=qB\)BQ�Bp�B�B
=B�
B	�B	�B
�HBQ�Bp�BffB�Bz�B��B{B
=B  B��B=qB�BQ�Bp�B�\B\)B  B�B{B
=B Q�B!��B"�\B#�B$z�B%��B&�\B'�B(Q�B)p�B*=qB+
=B,(�B-�B.=qB/33B0  B1p�B2ffB3�B4��B5��B6�\B7�B8Q�B9p�B:{B;33B<(�B=G�B>{B?33B@Q�BA��BB�\BC�BD��BEp�BF=qBG33BH(�BH��BJ�\BK�BLz�BM��BN�\BO�BPz�BQ�BR{BS33BT  BU�BVffBW�BXz�BYp�BZ�\B[�B\Q�B\��B]�B^�HB_�
BaG�Bb{Bc33Bd(�Bd��Be��Bf�RBg�Bhz�Bip�Bj�\Bk�Bl��BmBn�HBo�
Bp��Bqp�BrffBs33Bt  Bt��Bu�Bv�RBx(�By�By�B{
=B|  B|��B}��B~=qB\)B�  B��\B���B�p�B��
B�z�B�
=B��B�  B�z�B���B��B��B�{B��\B�33B�B�=qB��RB�33B��B��B�ffB���B�G�B��B�z�B���B�p�B�  B�ffB���B��B���B�(�B��RB�33B��B�=qB��\B��HB�p�B��
B�ffB���B�\)B�  B��\B�
=B��B�B�=qB���B�G�B��
B�ffB��RB��B���B�(�B��HB�\)B�B�=qB��RB�p�B�{B��\B���B��B�Q�B���B�G�B�B�ffB��B��B�Q�B��RB�G�B�{B��RB�
=B���B�=qB���B�\)B�  B��\B�p�B�{B�z�B��B��B��\B�
=B���B�Q�B�33B���B�=qB��B�B�=qB���B��
B��\B���B�B��\B��B��B��\B�G�B�B�ffB�\)B�{B�z�B�33B�(�B��HB�\)B�  B��HBř�B�{BƸRBǮB�=qB���BɮB�ffB��HB�B�z�B�
=BͮBΣ�B�G�B��B��HBљ�B�{B���B�B�z�B�
=B�BָRB�33B�{B��HB�\)B�(�B���BۅB�=qB��B�B�ffB�G�B��B�z�B�p�B�(�B�RB�B�Q�B��HB�B��B�
=B��
B�RB�G�B�  B���B�p�B�=qB�33B��B��B�B�  B���B�B�(�B��B��B�ffB�33B�{B��\B�\)B��B�Q�B��B���B��B���B�
=B�G�B�  B�=qB�z�B���B�\)B��B�B�Q�B�ffB��RB�G�B���B�C �C Q�C \)C �\C C �
C ��C=qCffCp�C�RC�
C�C33CQ�Cp�C�RC�HC��C33CffCp�C�RC�HC�C33CG�Cp�C�C��C  C33CG�Cz�C�RCC  C33C=qC�\C��CC  C{CG�C�\C��C�HC{C�CffCp�C��C�C	  C	=qC	ffC	z�C	C	�
C
{C
G�C
Q�C
��C
�RC  C{C33C�C�\C�
C
=C
=CQ�Cz�C�\C�HC��C=qCQ�Cz�CC�
C{CG�C\)C�CC
=C(�CQ�C��C��C��C{CQ�C�C��C�HC��C=qCz�C�C��C�HC{CffCz�CC�
C
=C\)Cp�C�RC�
C��CG�CffC��C�HC��CG�CQ�C��C�
C�C=qCQ�C��C�RC�C=qCG�C��C�RC  C�CG�C��C��C��C{C\)Cp�CC�
C{C\)Cp�CC�
C(�C=qC�\C�C�HC�C33C�C��C�C
=CQ�Cp�C�RC�
C{CQ�CffC�RC�
C�C33Cp�C�CC {C (�C z�C �\C �
C �C!=qC!\)C!��C!��C!�C"=qC"Q�C"��C"�RC#  C#{C#ffC#z�C#��C#�HC${C$\)C$z�C$��C$�
C%�C%=qC%�\C%��C%�HC&�C&=qC&�C&��C&��C'
=C'Q�C'z�C'C'�HC((�C(G�C(�\C(�C)  C){C)\)C)�C)��C)��C*(�C*Q�C*��C*C+  C+=qC+ffC+�C+C,�C,=qC,��C,��C-  C-{C-p�C-�C-�HC-��C.Q�C.ffC.�RC.�
C/(�C/G�C/��C/C0
=C033C0�C0��C0��C1{C1ffC1�C1��C2
=C233C2�C2��C2��C3
=C3ffC3z�C3�
C3��C4Q�C4\)C4�RC4�HC5(�C5\)C5�\C5C6  C6G�C6p�C6C6�
C733C7Q�C7��C7C8{C833C8�\C8��C9  C9
=C9ffC9�\C9C:{C:33C:�C:��C:��C;{C;\)C;�C;�
C;�C<G�C<\)C<�C<�HC={C=\)C=z�C=��C=�C>G�C>ffC>�RC>�
C?�C?\)C?�\C?�HC@  C@\)C@z�C@��C@�CAG�CAffCA�RCA�
CB33CB\)CB�\CB�HCC  CC\)CC�CC��CC�CDG�CDp�CD�CE  CE(�CEz�CE��CE�CF(�CFQ�CF�CF�
CG(�CGG�CG�\CG�
CH  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AϾwA�A�ƨA���A���A��
A��A���A���A���A��
A���A��/A��HA��HA���A���A�x�A�x�A�z�A�v�A�t�A�r�A�r�A�r�A�t�A�|�AρAυAϋDAω7AυAσAχ+Aω7AρA�ffA��A��TA�ȴA��A�JA�x�A�l�A˶FA�A�dZA�t�A���A�K�A�JA�JA��A�(�A��/A�33A�ĜA��A�(�A�x�A�ƨA� �A��A��\A��9A�?}A�9XA�hsA��wA�~�A�ffA�ĜA��A�{A�5?A��\A���A��A��A��wA~jA{p�AyK�Axr�Aw33Au��Ar�/An�uAk�Af{Ab��A^��A]33AY��AU|�ATA�AQ�TAL�HAI�AHbAF-AC�#ABA�AA;dA?�TA?|�A?33A=�A;�A8A�A6�!A5�A5VA4jA4$�A3�PA2��A1�TA0��A/�#A.�yA-��A-G�A,��A,��A,��A,bA)O�A( �A&�jA%��A$n�A"��A"  A!��A ��A JA��A�9AbNA\)A;dA�A��AA�A��A�\A�A�A�TA�AQ�AZAv�A$�A�A�;Az�A�hAp�A�9A�A�AȴA1A%A�+A+A(�A=qAC�A�-AƨAXAn�A��Al�A �A��AffA  A��A�`A  A�A��A�A=qA�;A|�A
�/A
�+A
$�A	�hA	
=A9XA{An�A�A�AA��Al�A�uA5?A9XAI�AM�AA�AA�A��A
=A"�A\)AO�A��Ar�A�A|�A�\A�FA�7A%A ��A =qA (�@���@��@��/@�Z@�=q@�x�@��`@��w@��R@�{@�@�`B@���@���@� �@�F@�"�@��@�7@�G�@�  @�  @��m@�F@�;d@�o@��H@�n�@��#@�7L@���@�@�ƨ@�R@��@�w@�n�@���@�G�@��`@�\)@���@�@�@��H@⟾@�^5@��@��/@��u@�1@ߥ�@�t�@�C�@���@�ff@��@�p�@��`@���@ܛ�@��@ۅ@�;d@�@�v�@��@ّh@���@�(�@�  @��m@��;@�ƨ@׮@�
=@�J@���@�1@��@ӕ�@ҏ\@љ�@�Ĝ@�A�@�(�@�ƨ@���@�~�@�M�@�5?@��#@͑h@�X@̼j@��m@˝�@�ȴ@�V@ɺ^@�X@�?}@���@�A�@ǶF@�"�@�M�@ě�@Õ�@�t�@�"�@�ff@�o@�|�@�t�@�@�V@�-@��@���@�z�@�l�@���@�n�@���@�
=@���@���@�j@��w@��w@�dZ@��H@���@�ff@�^5@�M�@��@���@�/@��D@�j@�bN@� �@���@�;d@�ȴ@�E�@��#@�?}@��u@�9X@���@�=q@��#@��7@�1'@�;d@��@�@��@��@��y@���@���@��+@�@���@�I�@�1@���@�dZ@��R@�n�@�-@��^@�X@���@�z�@�1'@�|�@�"�@��y@��!@�=q@���@���@���@�`B@��@��/@��j@��j@��9@�b@���@�S�@�C�@�;d@�o@���@���@��+@�5?@���@��-@��7@�X@�/@�%@���@�r�@�9X@��@�b@�1@�  @��;@�|�@���@�ȴ@���@�n�@�{@��@���@��@���@���@�9X@� �@��m@��@�dZ@�@�~�@�M�@�-@��@��-@�/@��@���@��@��D@�j@�1'@��
@���@�dZ@�C�@�
=@��y@���@�=q@�-@��T@�p�@��/@�Z@�b@��m@���@�"�@��@��y@��y@��H@��@��@���@��+@�=q@��@���@�@��7@�?}@��@�Q�@� �@���@��w@�l�@�"�@��H@��R@�~�@�^5@�5?@��@�J@��@���@�V@���@�9X@���@���@�l�@�\)@�+@���@���@���@�-@���@��-@�O�@��@��@���@��@�j@�Z@�Q�@�A�@���@��F@��@�S�@�+@�
=@��H@��@���@�=q@��@�O�@�V@���@��@�A�@�  @|�@�@~�R@~$�@}@}��@}�h@}�@}�@|�j@|��@|�D@|z�@|Z@|Z@|(�@{��@{dZ@z�!@z=q@y7L@x�@xr�@xA�@w��@w�P@wl�@w\)@w;d@w+@vȴ@v$�@u@u/@t��@t1@s�@sdZ@sC�@so@r�H@r�!@q7L@pbN@pbN@pQ�@p1'@p �@o�@o�w@o\)@nȴ@nV@l��@l�@k�m@j��@i��@i��@i��@i�7@ix�@ihs@hb@g;d@g
=@g
=@f��@f�y@f�y@f�+@e��@d�@d(�@cS�@b�@ax�@`��@`r�@_�;@_�@^ff@^@]��@]�-@]`B@\�@\�D@\Z@[��@Z�H@Z�!@Z=q@Y�#@Y�7@YG�@Y�@Xr�@X1'@W�@W�P@Wl�@W;d@W�@V��@VE�@V@U�h@S��@R��@R��@R�\@Rn�@RM�@R�@Q��@Q��@Q�@Q��@QX@Q%@PQ�@OK�@N�y@N��@Mp�@L��@L��@Lz�@L9X@L�@Kƨ@K��@Kt�@KdZ@KdZ@KS�@K33@J^5@I��@H��@H1'@G�@G
=@FE�@E��@E�@E`B@E/@D�@D�D@DI�@CdZ@B��@B~�@Bn�@B^5@BM�@A��@A�#@Ax�@A�@@�`@@Ĝ@@�@@bN@@ �@@  @?|�@?+@>��@>@=�@=V@<Z@<(�@<(�@;��@;�
@;�F@;��@;t�@;"�@:�\@:=q@9�#@9��@9hs@97L@9�@8Ĝ@8�u@8Q�@8A�@8 �@7�@7��@7�@7\)@6v�@5O�@4�@4�j@4j@3�
@2�@2-@1�^@1��@1hs@0�9@/�@/+@.�y@.V@-�@-�h@-`B@-/@-V@,�D@,(�@+"�@*��@*^5@)��@)��@)&�@)%@(�u@(�@(r�@(Q�@(  @'K�@'
=@&��@&�y@&ȴ@&��@&��@&ff@&{@%��@%@%p�@%?}@$�j@$�@#ƨ@#�F@#C�@"�!@"n�@"=q@"�@!�@!��@!�7@!hs@!hs@!7L@!%@ �9@ �@ Q�@ A�@ A�@ A�@  �@  �@ b@�;@�@�P@K�@
=@�y@�y@�y@�y@�y@ȴ@ȴ@�R@��@��@�+@�+@�+@v�@ff@E�@$�@{@��@p�@?}@�/@�j@�@�m@33@�@�!@~�@n�@M�@=q@-@�@J@��@�@�#@�#@��@�^@��@hs@&�@�9@1'@�P@+@��@��@ff@@�T@��@?}@V@j@(�@�m@t�@@�H@�H@��@��@�!@~�@n�@^5@^5@M�@�@��@x�@7L@�@��@Ĝ@bN@A�@b@��@�;@��@�;@��@�w@�@�P@l�@�@��@�y@ȴ@�+@$�@�@��@��@��@��@��@��@@��@�h@�@p�@`B@�@�@�D@I�@(�@(�@�@�@��@�m@ƨ@�F@�F@��@S�@o@
�@
�!@
�\@
n�@
M�@
-@	��@	��@	hs@�9@�u@r�@bN@1'@�@l�@�@v�@V@E�@$�@{@�T@��@��@@@�-@��@�h@p�@`B@`B@O�@?}@V@��@�@��@j@Z@Z@Z@ZA�ȴAϼjAϾwAϺ^AϼjA�A�ƨA�ƨA�ƨA���A���A�ƨA���A���A��
A��A��
A��
A��A��A���A���A���A���A��#A��
A���A���A���A���A���A���A��#A��
A��
A���A���A��#A��HA��HA��`A��TA��TA��HA��/A��/A��HA��TA��`A��HA��#A���A���A���A���A��
A��
A��;A���A϶FAϗ�A�t�A�n�A�p�A�t�A�t�A�t�A�r�A�t�A�x�A�z�A�~�A�~�A�~�A�|�A�~�A�|�A�z�A�x�A�x�A�x�A�x�A�v�A�v�A�v�A�v�A�v�A�t�A�t�A�t�A�r�A�r�A�r�A�p�A�p�A�n�A�p�A�r�A�r�A�r�A�r�A�t�A�t�A�r�A�p�A�p�A�n�A�p�A�r�A�r�A�r�A�t�A�r�A�r�A�p�A�p�A�p�A�n�A�p�A�r�A�t�A�t�A�t�A�t�A�r�A�v�A�v�A�v�A�v�A�v�A�x�A�|�A�|�A�~�AρAσAσAσAσAσAσAσAσAρAρAρAσAσAσAω7Aω7Aω7AϋDAϋDAϋDAϋDAω7AϋDAϋDAϋDAϋDAϋDAϋDAϏ\Aω7AυAυAσAσAσAσAσAυAχ+Aχ+Aχ+AυAυAρAρAρAρAρAσAυAχ+Aω7Aω7Aω7Aω7Aω7Aω7Aω7AϋDAω7AϋDAϋDAω7Aω7Aω7AυAυAσAρAρAρA�z�A�x�A�t�A�r�A�z�A�x�A�r�A�hsA�S�A�M�A�I�A�;dA�1'A�(�A� �A�oA�A�A�A���A���A��A��yA��;A��#A��
A���A���A���A���A�ȴA���A�ƨA�ƨA���Aκ^A���A���A�A�1A�VA��A�(�A�-A�/A�9XA�9XA�/A�{A�1A���A��A��#A���Aκ^AήAΑhA�t�A�ZA�?}A�-A�JAͰ!A͏\A�x�A�n�A�Q�A�9XA�-A��A���Ạ�A�jA�7LA�AˁAʛ�A�ȴA�5?A�;dA�A��#A��A��/A�S�A�
=A��A��7A�K�A��A���A���A�bNA�+A�/A���A���A��9A�v�A�bA��uA��7A��9A��+A�r�A��A�A�A���A���A���A��DA��A�^5A�9XA��!A��\A��mA���A�^5A�=qA�C�A�1A��`A��RA���A��A�;dA��FA�ZA��A�  A��+A��PA�hsA�JA�ZA�A�A��PA�K�A�x�A��PA��\A���A���A���A�C�A�A�n�A�E�A� �A���A�9XA�ƨA��A�bNA���A�G�A���A��-A�ffA�G�A��A��`A��uA�z�A�|�A�|�A�z�A�r�A�I�A��A��HA���A�|�A�33A�A��A�Q�A�A���A��
A�dZA�VA���A�z�A�7LA��A�ȴA���A��A�bNA��A���A��HA��RA�~�A�A�A���A��A�+A�A�O�A���A��HA�1'A���A�r�A�G�A� �A���A��A�?}A���A��wA���A�jA�;dA�  A���A��
A��-A��uA�t�A�9XA�JA���A��A��#A�ƨA��A���A��A�jA�S�A�(�A��A�oA�JA���A��A��mA��#A�A���A�~�A�C�A���A��-A��\A�33A���A�-A��A�oA���A���A�~�A�v�A�bNA�+A�+A��A�
=A�%A�A���A���A��\A�O�A�-A�oA��A��A��-A�z�A�1A�ƨA���A�dZA�?}A�{A��^A�C�A��yA��hA��A��A���A�ƨA���A�l�A�C�A�"�A��HA���A�jA�\)A�M�A�E�A�$�A��;A��^A���A�n�A�K�A�33A�JA��A�A�|�A�A�A�A�r�A�ĜA���A��uA�S�A��A�  A��yA��;A�A��A���A��hA��A�r�A�S�A�=qA�(�A��A�JA��A��
A�ƨA��!A���A�|�A�r�A�dZA�M�A�;dA�(�A�oA��jA�ĜA�Q�A�"�A���A��A�G�A�  Ax�A33A~�A~v�A~ZA~$�A~JA}�mA}��A}dZA}G�A}"�A|�/A|n�Az�yAz��Az~�AzbNAz5?Ay�TAy�#Ay�Ay�hAy|�Ayt�AyO�Ay;dAy�Ax�jAxȴAx��Ax��Ax��Ax�+Axr�AxffAxjAxA�AxA�Ax9XAx9XAw�Aw��Aw�Awl�Aw?}Av��Av�!Av�+AvI�Av{Av{Au��Au�TAu�#AuƨAuAu�wAu�Au��Au�hAu�hAu�Aux�Au"�At  Ar  Ap�Ao�#Ao`BAoO�AoO�AoK�AoG�Ao&�Ao%An�AnM�An  Am�hAmS�Am
=Al�jAl��Al~�AlbNAlVAl-Akx�Aj9XAi�Ai+Ah�Ah-Ag�Af9XAe�PAd��Ac�TAc��AcXAc?}Ac&�Ac�AcAb��Ab�Ab�AbE�Aal�A`��A`$�A_`BA_/A_�A^��A^��A^z�A^~�A^VA^(�A]A]��A]O�A];dA\�yA\��A\z�A\jA\ �A[��A[��A[�hA["�AYS�AXjAV��AV^5AV �AVJAU�
AU��AUdZAUG�AU"�AU
=AT��AT��AT�HATȴAT�\ATffATbAS�
AS�7ASK�ASoAR�HARn�AR9XAR{AQƨAQO�AP�AP{AO��ANM�AL��AL�\ALE�AK��AK��AKdZAK&�AJ�jAJVAJ�AJAI��AIAI��AIp�AI/AH��AHȴAH��AH�uAHI�AG�TAG�AGS�AGVAFĜAF��AF�AFbNAFI�AFJAE�AE�wAE�AEG�AE+AEAD��AC�TAB��AB�uAB~�AB�ABv�ABjABVABI�AB=qAB1'ABJABAA�AA��AAt�AA+AAoA@�A@�A@�`A@��A@ZA@1A?�A?A?�A?��A?��A?��A?�7A?�PA?�7A?�7A?�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                  AϾwA�A�ƨA���A���A��
A��A���A���A���A��
A���A��/A��HA��HA���A���A�x�A�x�A�z�A�v�A�t�A�r�A�r�A�r�A�t�A�|�AρAυAϋDAω7AυAσAχ+Aω7AρA�ffA��A��TA�ȴA��A�JA�x�A�l�A˶FA�A�dZA�t�A���A�K�A�JA�JA��A�(�A��/A�33A�ĜA��A�(�A�x�A�ƨA� �A��A��\A��9A�?}A�9XA�hsA��wA�~�A�ffA�ĜA��A�{A�5?A��\A���A��A��A��wA~jA{p�AyK�Axr�Aw33Au��Ar�/An�uAk�Af{Ab��A^��A]33AY��AU|�ATA�AQ�TAL�HAI�AHbAF-AC�#ABA�AA;dA?�TA?|�A?33A=�A;�A8A�A6�!A5�A5VA4jA4$�A3�PA2��A1�TA0��A/�#A.�yA-��A-G�A,��A,��A,��A,bA)O�A( �A&�jA%��A$n�A"��A"  A!��A ��A JA��A�9AbNA\)A;dA�A��AA�A��A�\A�A�A�TA�AQ�AZAv�A$�A�A�;Az�A�hAp�A�9A�A�AȴA1A%A�+A+A(�A=qAC�A�-AƨAXAn�A��Al�A �A��AffA  A��A�`A  A�A��A�A=qA�;A|�A
�/A
�+A
$�A	�hA	
=A9XA{An�A�A�AA��Al�A�uA5?A9XAI�AM�AA�AA�A��A
=A"�A\)AO�A��Ar�A�A|�A�\A�FA�7A%A ��A =qA (�@���@��@��/@�Z@�=q@�x�@��`@��w@��R@�{@�@�`B@���@���@� �@�F@�"�@��@�7@�G�@�  @�  @��m@�F@�;d@�o@��H@�n�@��#@�7L@���@�@�ƨ@�R@��@�w@�n�@���@�G�@��`@�\)@���@�@�@��H@⟾@�^5@��@��/@��u@�1@ߥ�@�t�@�C�@���@�ff@��@�p�@��`@���@ܛ�@��@ۅ@�;d@�@�v�@��@ّh@���@�(�@�  @��m@��;@�ƨ@׮@�
=@�J@���@�1@��@ӕ�@ҏ\@љ�@�Ĝ@�A�@�(�@�ƨ@���@�~�@�M�@�5?@��#@͑h@�X@̼j@��m@˝�@�ȴ@�V@ɺ^@�X@�?}@���@�A�@ǶF@�"�@�M�@ě�@Õ�@�t�@�"�@�ff@�o@�|�@�t�@�@�V@�-@��@���@�z�@�l�@���@�n�@���@�
=@���@���@�j@��w@��w@�dZ@��H@���@�ff@�^5@�M�@��@���@�/@��D@�j@�bN@� �@���@�;d@�ȴ@�E�@��#@�?}@��u@�9X@���@�=q@��#@��7@�1'@�;d@��@�@��@��@��y@���@���@��+@�@���@�I�@�1@���@�dZ@��R@�n�@�-@��^@�X@���@�z�@�1'@�|�@�"�@��y@��!@�=q@���@���@���@�`B@��@��/@��j@��j@��9@�b@���@�S�@�C�@�;d@�o@���@���@��+@�5?@���@��-@��7@�X@�/@�%@���@�r�@�9X@��@�b@�1@�  @��;@�|�@���@�ȴ@���@�n�@�{@��@���@��@���@���@�9X@� �@��m@��@�dZ@�@�~�@�M�@�-@��@��-@�/@��@���@��@��D@�j@�1'@��
@���@�dZ@�C�@�
=@��y@���@�=q@�-@��T@�p�@��/@�Z@�b@��m@���@�"�@��@��y@��y@��H@��@��@���@��+@�=q@��@���@�@��7@�?}@��@�Q�@� �@���@��w@�l�@�"�@��H@��R@�~�@�^5@�5?@��@�J@��@���@�V@���@�9X@���@���@�l�@�\)@�+@���@���@���@�-@���@��-@�O�@��@��@���@��@�j@�Z@�Q�@�A�@���@��F@��@�S�@�+@�
=@��H@��@���@�=q@��@�O�@�V@���@��@�A�@�  @|�@�@~�R@~$�@}@}��@}�h@}�@}�@|�j@|��@|�D@|z�@|Z@|Z@|(�@{��@{dZ@z�!@z=q@y7L@x�@xr�@xA�@w��@w�P@wl�@w\)@w;d@w+@vȴ@v$�@u@u/@t��@t1@s�@sdZ@sC�@so@r�H@r�!@q7L@pbN@pbN@pQ�@p1'@p �@o�@o�w@o\)@nȴ@nV@l��@l�@k�m@j��@i��@i��@i��@i�7@ix�@ihs@hb@g;d@g
=@g
=@f��@f�y@f�y@f�+@e��@d�@d(�@cS�@b�@ax�@`��@`r�@_�;@_�@^ff@^@]��@]�-@]`B@\�@\�D@\Z@[��@Z�H@Z�!@Z=q@Y�#@Y�7@YG�@Y�@Xr�@X1'@W�@W�P@Wl�@W;d@W�@V��@VE�@V@U�h@S��@R��@R��@R�\@Rn�@RM�@R�@Q��@Q��@Q�@Q��@QX@Q%@PQ�@OK�@N�y@N��@Mp�@L��@L��@Lz�@L9X@L�@Kƨ@K��@Kt�@KdZ@KdZ@KS�@K33@J^5@I��@H��@H1'@G�@G
=@FE�@E��@E�@E`B@E/@D�@D�D@DI�@CdZ@B��@B~�@Bn�@B^5@BM�@A��@A�#@Ax�@A�@@�`@@Ĝ@@�@@bN@@ �@@  @?|�@?+@>��@>@=�@=V@<Z@<(�@<(�@;��@;�
@;�F@;��@;t�@;"�@:�\@:=q@9�#@9��@9hs@97L@9�@8Ĝ@8�u@8Q�@8A�@8 �@7�@7��@7�@7\)@6v�@5O�@4�@4�j@4j@3�
@2�@2-@1�^@1��@1hs@0�9@/�@/+@.�y@.V@-�@-�h@-`B@-/@-V@,�D@,(�@+"�@*��@*^5@)��@)��@)&�@)%@(�u@(�@(r�@(Q�@(  @'K�@'
=@&��@&�y@&ȴ@&��@&��@&ff@&{@%��@%@%p�@%?}@$�j@$�@#ƨ@#�F@#C�@"�!@"n�@"=q@"�@!�@!��@!�7@!hs@!hs@!7L@!%@ �9@ �@ Q�@ A�@ A�@ A�@  �@  �@ b@�;@�@�P@K�@
=@�y@�y@�y@�y@�y@ȴ@ȴ@�R@��@��@�+@�+@�+@v�@ff@E�@$�@{@��@p�@?}@�/@�j@�@�m@33@�@�!@~�@n�@M�@=q@-@�@J@��@�@�#@�#@��@�^@��@hs@&�@�9@1'@�P@+@��@��@ff@@�T@��@?}@V@j@(�@�m@t�@@�H@�H@��@��@�!@~�@n�@^5@^5@M�@�@��@x�@7L@�@��@Ĝ@bN@A�@b@��@�;@��@�;@��@�w@�@�P@l�@�@��@�y@ȴ@�+@$�@�@��@��@��@��@��@��@@��@�h@�@p�@`B@�@�@�D@I�@(�@(�@�@�@��@�m@ƨ@�F@�F@��@S�@o@
�@
�!@
�\@
n�@
M�@
-@	��@	��@	hs@�9@�u@r�@bN@1'@�@l�@�@v�@V@E�@$�@{@�T@��@��@@@�-@��@�h@p�@`B@`B@O�@?}@V@��@�@��@j@Z@Z@Z@ZA�ȴAϼjAϾwAϺ^AϼjA�A�ƨA�ƨA�ƨA���A���A�ƨA���A���A��
A��A��
A��
A��A��A���A���A���A���A��#A��
A���A���A���A���A���A���A��#A��
A��
A���A���A��#A��HA��HA��`A��TA��TA��HA��/A��/A��HA��TA��`A��HA��#A���A���A���A���A��
A��
A��;A���A϶FAϗ�A�t�A�n�A�p�A�t�A�t�A�t�A�r�A�t�A�x�A�z�A�~�A�~�A�~�A�|�A�~�A�|�A�z�A�x�A�x�A�x�A�x�A�v�A�v�A�v�A�v�A�v�A�t�A�t�A�t�A�r�A�r�A�r�A�p�A�p�A�n�A�p�A�r�A�r�A�r�A�r�A�t�A�t�A�r�A�p�A�p�A�n�A�p�A�r�A�r�A�r�A�t�A�r�A�r�A�p�A�p�A�p�A�n�A�p�A�r�A�t�A�t�A�t�A�t�A�r�A�v�A�v�A�v�A�v�A�v�A�x�A�|�A�|�A�~�AρAσAσAσAσAσAσAσAσAρAρAρAσAσAσAω7Aω7Aω7AϋDAϋDAϋDAϋDAω7AϋDAϋDAϋDAϋDAϋDAϋDAϏ\Aω7AυAυAσAσAσAσAσAυAχ+Aχ+Aχ+AυAυAρAρAρAρAρAσAυAχ+Aω7Aω7Aω7Aω7Aω7Aω7Aω7AϋDAω7AϋDAϋDAω7Aω7Aω7AυAυAσAρAρAρA�z�A�x�A�t�A�r�A�z�A�x�A�r�A�hsA�S�A�M�A�I�A�;dA�1'A�(�A� �A�oA�A�A�A���A���A��A��yA��;A��#A��
A���A���A���A���A�ȴA���A�ƨA�ƨA���Aκ^A���A���A�A�1A�VA��A�(�A�-A�/A�9XA�9XA�/A�{A�1A���A��A��#A���Aκ^AήAΑhA�t�A�ZA�?}A�-A�JAͰ!A͏\A�x�A�n�A�Q�A�9XA�-A��A���Ạ�A�jA�7LA�AˁAʛ�A�ȴA�5?A�;dA�A��#A��A��/A�S�A�
=A��A��7A�K�A��A���A���A�bNA�+A�/A���A���A��9A�v�A�bA��uA��7A��9A��+A�r�A��A�A�A���A���A���A��DA��A�^5A�9XA��!A��\A��mA���A�^5A�=qA�C�A�1A��`A��RA���A��A�;dA��FA�ZA��A�  A��+A��PA�hsA�JA�ZA�A�A��PA�K�A�x�A��PA��\A���A���A���A�C�A�A�n�A�E�A� �A���A�9XA�ƨA��A�bNA���A�G�A���A��-A�ffA�G�A��A��`A��uA�z�A�|�A�|�A�z�A�r�A�I�A��A��HA���A�|�A�33A�A��A�Q�A�A���A��
A�dZA�VA���A�z�A�7LA��A�ȴA���A��A�bNA��A���A��HA��RA�~�A�A�A���A��A�+A�A�O�A���A��HA�1'A���A�r�A�G�A� �A���A��A�?}A���A��wA���A�jA�;dA�  A���A��
A��-A��uA�t�A�9XA�JA���A��A��#A�ƨA��A���A��A�jA�S�A�(�A��A�oA�JA���A��A��mA��#A�A���A�~�A�C�A���A��-A��\A�33A���A�-A��A�oA���A���A�~�A�v�A�bNA�+A�+A��A�
=A�%A�A���A���A��\A�O�A�-A�oA��A��A��-A�z�A�1A�ƨA���A�dZA�?}A�{A��^A�C�A��yA��hA��A��A���A�ƨA���A�l�A�C�A�"�A��HA���A�jA�\)A�M�A�E�A�$�A��;A��^A���A�n�A�K�A�33A�JA��A�A�|�A�A�A�A�r�A�ĜA���A��uA�S�A��A�  A��yA��;A�A��A���A��hA��A�r�A�S�A�=qA�(�A��A�JA��A��
A�ƨA��!A���A�|�A�r�A�dZA�M�A�;dA�(�A�oA��jA�ĜA�Q�A�"�A���A��A�G�A�  Ax�A33A~�A~v�A~ZA~$�A~JA}�mA}��A}dZA}G�A}"�A|�/A|n�Az�yAz��Az~�AzbNAz5?Ay�TAy�#Ay�Ay�hAy|�Ayt�AyO�Ay;dAy�Ax�jAxȴAx��Ax��Ax��Ax�+Axr�AxffAxjAxA�AxA�Ax9XAx9XAw�Aw��Aw�Awl�Aw?}Av��Av�!Av�+AvI�Av{Av{Au��Au�TAu�#AuƨAuAu�wAu�Au��Au�hAu�hAu�Aux�Au"�At  Ar  Ap�Ao�#Ao`BAoO�AoO�AoK�AoG�Ao&�Ao%An�AnM�An  Am�hAmS�Am
=Al�jAl��Al~�AlbNAlVAl-Akx�Aj9XAi�Ai+Ah�Ah-Ag�Af9XAe�PAd��Ac�TAc��AcXAc?}Ac&�Ac�AcAb��Ab�Ab�AbE�Aal�A`��A`$�A_`BA_/A_�A^��A^��A^z�A^~�A^VA^(�A]A]��A]O�A];dA\�yA\��A\z�A\jA\ �A[��A[��A[�hA["�AYS�AXjAV��AV^5AV �AVJAU�
AU��AUdZAUG�AU"�AU
=AT��AT��AT�HATȴAT�\ATffATbAS�
AS�7ASK�ASoAR�HARn�AR9XAR{AQƨAQO�AP�AP{AO��ANM�AL��AL�\ALE�AK��AK��AKdZAK&�AJ�jAJVAJ�AJAI��AIAI��AIp�AI/AH��AHȴAH��AH�uAHI�AG�TAG�AGS�AGVAFĜAF��AF�AFbNAFI�AFJAE�AE�wAE�AEG�AE+AEAD��AC�TAB��AB�uAB~�AB�ABv�ABjABVABI�AB=qAB1'ABJABAA�AA��AAt�AA+AAoA@�A@�A@�`A@��A@ZA@1A?�A?A?�A?��A?��A?��A?�7A?�PA?�7A?�7A?�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	B	 �B	�B	�B	�B	B	�B	�B	MB	�B	{B	�B	MB	�B	�B	oB��B�	B�B�rB��B�B��B��B�8B��B�DB��B�PB�"B�VB�PB�PB�"B��B�]B��B	�B	�B	N�B	�B
<�B
��B
�ZB
��B
ÖB
�6B
�mB
�B
�aB
ܒB
�jB
�sB
�B�BK�B@B,=B�B�B �B��B�HB��B��B�'B��Bd�BGEB3�BqB
�B
�qB
t�B
@�B
-�B
�B
{B
B	�B	�B	̘B	ŢB	�3B	�'B	��B	�B	�@B	��B	t�B	c�B	S�B	I�B	C�B	+6B	#�B	�B	DB�PB��B��B�B�gB�sBچBخB�yBߤB�QBɆB�tB��BȴB�zB�EB��BǮB�XB��BȀB��B�XB�B՛B�[BѷB� B��B�<B�[B��B�EBȀB��B�zB�zB��B�B�[B��B�B�|B��B	+B	MB	,B	<�B	P�B	DgB	K�B	h�B	d�B	a�B	m�B	xlB	x�B	v�B	z�B	��B	��B	��B	��B	�wB	�B	��B	��B	�B	��B	��B	�zB	��B	�2B	��B	�B	��B	�B	�yB	�
B	�,B	�6B	��B	�B	�)B	��B	�B	��B	�&B	֡B	уB	�B	�jB	��B	�B	ȀB	�^B	ɆB	�KB	ΥB	��B	��B	��B	ܒB	�]B	��B	�QB	��B	ںB	��B	��B	�/B	�HB	�B	�2B	�8B	�B	��B	�+B	�B	�%B	�ZB	��B	��B	�cB	��B	�]B	��B	�B	�)B	��B	��B	�B	�cB	� B	�B	�DB	�mB	��B	��B	��B	�B	� B	�B	� B	�B	� B	�vB	�vB	�B	�B	�2B	�B	�)B	��B	�]B	�)B	�)B	�)B	��B	�"B	�WB	�)B	�B	��B	�DB	�yB	��B	�KB	�B	�]B	�)B	��B	�cB	�B	�B	�AB	�MB	�|B	��B	�B	�B	��B	�ZB	��B	��B	��B	��B	��B	��B	��B	�PB	��B	�B	��B	��B	��B	��B	�(B	�VB	��B	�VB	��B	��B	�"B	�]B	��B	�]B	��B	�xB	�JB	��B	�"B	�"B	��B	�B	�B	��B	��B	�xB	��B	��B	��B	�fB	�`B	�B	�B	�B	�B	�;B	��B	�B	�)B	�WB	��B	�B	�QB	�B	�B	�B	��B	�`B	�B	�B	�%B	�`B	�ZB	��B	�B	�B	�|B	��B	�B	��B	�)B	�B	�DB	�B	�QB	�B	�B	�QB	��B	�B	��B	�B	��B	��B	��B	�B	�)B	��B	��B	�WB	�)B	�)B	�cB	��B	�B	��B	�MB	�B	��B	��B	��B	�ZB	�B	��B	�JB	��B	��B	��B	�B	�B	�B	�B	��B
B
�B
;B
uB
�B
MB
B
�B
B
�B
_B
_B
�B
	B
	lB
	lB
	�B
B
DB
xB
�B
B
�B
B
B
B
�B
�B
(B
\B
(B
\B
�B
�B
.B
.B
�B
hB
hB
�B
�B
B
B
B
�B
B
B
�B
B
B
uB
�B
�B
�B
�B
�B
�B
�B
�B
YB
_B
�B
eB
�B
1B
�B
�B
�B
=B
	B
=B
	B
CB
B
IB
B
�B
�B
�B
�B
�B
!B
!B
VB
�B
�B
 \B
VB
!B
�B
 �B
 �B
!�B
 �B
!bB
!�B
#:B
#�B
#�B
#�B
#nB
#�B
#�B
#�B
$�B
%zB
&�B
&LB
&�B
'�B
'�B
(XB
)�B
(�B
)�B
*0B
*�B
+B
+kB
+�B
+�B
,B
,B
,=B
,B
,B
,�B
-�B
.B
.}B
.�B
/�B
/�B
/OB
.�B
.�B
.�B
.}B
/�B
.�B
/B
/�B
.}B
/OB
/�B
/�B
/�B
0!B
/�B
0!B
0�B
0�B
0�B
1�B
1'B
0�B
0�B
0�B
1[B
1�B
1�B
49B
3�B
4B
4�B
5B
6zB
5�B
6FB
6�B
7LB
7�B
7�B
7B
7�B
8�B
8RB
8�B
8�B
7�B
8B
7�B
8B
7�B
8RB
9$B
:*B
;0B
;0B
:�B
;�B
<6B
<6B
;�B
;�B
;�B
<B
<�B
<�B
<jB
=�B
=�B
>�B
>wB
>BB
>wB
?HB
>�B
>B
A B
@�B
@�B
A B
A B
@OB
@B
@OB
@�B
@OB
@�B
B[B
A�B
A�B
C�B
B'B
A�B
A�B
A�B
A�B
@�B
A B
A�B
A�B
B'B
B�B
B�B
B'B
B�B
CaB
DgB
EB
F?B
FtB
F�B
F�B
GEB
G�B
IB
H�B
IB
IRB
IRB
IB
IRB
J#B
JXB
K�B
L�B
LdB
MjB
N<B
OB
OvB
OvB
P�B
P}B
PHB
P}B
P}B
P�B
QB
QB
QB
P�B
Q�B
U2B
UgB
T�B
T�B
T�B
T�B
T�B
T�B
T�B
UgB
UgB
U2B
V9B
V�B
XB
XB
W�B
YB
YB
YB
ZB
YB
YB
ZB
Z�B
Z�B
ZQB
Y�B
ZB
ZQB
Z�B
ZB
\)B
\)B
\�B
\�B
]�B
]dB
\�B
\�B
]dB
]�B
^B
^jB
`B
_�B
_�B
_�B
_�B
`B
`vB
_�B
`BB
`�B
aB
aHB
`�B
`�B
`�B
`�B
a�B
bB
bNB
b�B
c�B
cTB
d�B
dZB
c�B
d�B
dZB
dZB
d�B
d&B
d�B
e�B
e`B
e�B
e�B
f2B
e�B
e�B
f2B
f�B
g8B
g8B
gmB
gmB
g�B
g�B
g�B
h>B
iB
i�B
iyB
i�B
i�B
i�B
kQB
j�B
kQB
j�B
lWB
l�B
l�B
l�B
m�B
n/B
ncB
n/B
ncB
n�B
o B
o B
p�B
pB
p�B
qB
qB
q�B
q�B
rGB
rB
rGB
rGB
rGB
sMB
sB
r�B
sMB
r�B
sMB
sMB
s�B
s�B
s�B
s�B
tB
sMB
t�B
uZB
t�B
t�B
u�B
v+B
u�B
v�B
v�B
v`B
w2B
w�B
v�B
v�B
wfB
w2B
w�B
w�B
xB
x8B
x�B
x8B
x�B
xB
x�B
x�B
y	B
x�B
y>B
yrB
y�B
yrB
y>B
y�B
y>B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
zB
y�B
zDB
y�B
z�B
z�B
z�B
{B
{B
z�B
|�B
|�B
|�B
}"B
}�B
}"B
}�B
}VB
}VB
}�B
}�B
}�B
}�B
~(B
~(B
~�B
~(B
}�B
~(B
~�B
~�B
� B
�4B
�iB
�iB
�B
�;B
��B
��B
��B
�AB
�uB
�B
�B
�{B
�B
�MB
��B
��B
��B
��B
��B
�B
�B
�B
��B
��B
�B
��B
��B
�YB
�YB
�%B
�YB
�_B
��B
��B
��B
��B
��B
��B
�_B
�1B
��B
��B
�1B
��B
��B
�fB
�B
��B
��B
�lB
��B
��B
��B
��B
��B
��B
�lB
�=B
��B
��B
��B
�=B
��B
�B
��B
�xB
�xB
�DB
��B
�B
��B
�xB
�B
�B
�xB
��B
�~B
�B
�JB
��B
�B
��B
�B
��B
��B
��B
��B
��B
��B
�(B
��B
��B
�\B
�.B
��B
� B
�4B
��B
��B
�4B
��B
�hB
��B
�4B
��B
�B
��B
��B
�:B
�:B
�B
��B
��B
�:B
��B
�B
�uB
��B
�B
�B
��B
�uB	�B	B	B	;B	;B�.B	B	�B	 iB��B	YB	B	{B	�B	B	�B	{B	GB	�B	�B	B	MB	�B	�B	B	MB	�B	�B	�B	�B	�B	uB	�B	GB	{B	B	%B	uB	�B	B	{B	�B	�B	�B	�B	B	B	B	uB	�B	{B	MB	uB	{B	  B	 �B	B�(B	B��B	GB��B��B��B�+B��B��B��B��B��B�	B��B��B��B�lB�lB�	B�>B�B�B��B��B�B�8B�2B�2B��B��B�B��B�lB�lB��B��B��B�>B��B�B�B�fB��B��B��B��B�8B��B��B�lB��B�fB�2B��B�2B��B�B��B�	B�rB��B�B�B��B�B�8B�	B�8B�DB��B��B�xB�>B��B��B��B��B�B�B��B�xB��B�B��B��B��B��B�VB��B��B��B��B��B��B�PB�PB��B��B��B��B��B��B��B��B�B�VB��B��B��B�"B�"B��B�PB��B�B�B�JB��B��B�PB��B�"B��B�"B��B��B�PB��B��B�PB�]B��B�cB��B��B�(B��B�PB��B��B��B��B��B�(B��B�.B�VB��B	 4B�.B��B��B�(B	 4B	;B	 �B	B��B��B��B�cB�(B	 �B	GB	SB	SB	YB	1B	
�B	�B	(B	uB	�B	B	�B	!�B	#nB	%B	-�B	E�B	O�B	XEB	gmB	��B	��B	�B	��B	� B	��B	�B	�&B	��B	�B	�B	��B
	7B
>�B
OvB
VmB
ffB
zB
��B
��B
�}B
�B
�6B
�wB
��B
��B
�5B
��B
��B
�>B
��B
��B
��B
�vB
�B
�ZB
��B
��B
�`B
�B
�B
خB
��B
�PB
̘B
��B
��B
��B
�$B
�[B
��B
��B
��B
��BkB
چB
�9B
�B,qB
�B
��B
��B
��B
��B
�^B
� B
�B
�zB
��B
�2B
�)B
�B
��B
چB
�OB
�6B
�dB
��B
�[B
�QB
�DB
��B
�B
֡B
��B
�#B
�B
�HB
��B
�B
�B
�B
�8B
�]B
�/B
ҽB
�;B
ȴB
�6BYBC�B��B�6BSBB@OB=�B;�B=�BD�B]/Bi�B<B6FB?�BR�B=�B>�B>B9XB*�B"hBBeBhBDB"BPB�BuB 4BB �BuB�B�B�B��B�B��B��BAB�B��B 'B��B͟B�B��B��B�jB��B��B��B��B��B��B�6B��B��B�}B��B�B�B�hB��B��B�B��B�!B~�B~�BwfBqvBoiBiDB\)B^�BU2BR BS&BNBK)BC�BL�BD�BB�BC�BD�B<�B;0B9�B:*B:^B5tB8B5tB5B4�B3�B+�B)�B(�B(XB#�B �B�B!�B�B=BxB B@B{B�BDB
��B
��B
�B
چB
��B
�&B
�KB
�KB
�<B
��B
��B
�zB
�B
��B
�nB
�3B
�B
�kB
��B
��B
��B
�{B
�$B
�uB
�B
~]B
xlB
~(B
j�B
sMB
lWB
l�B
^jB
^B
V9B
HKB
EB
A B
H�B
C�B
B[B
@OB
@B
?�B
6B
2�B
2-B
/�B
5�B
:�B
0�B
,�B
0�B
,qB
*0B
'RB
$�B
%B
 'B
B
B
+kB
 \B
VB
�B
�B
�B
;B
B
�B
{B
SB
�B
�B
{B
SB
+B
YB
B
B
�B
B
 �B
SB	�cB	��B
AB	��B
 �B
�B	�B	�VB	�DB
B
�B	��B	�B	�DB	�B	�>B	�B	�|B	چB	��B	�?B	��B	�9B	�B	��B	�B	�BB	�B	�B	�pB	�TB	�KB	ȀB	��B	ŢB	��B	��B	��B	�?B	�B	�-B	�tB	�B	�zB	��B	��B	��B	��B	��B	�zB	��B	�gB	��B	� B	��B	��B	�-B	� B	ɺB	�EB	�?B	��B	�<B	ǮB	�aB	��B	��B	��B	��B	�$B	�*B	��B	��B	�FB	��B	��B	��B	�tB	�B	�nB	�3B	��B	�B	�B	��B	�[B	��B	�+B	��B	��B	�B	�$B	�FB	��B	�uB	��B	��B	��B	��B	�B	�iB	~�B	~�B	{JB	~(B	�JB	v�B	{B	tB	o5B	�_B	y�B	��B	wfB	{JB	s�B	kQB	kQB	hsB	bNB	e�B	b�B	`vB	_;B	`B	o�B	`�B	bB	f2B	OB	UgB	W�B	]�B	L0B	Q�B	IB	QB	M�B	M6B	L�B	Q�B	IRB	N�B	D�B	E�B	EmB	CaB	?HB	B�B	>BB	C�B	^B	;dB	U�B	3�B	.}B	,�B	/�B	-B	,qB	(�B	*0B	'�B	&LB	$�B	%�B	&B	&�B	%zB	$�B	$@B	VB	!�B	B	B	qB	B	�B	�B	SB	�B	�B	�B	+kB	B	+B	�B	B	�B	;B	 �B	 �B	AB��B�B�DB�rB��B�xB��B��B�>B�B��B�|B��B�>B�B�B�B�B�B�B��B�vB�TB��B��B�B��BٴB��B��B�5B�B��BӏB�gBԕB�mB�gB�BԕB՛B�2B��B�
B�pB֡B֡B�
B�B�2B�yB�5B��BרB��BٴB�BٴB�EB�B�B�yB�yB�EG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                  B�B��B��B��B��B�%B��B��B�YB��B��B��B�YB��B��B�{B��B�B�B�~B�B�B��B��B�DB�B�PB�B�\B�.B�bB�\B�\B�.B� B�iB�B��B	
�B	F�B	�B
4�B
��B
�fB
��B
��B
�BB
�yB
�&B
�mB
ԞB
�vB
�B
�B�BDB8&B$IB�B��B��B��B�TB��B�B�3B��B\�B?QB+�B}B
�B
�}B
l�B
8�B
%�B
�B	��B	�B	�%B	�B	ĤB	��B	�?B	�3B	��B	�B	�LB	y�B	l�B	[�B	K�B	A�B	;�B	#BB	�B	�B	PB�\B��B��BڎB�sB�BҒBкBЅBװB�]B��B��B��B��B��B�QB��B��B�dB��B��B��B�dB� BͧB�gB��B�,B��B�HB�gB��B�QB��B��B��B��B�B� B�gB��BܛB�B��B�7B	YB	$B	4�B	H�B	<sB	C�B	`�B	\�B	Y�B	fB	pxB	p�B	n�B	r�B	��B	��B	��B	�B	��B	�B	��B	��B	� B	��B	��B	��B	��B	�>B	��B	ضB	��B	�B	ЅB	�B	�8B	�BB	��B	�B	�5B	��B	�B	��B	�2B	έB	ɏB	� B	�vB	�B	�)B	��B	�jB	��B	�WB	ƱB	�B	��B	�B	ԞB	�iB	��B	�]B	��B	��B	��B	��B	�;B	�TB	ܛB	�>B	�DB	�B	��B	�7B	�B	�1B	�fB	��B	�B	�oB	� B	�iB	�B	�B	�5B	��B	��B	�B	�oB	�B	�(B	�PB	�yB	��B	��B	��B	۔B	�,B	۔B	�,B	�%B	�,B	؂B	؂B	ضB	�%B	�>B	�B	�5B	�B	�iB	�5B	�5B	�5B	� B	�.B	�cB	�5B	�B	�B	�PB	�B	��B	�WB	�B	�iB	�5B	�B	�oB	�B	�B	�MB	�YB	�B	��B	�B	�B	��B	�fB	�	B	��B	�B	�	B	��B	�B	��B	�\B	��B	�B	��B	�B	� B	��B	�4B	�bB	��B	�bB	��B	��B	�.B	�iB	��B	�iB	�B	�B	�VB	��B	�.B	�.B	��B	�B	�(B	��B	��B	�B	�B	�B	��B	�rB	�lB	��B	�B	�%B	�B	�GB	�B	�B	�5B	�cB	��B	�B	�]B	�B	ݡB	ܛB	��B	�lB	�B	�B	�1B	�lB	�fB	��B	��B	�B	�B	��B	�B	��B	�5B	�B	�PB	�B	�]B	��B	�B	�]B	��B	�B	��B	�"B	��B	��B	��B	�B	�5B	��B	��B	�cB	�5B	�5B	�oB	��B	�B	��B	�YB	��B	��B	��B	�B	�fB	�B	��B	�VB	�B	��B	�B	�B	�B	�(B	�B	� B	�B	��B	�GB	��B	��B	�YB	�%B	��B	�+B	��B	�kB	�kB	��B
B
xB
xB
�B
B
PB
�B
�B
!B
�B
'B
'B
'B
�B
�B
4B
hB
4B
hB
�B
�B
:B
:B
�B
	tB
	tB
	�B
	�B

B

B
B

�B
B
B

�B
B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
eB
kB
�B
qB
�B
=B
�B
�B
�B
IB
B
IB
B
OB
!B
UB
!B
�B
�B
�B
�B
�B
-B
-B
bB
�B
�B
hB
bB
-B
�B
�B
B
�B
B
nB
�B
FB
�B
�B
�B
zB
�B
�B
�B
�B
�B
�B
XB
�B
�B
�B
 dB
!�B
!B
!�B
"<B
"�B
#B
#wB
#�B
#�B
$B
$B
$IB
$B
$B
$�B
%�B
& B
&�B
&�B
'�B
'�B
'[B
&�B
&�B
&�B
&�B
'�B
&�B
''B
'�B
&�B
'[B
'�B
'�B
'�B
(-B
'�B
(-B
(�B
(�B
(�B
)�B
)3B
(�B
(�B
(�B
)gB
)�B
*B
,EB
+�B
,B
,�B
-B
.�B
-�B
.RB
.�B
/XB
/�B
/�B
/#B
/�B
0�B
0^B
0�B
0�B
/�B
0)B
/�B
0)B
/�B
0^B
10B
26B
3<B
3<B
2�B
3�B
4BB
4BB
3�B
3�B
3�B
4B
4�B
4�B
4vB
5�B
5�B
6�B
6�B
6NB
6�B
7TB
6�B
6B
9,B
8�B
8�B
9,B
9,B
8[B
8&B
8[B
8�B
8[B
8�B
:gB
9�B
9�B
;�B
:3B
9�B
9�B
9�B
9�B
8�B
9,B
9�B
9�B
:3B
:�B
:�B
:3B
:�B
;mB
<sB
=B
>KB
>�B
>�B
>�B
?QB
?�B
A)B
@�B
A)B
A^B
A^B
A)B
A^B
B/B
BdB
C�B
D�B
DpB
EvB
FHB
GB
G�B
G�B
H�B
H�B
HTB
H�B
H�B
H�B
I&B
I&B
I&B
H�B
I�B
M>B
MsB
M
B
L�B
L�B
M
B
M
B
M
B
L�B
MsB
MsB
M>B
NEB
N�B
PB
PB
O�B
Q�B
Q#B
Q�B
R)B
Q�B
Q�B
R)B
R�B
R�B
R]B
Q�B
R)B
R]B
R�B
R)B
T5B
T5B
T�B
T�B
U�B
UpB
T�B
UB
UpB
U�B
VB
VvB
XB
W�B
W�B
W�B
W�B
XB
X�B
W�B
XNB
X�B
YB
YTB
X�B
X�B
X�B
X�B
Y�B
Z%B
ZZB
Z�B
[�B
[`B
\�B
\fB
[�B
\�B
\fB
\fB
\�B
\2B
\�B
]�B
]lB
]�B
]�B
^>B
]�B
^
B
^>B
^�B
_DB
_DB
_yB
_yB
_�B
_�B
_�B
`JB
aB
a�B
a�B
a�B
a�B
a�B
c]B
b�B
c]B
b�B
dcB
d�B
e B
e B
e�B
f;B
foB
f;B
foB
f�B
gB
gB
h�B
hB
h�B
iB
iB
i�B
i�B
jSB
jB
jSB
jSB
jSB
kYB
k%B
j�B
kYB
j�B
kYB
kYB
k�B
k�B
k�B
k�B
l+B
kYB
l�B
mfB
l�B
l�B
nB
n7B
nB
n�B
n�B
nlB
o>B
o�B
o	B
o	B
orB
o>B
o�B
o�B
pB
pDB
p�B
pDB
p�B
pB
p�B
p�B
qB
p�B
qJB
q~B
q�B
q~B
qJB
q�B
qJB
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
rB
q�B
rPB
q�B
r�B
r�B
r�B
s�B
s"B
r�B
t�B
t�B
t�B
u.B
u�B
u.B
u�B
ubB
ubB
v B
v B
u�B
u�B
v4B
v4B
v�B
v4B
v B
v4B
v�B
wB
xB
x@B
xuB
xuB
yB
yGB
y�B
y�B
y�B
zMB
z�B
{B
{B
{�B
|%B
|YB
|�B
|�B
|�B
|�B
|�B
}+B
}+B
}+B
|�B
|�B
}+B
}�B
}�B
~eB
~eB
~1B
~eB
kB
B
�B
�	B
�B
�B
�B
kB
�=B
�B
�B
�=B
��B
��B
�rB
�B
��B
��B
�xB
��B
��B
��B
��B
��B
��B
�xB
�IB
��B
��B
��B
�IB
��B
�B
��B
��B
��B
�PB
��B
�B
��B
��B
�!B
�!B
��B
��B
��B
�!B
�VB
��B
�'B
��B
�'B
��B
��B
��B
��B
��B
��B
�4B
��B
��B
�hB
�:B
��B
�B
�@B
��B
��B
�@B
��B
�tB
��B
�@B
��B
�B
��B
��B
�FB
�FB
�B
��B
��B
�FB
��B
�B
��B
��B
�B
�B
��B
��B��B�B�B�GB�GB�:B�B��B�uB��B�eB�B��B��B�B��B��B�SB��B��B�+B�YB��B��B�B�YB��B��B��B��B��B��B��B�SB��B�+B�1B��B��B�%B��B��B��B��B��B�%B�B�B��B��B��B�YB��B��B�B��B�B�4B�B��B�SB��B�	B�B�7B�B�B��B�B�B�B��B�B�B�xB�xB�B�JB�B�B�B��B�B�DB�>B�>B�B��B�B�B�xB�xB��B��B��B�JB�B�B�B�rB�	B�B�	B��B�DB�B��B�xB�B�rB�>B�	B�>B�B�B��B�B�~B��B�B�B��B�B�DB�B�DB�PB��B��B�B�JB�B�B��B��B�"B�B��B�B��B�B��B��B��B��B�bB��B��B��B�B��B��B�\B�\B��B��B��B��B��B�B��B��B�(B�bB��B��B��B�.B�.B��B�\B��B�(B�B�VB��B�B�\B��B�.B��B�.B��B��B�\B�B��B�\B�iB��B�oB�B�B�4B��B�\B��B��B��B��B� B�4B��B�:B�bB��B�@B�:B��B��B�4B�@B�GB��B�B��B��B��B�oB�4B��B�SB�_B�_B�eB	 =B	�B	�B	4B	�B	�B	B	�B	�B	zB	B	%�B	=�B	G�B	PQB	_yB	z�B	��B	�#B	��B	�,B	��B	�#B	�2B	��B	��B	�%B	��B
CB
6�B
G�B
NyB
^rB
rB
��B
��B
��B
�#B
�BB
��B
��B
��B
�AB
��B
�
B
�JB
�B
�
B
��B
؂B
�B
�fB
�B
��B
�lB
ڎB
��B
кB
�B
�\B
ĤB
��B
�B
��B
�0B
�gB
��B
�B
��B
��BwB
ҒB
�EB
�"B$}B
�)B
��B
�B
��B
��B
�jB
�,B
�B
��B
�B
�>B
�5B
�&B
��B
ҒB
�[B
�BB
�pB
��B
�gB
�]B
�PB
�B
ضB
έB
��B
�/B
�B
�TB
��B
��B
۔B
�B
�DB
�iB
�;B
��B
�GB
��B
�BBQ#B;�B��B�BB_BB8[B5�B3�B5�B<�BU;Ba�B4B.RB7�BJ�B5�B6�B6B1dB"�BtBBqB	tBPB.B\B�B��B�@B�B��B��B��B��B��B��B�(B�B��B�MB�%B�B3B�BūB�B��B��B�vB��B��B��B��B��B��B�BB��B��B��B��B�B�B�tB��B��B�B�B�-BwBv�BorBi�BguBaPBT5BV�BM>BJ,BK2BFBC5B<
BD�B<�B;B<
B<�B4�B3<B1�B26B2jB-�B0)B-�B-B,�B+�B#�B!�B �B dB�B�B�BB�BIB�B	BLB
��B�BPB
��B
��B
�B
ҒB
��B
�2B
�WB
�WB
�HB
��B
��B
��B
�B
��B
�zB
�?B
�#B
�wB
��B
��B
��B
��B
�0B
��B
�B
viB
pxB
v4B
b�B
kYB
dcB
e B
VvB
VB
NEB
@WB
=B
9,B
@�B
<
B
:gB
8[B
8&B
7�B
.B
+B
*9B
'�B
-�B
3B
(�B
$�B
(�B
$}B
"<B
^B
�B
B
3B
'B
!B
#wB
hB
bB	��B	�B	��B	�GB	�B	��B	��B	�_B	��B	��B	��B	�_B	�7B	�eB	�+B	�B	��B	�+B	��B	�_B	�oB	��B	�MB	�B	��B	��B	�B	�bB	�PB	�B
�B	��B	߭B	�PB	�B	�JB	ݡB	وB	ҒB	�B	�KB	��B	�EB	�&B	��B	�&B	�NB	�B	�B	�|B	�`B	�WB	��B	��B	��B	��B	��B	��B	�KB	�#B	�9B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�sB	��B	�,B	�
B	��B	�9B	�,B	��B	�QB	�KB	��B	�HB	��B	�mB	��B	��B	��B	��B	�0B	�6B	��B	��B	�RB	��B	��B	��B	��B	�B	�zB	�?B	��B	�B	ыB	�B	�gB	��B	�7B	��B	��B	�$B	�0B	�RB	��B	��B	��B	��B	~�B	��B	yB	xuB	v�B	wB	sVB	v4B	�VB	n�B	s"B	l+B	gAB	kB	q�B	}�B	orB	sVB	k�B	c]B	c]B	`B	ZZB	]�B	Z�B	X�B	WGB	XB	g�B	X�B	Z%B	^>B	GB	MsB	O�B	U�B	D<B	I�B	A)B	I&B	E�B	EBB	D�B	I�B	A^B	F�B	<�B	=�B	=yB	;mB	7TB	;B	6NB	;�B	VB	3pB	M�B	+�B	&�B	$�B	'�B	%B	$}B	!B	"<B	�B	XB	�B	�B	$B	�B	�B	�B	LB	bB	�B	B	B	}B	B	�B	�B	_B	�B	�B	�B	#wB	B�7B��B�B��B�GB��B��B�MB�B�B�PB�~B�B�B�B��B�JB�B�B�B�B�JB�B�+B�B�B�"B�B��B�B�`B��B��B�B�B��B��B�B�ABыB�
B˛B�sB̡B�yB�sB�B̡BͧB�>B��B�B�|BέBέB�B�B�>BЅB�AB�BϴB��B��BыB��B�QBыB�BЅBЅB�QG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0077649                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0077649                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 23 11 2022 179 -0.0077649 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 23 11 2022 179 -0.0077649 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230426223227                            20230426223227AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023042622322720230426223227  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622322720230426223227QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F83E           783E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622322720230426223227QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               