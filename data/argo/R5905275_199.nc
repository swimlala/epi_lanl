CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-06-21T07:00:56Z creation      
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
resolution        =���   axis      Z        x  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [X   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  c8   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  �   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ɀ   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  �`   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x 0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 7�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x ?�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � _    PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x f�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20230621070056  20230621070056  5905275 5905275 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO7316                            7316                            2B  2B  AA  SOLO_II                         SOLO_II                         8644                            8644                            SBE602 11Jan18                  SBE602 11Jan18                  853 853 @�2L;1$@�2L;1$11  @�2����@�2����@-��a��@-��a���c�:}�O��c�:}�O�11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AA  AB  AB  ?�  ?��H@=p�@}p�@�p�@��R@�  A   A\)A   A+�A>�RA_\)A���A�  A��A��A��A�  A�Q�A�  A��B  BQ�B(�B (�B(Q�B0(�B8  B?�
BG�BO�
BX  B`(�BhQ�Bp  Bx  B�  B��B�{B��B��
B�  B�{B�  B�{B�  B��B�  B�(�B�  B�  B��B��B�  B��B�  B�  B�(�B�z�B�{B��B�  B�  B��B��B��B��B��B��C  C  C  C  C

=C  C  C
=C  C��C  C��C��C��C��C   C"  C$  C&  C(  C)��C,  C.  C0
=C2  C4  C6  C7�C9��C<
=C>  C@  CB  CD  CE��CG��CJ  CL
=CN  CO��CQ��CT
=CV{CX  CY��C\  C^
=C`
=Cb
=Cd  Ce��Cg�Ci�HCk��Cn
=Cp  Cr  Ct  Cv  Cx
=Cz  C{��C~
=C�C�C�  C���C���C���C���C���C�  C�  C�  C�  C�  C�  C�  C���C���C�  C���C�  C�
=C�C�C�C�C�C�  C���C�  C�C�C�  C�
=C�C�  C�C�C�C�C�  C���C�  C�  C���C���C���C�  C���C���C�  C���C���C�C�  C�C�C�C�  C���C�C���C���C�  C�
=C�C���C�  C�C���C���C�C�  C�  C���C���C�  C�C�\C�C�  C�C�  C���C�C�  C�  C�C�C�  C���C���C���C�  C�  C�C�
=C�C�  C���C���C�  C�
=C�  C���C���C�  C�C�C�C�
=C�C�  C���C���C���C�  C�C�  C���C�  C���C���C�  C���C���C���C���C�  D �D ��D  D� D�D� D  D}qD�qDz�D��D}qD�D��D  D� D  D� D�qD	��D
�D
��D  Dz�D��Dz�D�qD}qD�qD}qD  D� D  D��D�D}qD  D� D�qD� D�D}qD  D� D  D��D�D�DD� D  D}qD  D� D�qD}qD�qD� D  D� D�qD}qD  D� D   D }qD �qD!� D"�D"� D#  D#� D$  D$}qD%�D%� D%�qD&}qD&��D'z�D(  D(� D(�qD)}qD*�D*� D*�qD+� D,  D,� D-  D-}qD.  D.� D/  D/��D0D0��D1  D1� D2�D2� D3  D3� D4  D4� D5  D5��D6�D6� D7  D7� D8�D8��D9�D9��D:  D:� D;  D;� D<  D<� D=D=��D>�D>��D?  D?� D@�D@� DA  DA}qDB  DB� DB�qDCz�DC�qDD� DD�qDE� DF�DF��DGDG� DG�qDH� DI  DI� DJ  DJ}qDK  DK��DK�qDL� DM�DM��DN�DN� DN��DO}qDP�DP��DQ  DQ}qDR  DR��DR�qDS� DT  DT}qDU  DU}qDV  DV� DW  DW�DX  DXz�DX�qDY� DZDZ�D[�D[��D\  D\}qD]  D]}qD]�qD^}qD_  D_��D_�qD`}qDa  Da� Db  Db}qDc  Dc��DdDd��De�De� Df  Df� Dg  Dg� Dh  Dh}qDh�qDi}qDi�qDj}qDk  Dk}qDk��Dl� Dl�qDmz�Dm�qDn� Dn�qDo}qDp  Dp� Dq�Dq��Dr�Dr��Dr�qDs}qDt  Dt}qDu  Du�Dv�Dv� Dv�qDw� Dx�Dx��Dy�Dy� Dz  Dz� D{  D{� D{�qD|}qD|�qD}� D~  D~}qD  D��D�  D�@ D��HD��HD���D�@ D�� D���D�  D�@ D�� D�� D�HD�@ D�~�D���D�  D�@ D�� D�� D�HD�@ D�� D��HD�HD�@ D�~�D�� D�HD�B�D��HD���D�  D�@ D��HD�D�  D�>�D�� D�� D�HD�>�D�}qD���D�HD�@ D�}qD��qD�  D�AHD��HD��HD��D�@ D��HD��HD�HD�AHD��HD�D�HD�@ D�~�D���D�  D�=qD�~�D��HD�  D�>�D�� D��HD�  D�>�D�� D�� D���D�>�D��HD��HD���D�>�D�~�D�� D�HD�@ D��HD���D���D�@ D�~�D�� D��D�AHD�~�D�� D��D�AHD�}qD��qD�HD�AHD�~�D��qD�  D�AHD��HD�D�HD�>�D�� D��HD���D�@ D���D��HD�  D�>�D�� D��HD�  D�@ D�~�D�� D��D�AHD�� D���D���D�>�D�~�D��HD�HD�>�D�~�D���D���D�>�D�� D�� D�HD�AHD�� D��qD���D�@ D��HD��HD�  D�=qD�� D��HD�  D�>�D�� D�D�HD�@ D��HD��HD�  D�>�D�}qD���D�HD�AHD��HD��HD�  D�@ D�~�D��HD�HD�@ D�� D���D�HD�AHD��HD��HD�HD�AHD��HD��HD�  D�@ D��HD��HD���D�@ D��HD�� D�  D�AHD��HD�D�  D�@ D�� D��HD��D�AHD�� D�� D�  D�AHD���D�� D���D�@ D���D��HD�  D�=qD�� D��HD�  D�AHD���D�� D�HD�B�D�� D���D���D�AHD��HD���D�  D�AHD�� D�� D�  D�>�D�~�D���D�HD�B�D�� D�� D�HD�@ D�~�D���D�  D�@ D�� D��HD�  D�>�D�� D�� D�HD�AHD�� D���D�HD�@ D�� D�� D�  D�@ D�~�D���D���D�>�D D��HD��D�AHDÀ Dþ�D���D�@ DĀ D�� D�HD�@ D�}qD�� D�HD�AHDƀ D�� D�HD�@ Dǀ D�� D���D�@ DȁHD�� D�HD�@ Dɀ D��HD�  D�AHDʁHD�� D�  D�AHDˁHD�� D�  D�@ D̀ D��HD�HD�@ D�~�D�� D�  D�@ D΁HD��HD���D�>�D�~�DϾ�D�  D�AHDЁHD�� D�  D�@ Dр D��HD���D�=qDҀ D��HD�  D�@ DӁHD��HD�  D�>�D�~�D��HD�HD�AHDՂ�D�� D�  D�@ Dր D�� D�  D�>�D�}qD׾�D�  D�AHD؁HD�D�HD�@ D�~�Dپ�D�  D�@ DځHD��HD�  D�@ Dۀ D�� D�HD�@ D܀ D��HD�  D�AHD݀ Dݾ�D�HD�@ D�~�D޾�D���D�>�D߀ D�� D��qD�=qD�~�D�� D�HD�@ D� DᾸD�HD�@ D� D�D�HD�AHD� D�� D�  D�@ D� D�� D�HD�AHD�~�D�� D�HD�>�D�~�D澸D���D�>�D�~�D�� D�HD�AHD� D�� D���D�@ D� D�� D�HD�AHD�~�D�qD���D�AHD�HD��HD�HD�AHD�HD쾸D���D�@ D�HD��HD�HD�@ D� D��HD�  D�>�D�}qDﾸD�  D�AHD���D��HD���D�@ D� D�D�  D�@ D� D�D��qD�>�D� D�qD���D�@ D�~�D��HD��D�AHD��HD���D��qD�>�D�� D���D�  D�AHD�� D���D��qD�>�D�~�D�� D�  D�@ D���D�D��D�/\?\)?8Q�?W
=?�z�?�p�?�(�?��@��@!G�@0��@@  @Q�@fff@s33@�  @��@���@�
=@�p�@��@���@�33@���@�G�@˅@��@ٙ�@�G�@�=q@�z�@��HAG�A
=A
�HA\)A�
AQ�A{A!�A%A+�A0��A5�A9��A>�RAC�
AH��AMp�AQ�AVffA\(�AaG�AeAi��Ap  Au�Az=qA~{A��A�z�A��RA���A�33A�A�Q�A�=qA�z�A�
=A���A��A�p�A��A��\A��A�\)A�G�A�(�A�\)A��A�z�A�
=A�=qA���A�\)A\A�A�  Aʏ\A��A�Q�Aҏ\A���A׮Aڏ\A��A�\)A��A���A�\)A陚A�(�A�
=A�=qA���A�
=A��A��A��B�B�\B(�Bp�B�RB(�B	B\)B��B{B�BG�B�\B  Bp�B
=B��B�B\)B��B=qB�
B!G�B"�\B$  B%��B'
=B(Q�B)��B+
=B,��B.{B/�B0��B2=qB3�B5G�B6�\B7�
B9G�B:�HB<Q�B=��B>�HB@Q�BB{BC�BD��BF{BG�BI�BJ�RBL  BMG�BN�RBPQ�BQBS33BTz�BUBW\)BX��BZ=qB[�B\��B^ffB`  BaG�Bb�\Bc�
Bep�Bg
=Bhz�BiBk
=BlQ�Bn{Bo�Bp��Br{Bs�Bt��BvffBw�Bx��BzffB{�B}p�B~�RB�  B���B�G�B�{B��HB�p�B�{B��RB�p�B�(�B��HB���B�=qB��HB��B�=qB���B��B�Q�B���B���B�Q�B�
=B��B�Q�B���B��B�ffB�
=B��B�=qB���B��B�Q�B���B��B�=qB��HB��B�(�B��RB�G�B��B���B�G�B��B�ffB���B���B�Q�B��HB�p�B��B�z�B�
=B���B�=qB���B�G�B��B�Q�B��HB��B�{B���B��B���B�Q�B���B���B�=qB���B�p�B�  B��RB�\)B�  B��RB�G�B��B�z�B�33B��
B��\B�33B��
B�ffB�
=B���B�=qB��HB���B�=qB��HB�p�B�  B��RB�\)B�{B��RB�p�B�(�B��RB�p�B�  B¸RB�\)B�(�B���Bř�B�Q�B���BǙ�B�=qB��HBə�B�=qB���BˮB�ffB��B�B�z�B��BϮB�Q�B���BѮB�=qB���Bә�B�=qB���B�\)B��
B�Q�BָRB�
=B�p�B׮B�  B�Q�B؏\B���B�
=B�33B�p�Bٙ�B�B��B�  B�(�B�ffBڣ�B���B���B��B�\)Bۙ�B�B��B�{B�Q�B�ffB܏\BܸRB�
=B�33B�\)Bݙ�B�B��B�(�B�Q�B�z�Bޣ�B���B���B�33B�\)B߅B�B�  B�=qB�z�B�RB���B��B�\)BᙚB�B�  B�(�B�ffB�\B���B�
=B�33B�p�B�B��B�(�B�ffB��B��HB��B�G�B�B�B��B�(�B�ffB��B��HB�33B�p�B�B��B�=qB�\B���B�
=B�G�B�B��
B�(�B�ffB��B���B�33B�B��
B�(�B�ffB��B���B�33B홚B��B�=qB�\B��HB�33B�B��B�Q�B��\B���B�G�B�B�  B�ffB�RB�
=B�p�B�B�(�B�\B���B�\)B��B�{B��\B���B�\)B�B�=qB���B��B���B�  B�z�B���B�\)B��
B�Q�B���B�G�B�B�(�B���B��B��C   C =qC p�C ��C �HC�CQ�C��C��C
=C=qCz�C�RC��C33Cp�C��C�
C{CQ�C�\CC  C33Cp�C��C�HC{CQ�C�C�RC��C(�CffC��C�
C�CQ�C�\C��C	  C	=qC	z�C	�RC	��C
33C
p�C
��C
�HC�CffC��C�HC{CQ�C�\C��C
=CG�Cz�CC��C=qCp�C�C�HC�C\)C�\C��C
=C=qCz�C�RC��C(�CffC��C�HC{CQ�C�\CC  CG�Cz�C�RC��C(�CffC��C�
C{CG�C�C�RC  C=qCz�C�RC��C33Cz�C�RC�C(�CffC��C�
C{CQ�C�\C��C
=CG�C�CC  C=qCz�C�RC��C=qCp�C�C�HC�CQ�C�\C��C
=CG�C�\C�
C�C\)C��C�HC �C \)C ��C �
C!(�C!p�C!�RC!��C"33C"p�C"�C"��C#33C#�C#C$
=C$G�C$�\C$C%  C%G�C%�C%��C&{C&\)C&��C&�HC'(�C'ffC'��C'�HC((�C(p�C(C)
=C)Q�C)�\C)��C*{C*Q�C*��C*�C+33C+z�C+�RC+��C,33C,z�C,C-{C-\)C-��C-�HC.�C.ffC.�C.��C/=qC/�\C/�
C0(�C0p�C0�C0��C1=qC1�\C1�HC233C2z�C2C3
=C3Q�C3��C3��C4=qC4�\C4�
C5(�C5ffC5�C6  C6G�C6��C6�C7=qC7z�C7C8
=C8\)C8�C9  C9Q�C9��C9�HC:(�C:z�C:��C;(�C;p�C;�RC<
=C<Q�C<��C<��C=G�C=��C=�HC>(�C>p�C>C?{C?ffC?�RC@
=C@Q�C@��C@�HCA=qCA�\CA�HCB�CBp�CBCC{CCffCC��CC�CDG�CD��CD�CE33CEz�CECF�CFp�CFCG
=CGQ�CG��CG�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                           ?�  ?��H@=p�@}p�@�p�@��R@�  A   A\)A   A+�A>�RA_\)A���A�  A��A��A��A�  A�Q�A�  A��B  BQ�B(�B (�B(Q�B0(�B8  B?�
BG�BO�
BX  B`(�BhQ�Bp  Bx  B�  B��B�{B��B��
B�  B�{B�  B�{B�  B��B�  B�(�B�  B�  B��B��B�  B��B�  B�  B�(�B�z�B�{B��B�  B�  B��B��B��B��B��B��C  C  C  C  C

=C  C  C
=C  C��C  C��C��C��C��C   C"  C$  C&  C(  C)��C,  C.  C0
=C2  C4  C6  C7�C9��C<
=C>  C@  CB  CD  CE��CG��CJ  CL
=CN  CO��CQ��CT
=CV{CX  CY��C\  C^
=C`
=Cb
=Cd  Ce��Cg�Ci�HCk��Cn
=Cp  Cr  Ct  Cv  Cx
=Cz  C{��C~
=C�C�C�  C���C���C���C���C���C�  C�  C�  C�  C�  C�  C�  C���C���C�  C���C�  C�
=C�C�C�C�C�C�  C���C�  C�C�C�  C�
=C�C�  C�C�C�C�C�  C���C�  C�  C���C���C���C�  C���C���C�  C���C���C�C�  C�C�C�C�  C���C�C���C���C�  C�
=C�C���C�  C�C���C���C�C�  C�  C���C���C�  C�C�\C�C�  C�C�  C���C�C�  C�  C�C�C�  C���C���C���C�  C�  C�C�
=C�C�  C���C���C�  C�
=C�  C���C���C�  C�C�C�C�
=C�C�  C���C���C���C�  C�C�  C���C�  C���C���C�  C���C���C���C���C�  D �D ��D  D� D�D� D  D}qD�qDz�D��D}qD�D��D  D� D  D� D�qD	��D
�D
��D  Dz�D��Dz�D�qD}qD�qD}qD  D� D  D��D�D}qD  D� D�qD� D�D}qD  D� D  D��D�D�DD� D  D}qD  D� D�qD}qD�qD� D  D� D�qD}qD  D� D   D }qD �qD!� D"�D"� D#  D#� D$  D$}qD%�D%� D%�qD&}qD&��D'z�D(  D(� D(�qD)}qD*�D*� D*�qD+� D,  D,� D-  D-}qD.  D.� D/  D/��D0D0��D1  D1� D2�D2� D3  D3� D4  D4� D5  D5��D6�D6� D7  D7� D8�D8��D9�D9��D:  D:� D;  D;� D<  D<� D=D=��D>�D>��D?  D?� D@�D@� DA  DA}qDB  DB� DB�qDCz�DC�qDD� DD�qDE� DF�DF��DGDG� DG�qDH� DI  DI� DJ  DJ}qDK  DK��DK�qDL� DM�DM��DN�DN� DN��DO}qDP�DP��DQ  DQ}qDR  DR��DR�qDS� DT  DT}qDU  DU}qDV  DV� DW  DW�DX  DXz�DX�qDY� DZDZ�D[�D[��D\  D\}qD]  D]}qD]�qD^}qD_  D_��D_�qD`}qDa  Da� Db  Db}qDc  Dc��DdDd��De�De� Df  Df� Dg  Dg� Dh  Dh}qDh�qDi}qDi�qDj}qDk  Dk}qDk��Dl� Dl�qDmz�Dm�qDn� Dn�qDo}qDp  Dp� Dq�Dq��Dr�Dr��Dr�qDs}qDt  Dt}qDu  Du�Dv�Dv� Dv�qDw� Dx�Dx��Dy�Dy� Dz  Dz� D{  D{� D{�qD|}qD|�qD}� D~  D~}qD  D��D�  D�@ D��HD��HD���D�@ D�� D���D�  D�@ D�� D�� D�HD�@ D�~�D���D�  D�@ D�� D�� D�HD�@ D�� D��HD�HD�@ D�~�D�� D�HD�B�D��HD���D�  D�@ D��HD�D�  D�>�D�� D�� D�HD�>�D�}qD���D�HD�@ D�}qD��qD�  D�AHD��HD��HD��D�@ D��HD��HD�HD�AHD��HD�D�HD�@ D�~�D���D�  D�=qD�~�D��HD�  D�>�D�� D��HD�  D�>�D�� D�� D���D�>�D��HD��HD���D�>�D�~�D�� D�HD�@ D��HD���D���D�@ D�~�D�� D��D�AHD�~�D�� D��D�AHD�}qD��qD�HD�AHD�~�D��qD�  D�AHD��HD�D�HD�>�D�� D��HD���D�@ D���D��HD�  D�>�D�� D��HD�  D�@ D�~�D�� D��D�AHD�� D���D���D�>�D�~�D��HD�HD�>�D�~�D���D���D�>�D�� D�� D�HD�AHD�� D��qD���D�@ D��HD��HD�  D�=qD�� D��HD�  D�>�D�� D�D�HD�@ D��HD��HD�  D�>�D�}qD���D�HD�AHD��HD��HD�  D�@ D�~�D��HD�HD�@ D�� D���D�HD�AHD��HD��HD�HD�AHD��HD��HD�  D�@ D��HD��HD���D�@ D��HD�� D�  D�AHD��HD�D�  D�@ D�� D��HD��D�AHD�� D�� D�  D�AHD���D�� D���D�@ D���D��HD�  D�=qD�� D��HD�  D�AHD���D�� D�HD�B�D�� D���D���D�AHD��HD���D�  D�AHD�� D�� D�  D�>�D�~�D���D�HD�B�D�� D�� D�HD�@ D�~�D���D�  D�@ D�� D��HD�  D�>�D�� D�� D�HD�AHD�� D���D�HD�@ D�� D�� D�  D�@ D�~�D���D���D�>�D D��HD��D�AHDÀ Dþ�D���D�@ DĀ D�� D�HD�@ D�}qD�� D�HD�AHDƀ D�� D�HD�@ Dǀ D�� D���D�@ DȁHD�� D�HD�@ Dɀ D��HD�  D�AHDʁHD�� D�  D�AHDˁHD�� D�  D�@ D̀ D��HD�HD�@ D�~�D�� D�  D�@ D΁HD��HD���D�>�D�~�DϾ�D�  D�AHDЁHD�� D�  D�@ Dр D��HD���D�=qDҀ D��HD�  D�@ DӁHD��HD�  D�>�D�~�D��HD�HD�AHDՂ�D�� D�  D�@ Dր D�� D�  D�>�D�}qD׾�D�  D�AHD؁HD�D�HD�@ D�~�Dپ�D�  D�@ DځHD��HD�  D�@ Dۀ D�� D�HD�@ D܀ D��HD�  D�AHD݀ Dݾ�D�HD�@ D�~�D޾�D���D�>�D߀ D�� D��qD�=qD�~�D�� D�HD�@ D� DᾸD�HD�@ D� D�D�HD�AHD� D�� D�  D�@ D� D�� D�HD�AHD�~�D�� D�HD�>�D�~�D澸D���D�>�D�~�D�� D�HD�AHD� D�� D���D�@ D� D�� D�HD�AHD�~�D�qD���D�AHD�HD��HD�HD�AHD�HD쾸D���D�@ D�HD��HD�HD�@ D� D��HD�  D�>�D�}qDﾸD�  D�AHD���D��HD���D�@ D� D�D�  D�@ D� D�D��qD�>�D� D�qD���D�@ D�~�D��HD��D�AHD��HD���D��qD�>�D�� D���D�  D�AHD�� D���D��qD�>�D�~�D�� D�  D�@ D���D�D��D�/\?\)?8Q�?W
=?�z�?�p�?�(�?��@��@!G�@0��@@  @Q�@fff@s33@�  @��@���@�
=@�p�@��@���@�33@���@�G�@˅@��@ٙ�@�G�@�=q@�z�@��HAG�A
=A
�HA\)A�
AQ�A{A!�A%A+�A0��A5�A9��A>�RAC�
AH��AMp�AQ�AVffA\(�AaG�AeAi��Ap  Au�Az=qA~{A��A�z�A��RA���A�33A�A�Q�A�=qA�z�A�
=A���A��A�p�A��A��\A��A�\)A�G�A�(�A�\)A��A�z�A�
=A�=qA���A�\)A\A�A�  Aʏ\A��A�Q�Aҏ\A���A׮Aڏ\A��A�\)A��A���A�\)A陚A�(�A�
=A�=qA���A�
=A��A��A��B�B�\B(�Bp�B�RB(�B	B\)B��B{B�BG�B�\B  Bp�B
=B��B�B\)B��B=qB�
B!G�B"�\B$  B%��B'
=B(Q�B)��B+
=B,��B.{B/�B0��B2=qB3�B5G�B6�\B7�
B9G�B:�HB<Q�B=��B>�HB@Q�BB{BC�BD��BF{BG�BI�BJ�RBL  BMG�BN�RBPQ�BQBS33BTz�BUBW\)BX��BZ=qB[�B\��B^ffB`  BaG�Bb�\Bc�
Bep�Bg
=Bhz�BiBk
=BlQ�Bn{Bo�Bp��Br{Bs�Bt��BvffBw�Bx��BzffB{�B}p�B~�RB�  B���B�G�B�{B��HB�p�B�{B��RB�p�B�(�B��HB���B�=qB��HB��B�=qB���B��B�Q�B���B���B�Q�B�
=B��B�Q�B���B��B�ffB�
=B��B�=qB���B��B�Q�B���B��B�=qB��HB��B�(�B��RB�G�B��B���B�G�B��B�ffB���B���B�Q�B��HB�p�B��B�z�B�
=B���B�=qB���B�G�B��B�Q�B��HB��B�{B���B��B���B�Q�B���B���B�=qB���B�p�B�  B��RB�\)B�  B��RB�G�B��B�z�B�33B��
B��\B�33B��
B�ffB�
=B���B�=qB��HB���B�=qB��HB�p�B�  B��RB�\)B�{B��RB�p�B�(�B��RB�p�B�  B¸RB�\)B�(�B���Bř�B�Q�B���BǙ�B�=qB��HBə�B�=qB���BˮB�ffB��B�B�z�B��BϮB�Q�B���BѮB�=qB���Bә�B�=qB���B�\)B��
B�Q�BָRB�
=B�p�B׮B�  B�Q�B؏\B���B�
=B�33B�p�Bٙ�B�B��B�  B�(�B�ffBڣ�B���B���B��B�\)Bۙ�B�B��B�{B�Q�B�ffB܏\BܸRB�
=B�33B�\)Bݙ�B�B��B�(�B�Q�B�z�Bޣ�B���B���B�33B�\)B߅B�B�  B�=qB�z�B�RB���B��B�\)BᙚB�B�  B�(�B�ffB�\B���B�
=B�33B�p�B�B��B�(�B�ffB��B��HB��B�G�B�B�B��B�(�B�ffB��B��HB�33B�p�B�B��B�=qB�\B���B�
=B�G�B�B��
B�(�B�ffB��B���B�33B�B��
B�(�B�ffB��B���B�33B홚B��B�=qB�\B��HB�33B�B��B�Q�B��\B���B�G�B�B�  B�ffB�RB�
=B�p�B�B�(�B�\B���B�\)B��B�{B��\B���B�\)B�B�=qB���B��B���B�  B�z�B���B�\)B��
B�Q�B���B�G�B�B�(�B���B��B��C   C =qC p�C ��C �HC�CQ�C��C��C
=C=qCz�C�RC��C33Cp�C��C�
C{CQ�C�\CC  C33Cp�C��C�HC{CQ�C�C�RC��C(�CffC��C�
C�CQ�C�\C��C	  C	=qC	z�C	�RC	��C
33C
p�C
��C
�HC�CffC��C�HC{CQ�C�\C��C
=CG�Cz�CC��C=qCp�C�C�HC�C\)C�\C��C
=C=qCz�C�RC��C(�CffC��C�HC{CQ�C�\CC  CG�Cz�C�RC��C(�CffC��C�
C{CG�C�C�RC  C=qCz�C�RC��C33Cz�C�RC�C(�CffC��C�
C{CQ�C�\C��C
=CG�C�CC  C=qCz�C�RC��C=qCp�C�C�HC�CQ�C�\C��C
=CG�C�\C�
C�C\)C��C�HC �C \)C ��C �
C!(�C!p�C!�RC!��C"33C"p�C"�C"��C#33C#�C#C$
=C$G�C$�\C$C%  C%G�C%�C%��C&{C&\)C&��C&�HC'(�C'ffC'��C'�HC((�C(p�C(C)
=C)Q�C)�\C)��C*{C*Q�C*��C*�C+33C+z�C+�RC+��C,33C,z�C,C-{C-\)C-��C-�HC.�C.ffC.�C.��C/=qC/�\C/�
C0(�C0p�C0�C0��C1=qC1�\C1�HC233C2z�C2C3
=C3Q�C3��C3��C4=qC4�\C4�
C5(�C5ffC5�C6  C6G�C6��C6�C7=qC7z�C7C8
=C8\)C8�C9  C9Q�C9��C9�HC:(�C:z�C:��C;(�C;p�C;�RC<
=C<Q�C<��C<��C=G�C=��C=�HC>(�C>p�C>C?{C?ffC?�RC@
=C@Q�C@��C@�HCA=qCA�\CA�HCB�CBp�CBCC{CCffCC��CC�CDG�CD��CD�CE33CEz�CECF�CFp�CFCG
=CGQ�CG��CG�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                           G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��A���A�A���A���A���A���A��A���A�A�A�VA�1A�%A�  A��A���A��A��A��A��mAϺ^Aϗ�A�z�A�t�A�p�A�n�A�jA�hsA�bNA�`BA�`BA�`BA�^5A�^5A�\)A�ZA�ZA�XA�Q�A�O�A�O�A�O�A�M�A�E�A�oA�M�Aɥ�A�t�A�x�A���A��+A��7A�z�A�n�A���A���A�A�A�S�A�I�A�$�A�XA�"�A�v�A��^A�dZA�{A��A�Q�A��A�1A�p�A��A�(�A�l�A�A�M�A�?}A�n�A�"�A�(�A}+A{/AuXAmO�Ak�;Aj�DAg�#AdbNAa�-A^{AZ�!AV �AU��AS�
AR�9AQ�PAO��AL�9AK;dAJ=qAI7LAES�ACt�A@�jA<�yA;O�A:Q�A8I�A6Q�A6n�A7`BA6��A5K�A45?A2�yA1�#A0��A01'A/�TA/�wA/��A2�A2��A0r�A.ĜA.z�A.1A-�A,��A+��A+|�A+"�A+oA+%A*��A*bA)C�A(��A(9XA'|�A&��A&�uA&I�A%��A%;dA$�A$�A$=qA$1A#A#+A"~�A!��A!��A!�^A!��A!�A!K�A ��A A�A r�A�#AVA�9A��A^5A �A�A��A��A��A�HAz�AffAA�AA��AG�A�yAJA�A|�A�A��A�A��Al�A33A"�A�A�jA �A�;A�PA/A�jA��AffA�mA�hA�yAv�A�A��AffAA��Ax�A?}A�yA�9AM�AA�PA+A��A�uA �AhsA\)A�hA�-A/A�AE�A�A;dA%A
ZA
�A	�;A	x�A	%A�RA9XA  A�TAdZA
=AĜA��A~�A�+AjA1'A�TA�A�A�9A1A�A�TA��AoAv�AM�AJAp�A�A jA A�A b@���@�l�@�-@���@��;@�
=@�=q@�$�@�@��h@��@�z�@���@��R@�@�@�ƨ@�K�@�-@�G�@��@��@��
@�ȴ@�v�@�J@�@�x�@�hs@�O�@���@�@�o@�{@�V@�u@�w@�\@�h@�7L@�j@���@�@�"�@�ȴ@�-@�?}@�Ĝ@ߝ�@�C�@�C�@�o@�M�@�7L@�Ĝ@۶F@���@���@ڇ+@��T@ٲ-@�7L@؛�@ם�@�
=@և+@�V@�5?@ՙ�@��@ԋD@�bN@��@ԋD@�1'@ӶF@�C�@җ�@�hs@�&�@�&�@��@�%@У�@��@�+@�v�@��@�@ͩ�@�?}@̣�@̛�@�j@��
@�dZ@�ȴ@�~�@�E�@ɺ^@�7L@���@ȃ@�I�@��;@�t�@�33@�@�^5@�J@��@ũ�@�?}@��/@�A�@�t�@�;d@�ff@��@�-@��#@�@�p�@�%@�I�@� �@�b@��@�1@��@��@���@�@���@�O�@��@�z�@��m@���@�l�@�K�@���@��@��R@�v�@��@��@���@��u@�j@� �@�1@���@��@�dZ@�K�@�K�@�o@��!@�$�@��T@�G�@���@��`@��`@��@�A�@�ƨ@�l�@�+@�ȴ@�{@��#@��h@�X@�/@��/@�r�@�Q�@�1@���@���@�33@���@�M�@��@��#@��`@�I�@��
@�t�@���@���@�V@���@���@�G�@���@���@�bN@�9X@���@�;d@���@���@���@��\@�n�@�M�@�{@���@�hs@�G�@�&�@���@��9@�A�@��m@���@�ƨ@��@���@�S�@�@��!@�$�@���@�X@��@��j@�Z@�A�@�(�@���@��
@���@�;d@��H@��R@�~�@�J@��^@���@�hs@�G�@�/@��@��D@�(�@�b@���@��@�"�@���@��@�~�@�J@��#@���@�x�@���@��/@���@�j@�9X@��
@�33@�@��H@���@���@���@�ff@��h@�G�@��`@���@���@��@�r�@�  @�t�@�;d@���@���@�^5@�5?@��@��^@�`B@��@�Z@��
@�|�@�;d@�
=@��@���@���@��+@�=q@��@���@�O�@��@�Ĝ@��@�I�@��m@�;d@�o@�@��@��!@�$�@���@��7@�x�@��`@�z�@�Q�@�1'@��@���@��F@���@�dZ@�K�@�;d@�o@�
=@���@��y@��@�ȴ@�M�@�@���@��h@�p�@�G�@��@���@���@�z�@�I�@�  @��;@��@�dZ@�K�@�"�@��y@��R@��\@�v�@�M�@���@���@�@��7@�G�@�/@��/@���@�j@�b@��@�@~��@~ff@~@}@}�@}`B@}�@|�/@|��@|1@{o@z��@z�\@zn�@zJ@y��@y��@yX@xĜ@x�u@x �@w�P@w;d@w
=@vv�@up�@u?}@t��@tZ@t(�@s��@sS�@r�@r��@r^5@r-@q�#@qx�@p�`@p1'@o�w@oK�@n�R@nE�@n@m�-@m`B@mV@l�/@lZ@k�
@kS�@j�!@j=q@jJ@i�@i�#@i��@i�^@i��@i�7@i7L@h�`@h�9@hr�@h1'@h  @g�P@f�@f�+@fE�@f@f@e�h@e�@d�j@c�m@c�m@cƨ@cC�@b�H@bn�@a�@ahs@`Ĝ@` �@_+@^��@^��@^E�@]�T@]��@]`B@]V@\�@\��@\�@\j@[��@[��@[t�@[33@Z��@ZJ@Y��@YX@XĜ@Xr�@XQ�@Xb@W�w@W�P@W\)@W;d@W+@V�@VE�@U/@Tz�@T1@SC�@R�!@R^5@R=q@Q�#@Qhs@Q�@PQ�@O��@O+@O
=@N�@N��@N$�@M@M`B@L�@Lz�@LI�@L(�@L1@K�
@K��@KdZ@K33@J�@J��@J~�@Jn�@Jn�@JJ@IG�@H��@HĜ@Hr�@HQ�@HA�@H  @G�;@G��@GK�@F��@F@E@E�-@E�@EV@D��@DI�@C�
@CdZ@C"�@B�H@B�!@B�\@BM�@A�@Ax�@@�`@@��@@�9@@A�@@  @?�@?\)@>V@>E�@=�h@=/@<�@<j@;ƨ@;��@;S�@:�H@:��@:��@:n�@:�@9�7@9hs@9&�@8��@8r�@8A�@8  @7�P@7l�@6��@6$�@5�@5�-@5O�@4�@4�j@4j@41@3�
@3��@3��@3C�@2��@2-@1�#@1��@1x�@0�`@0r�@0A�@0  @/�;@/|�@/
=@.�R@.V@-��@-?}@,�j@,�D@,(�@+�m@+�@+@*~�@*-@)�@)�^@)�7@)X@)%@(��@(�u@(Q�@(bN@(bN@(Q�@( �@'�@'+@&ȴ@&v�@&5?@&{@%�@%@%`B@$��@$��@$�D@$z�@$Z@$(�@#�
@#ƨ@#"�@"�H@"��@"n�@"M�@"�@!�^@!x�@!&�@ �9@ Q�@ b@��@��@l�@;d@ȴ@��@v�@V@�@�-@��@�@`B@`B@?}@�/@z�@I�@9X@(�@(�@�@�
@��@dZ@33@"�@o@@�@�@��@��@~�@n�@M�@=q@�@��@��@x�@x�@hs@G�@%@�9@�u@�u@�@r�@r�@bN@1'@1'@1'@1'@1'@b@  @  @�;@�@\)@�R@��@�+@E�@��@�h@`B@/@�@��@�/@��@z�@(�@�m@ƨ@�@33@o@�@�!@�\@M�@-@=qA��A��A��A���A���A��A���A���A��A���A�A�A�A�  A�  A�A�  A���A�  A�  A���A���A���A���A���A���A���A���A���A���A���A���A��A��A��A���A�  A��A���A�A�1A���A�A�A�A�%A�  A�1A�JA�bA�bA�bA�
=A�JA�oA�
=A�%A�%A�
=A�1A�A�A�1A�A���A�%A�
=A�VA�%A�A�A���A���A���A���A��A���A��A��A��A��A���A�  A�A���A���A��A��A��A��A��A��A��A��A��A��A��A��yA��mA��mA��A��A��mA��mA��yA���A��A��yA��A��
A�ƨA�A��
Aϲ-AϬAϩ�Aϙ�Aϟ�Aϕ�Aϙ�Aϛ�Aχ+A�|�A�z�A�x�A�t�A�t�A�v�A�x�A�v�A�r�A�r�A�t�A�t�A�p�A�n�A�n�A�p�A�p�A�p�A�l�A�jA�n�A�n�A�l�A�jA�jA�l�A�l�A�jA�ffA�ffA�hsA�hsA�dZA�`BA�bNA�bNA�dZA�bNA�^5A�`BA�bNA�dZA�bNA�^5A�^5A�bNA�bNA�`BA�^5A�^5A�`BA�bNA�bNA�`BA�^5A�^5A�bNA�`BA�^5A�\)A�\)A�`BA�`BA�^5A�\)A�\)A�^5A�`BA�\)A�ZA�ZA�\)A�\)A�^5A�ZA�XA�XA�ZA�\)A�\)A�XA�XA�XA�ZA�\)A�\)A�XA�VA�S�A�S�A�S�A�S�A�O�A�M�A�M�A�Q�A�Q�A�Q�A�M�A�M�A�O�A�Q�A�S�A�O�A�M�A�O�A�Q�A�Q�A�O�A�M�A�M�A�Q�A�Q�A�Q�A�O�A�K�A�M�A�O�A�Q�A�O�A�I�A�E�A�G�A�I�A�G�A�E�A�?}A�9XA�33A�33A�5?A�-A�JA��#AΣ�A�S�A��A�v�A�;dA��ȂhA���A˝�A�z�A�/A��A�  A�Q�A�I�A�VA��
A�r�A�(�A�
=A��/AøRA×�AÁA�jA�E�A�(�A�bA��yA��TA��#A£�A�jA�G�A�(�A��A��FA�1'A��A�~�A�ƨA���A�hsA�M�A��A���A��HA���A�l�A�=qA��A��^A���A�r�A�\)A�E�A�$�A�%A��HA��9A�~�A�Q�A�$�A���A��mA��wA��+A�p�A�9XA�\)A��\A�7LA���A��TA���A��jA���A�~�A�ZA�\)A�^5A�^5A�`BA�^5A�`BA�VA�M�A�;dA�/A�(�A�&�A�$�A�"�A��A�%A��A��A��yA��#A�A���A�x�A�hsA�G�A��A���A��mA���A���A�z�A�S�A�"�A�oA�1A�A���A���A���A��A��/A��-A��\A�p�A�`BA�M�A�;dA�oA�|�A�;dA��A��A��7A�`BA�+A��A���A���A��9A��9A��A���A��A�G�A�+A�(�A� �A���A�A���A�hsA�;dA�bA��;A�jA�`BA�\)A�A�I�A���A�;dA�ffA���A��RA���A��DA�ffA��A�z�A��^A�+A��RA�G�A��RA�`BA��`A�oA�O�A���A���A�r�A�M�A�"�A�VA��yA��/A���A��A��+A�ffA�G�A�oA��yA��A���A���A���A��A�l�A�O�A�$�A��`A��A�hsA� �A��wA�"�A�5?A�O�A��A�?}A��;A��\A�\)A�/A�JA��A��RA���A��A�t�A�bNA�VA�Q�A�O�A�M�A�K�A�G�A�G�A�C�A�A�A�C�A�C�A�7LA�/A�bA��;A���A��!A�ffA�E�A�33A�(�A��A�VA��A���A�1'A��A��A��A��7A�t�A�XA�
=A��A���A�ZA���A���A�M�A��A��9A��A�
=A��;A�p�A��yA��A�(�A���A���A�r�A��A��7A��#A�JA�~�A��yA��\A��+A�\)A�/A�&�A��A�JA�
=A�A���A��yA��#A���A�A��-A���A�x�A�VA��A��mA��uA��RA��A��^A��A��uA��`A�E�A��#A�~�A��A���A��!A���A�S�A��;A���A�ƨA�|�A���A�r�A�Q�A��A��A�A�v�A���A�7LA��#A�v�A��AAS�A~�A~VA~�A}��A}��A}S�A}oA|��A|r�A|-A{�A{��A{O�A{33A{�A{%Az��Az�Az�RAz~�Az=qAyG�Av��At�Aq��Ap�!Ao��An��AnM�An�Am��Al��Aln�Al^5AlVAl{AlAl  Ak�Ak�TAk��Ak��Ak\)Ak"�Aj�`Aj��Aj~�AjffAj9XAi��Ai��Ai|�Ai?}Ah��Ag��Af�/AfjAe��Aep�Ae+Ad�Ad�\Ad-Ac��Ac\)Ab��Ab��AbI�Aa��Aa�PAa�A`�A`jA`$�A_�A^��A]��A]C�A\��A\bNA[ƨA[�-A[7LA[;dA[AZz�AY��AW�AV^5AV  AV1AV�AV{AVJAVJAVJAVbAVJAV  AU�AU�
AUp�AT�!ATVAT �AS�
AR��ARA�ARv�AR�AR�`ARĜAR��AR��AR�DARI�AQ�-AQ|�AQhsAQ?}AQ�AP�AP��APJAOVAOVAOoAOoAOAN�+AL��AK��AK�FAK��AKp�AKhsAKdZAKl�AKdZAK�AJ��AJ�+AJA�AJ-AJ5?AJ9XAJ5?AJ5?AJ5?AJ5?AJ(�AI`BAHjAG?}AFffAFJAE�wAE\)AE33AD��AC�TAC��AC�ACt�ACt�AChsAC\)AC
=AB$�AAhsA@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                           A��A��A���A�A���A���A���A���A��A���A�A�A�VA�1A�%A�  A��A���A��A��A��A��mAϺ^Aϗ�A�z�A�t�A�p�A�n�A�jA�hsA�bNA�`BA�`BA�`BA�^5A�^5A�\)A�ZA�ZA�XA�Q�A�O�A�O�A�O�A�M�A�E�A�oA�M�Aɥ�A�t�A�x�A���A��+A��7A�z�A�n�A���A���A�A�A�S�A�I�A�$�A�XA�"�A�v�A��^A�dZA�{A��A�Q�A��A�1A�p�A��A�(�A�l�A�A�M�A�?}A�n�A�"�A�(�A}+A{/AuXAmO�Ak�;Aj�DAg�#AdbNAa�-A^{AZ�!AV �AU��AS�
AR�9AQ�PAO��AL�9AK;dAJ=qAI7LAES�ACt�A@�jA<�yA;O�A:Q�A8I�A6Q�A6n�A7`BA6��A5K�A45?A2�yA1�#A0��A01'A/�TA/�wA/��A2�A2��A0r�A.ĜA.z�A.1A-�A,��A+��A+|�A+"�A+oA+%A*��A*bA)C�A(��A(9XA'|�A&��A&�uA&I�A%��A%;dA$�A$�A$=qA$1A#A#+A"~�A!��A!��A!�^A!��A!�A!K�A ��A A�A r�A�#AVA�9A��A^5A �A�A��A��A��A�HAz�AffAA�AA��AG�A�yAJA�A|�A�A��A�A��Al�A33A"�A�A�jA �A�;A�PA/A�jA��AffA�mA�hA�yAv�A�A��AffAA��Ax�A?}A�yA�9AM�AA�PA+A��A�uA �AhsA\)A�hA�-A/A�AE�A�A;dA%A
ZA
�A	�;A	x�A	%A�RA9XA  A�TAdZA
=AĜA��A~�A�+AjA1'A�TA�A�A�9A1A�A�TA��AoAv�AM�AJAp�A�A jA A�A b@���@�l�@�-@���@��;@�
=@�=q@�$�@�@��h@��@�z�@���@��R@�@�@�ƨ@�K�@�-@�G�@��@��@��
@�ȴ@�v�@�J@�@�x�@�hs@�O�@���@�@�o@�{@�V@�u@�w@�\@�h@�7L@�j@���@�@�"�@�ȴ@�-@�?}@�Ĝ@ߝ�@�C�@�C�@�o@�M�@�7L@�Ĝ@۶F@���@���@ڇ+@��T@ٲ-@�7L@؛�@ם�@�
=@և+@�V@�5?@ՙ�@��@ԋD@�bN@��@ԋD@�1'@ӶF@�C�@җ�@�hs@�&�@�&�@��@�%@У�@��@�+@�v�@��@�@ͩ�@�?}@̣�@̛�@�j@��
@�dZ@�ȴ@�~�@�E�@ɺ^@�7L@���@ȃ@�I�@��;@�t�@�33@�@�^5@�J@��@ũ�@�?}@��/@�A�@�t�@�;d@�ff@��@�-@��#@�@�p�@�%@�I�@� �@�b@��@�1@��@��@���@�@���@�O�@��@�z�@��m@���@�l�@�K�@���@��@��R@�v�@��@��@���@��u@�j@� �@�1@���@��@�dZ@�K�@�K�@�o@��!@�$�@��T@�G�@���@��`@��`@��@�A�@�ƨ@�l�@�+@�ȴ@�{@��#@��h@�X@�/@��/@�r�@�Q�@�1@���@���@�33@���@�M�@��@��#@��`@�I�@��
@�t�@���@���@�V@���@���@�G�@���@���@�bN@�9X@���@�;d@���@���@���@��\@�n�@�M�@�{@���@�hs@�G�@�&�@���@��9@�A�@��m@���@�ƨ@��@���@�S�@�@��!@�$�@���@�X@��@��j@�Z@�A�@�(�@���@��
@���@�;d@��H@��R@�~�@�J@��^@���@�hs@�G�@�/@��@��D@�(�@�b@���@��@�"�@���@��@�~�@�J@��#@���@�x�@���@��/@���@�j@�9X@��
@�33@�@��H@���@���@���@�ff@��h@�G�@��`@���@���@��@�r�@�  @�t�@�;d@���@���@�^5@�5?@��@��^@�`B@��@�Z@��
@�|�@�;d@�
=@��@���@���@��+@�=q@��@���@�O�@��@�Ĝ@��@�I�@��m@�;d@�o@�@��@��!@�$�@���@��7@�x�@��`@�z�@�Q�@�1'@��@���@��F@���@�dZ@�K�@�;d@�o@�
=@���@��y@��@�ȴ@�M�@�@���@��h@�p�@�G�@��@���@���@�z�@�I�@�  @��;@��@�dZ@�K�@�"�@��y@��R@��\@�v�@�M�@���@���@�@��7@�G�@�/@��/@���@�j@�b@��@�@~��@~ff@~@}@}�@}`B@}�@|�/@|��@|1@{o@z��@z�\@zn�@zJ@y��@y��@yX@xĜ@x�u@x �@w�P@w;d@w
=@vv�@up�@u?}@t��@tZ@t(�@s��@sS�@r�@r��@r^5@r-@q�#@qx�@p�`@p1'@o�w@oK�@n�R@nE�@n@m�-@m`B@mV@l�/@lZ@k�
@kS�@j�!@j=q@jJ@i�@i�#@i��@i�^@i��@i�7@i7L@h�`@h�9@hr�@h1'@h  @g�P@f�@f�+@fE�@f@f@e�h@e�@d�j@c�m@c�m@cƨ@cC�@b�H@bn�@a�@ahs@`Ĝ@` �@_+@^��@^��@^E�@]�T@]��@]`B@]V@\�@\��@\�@\j@[��@[��@[t�@[33@Z��@ZJ@Y��@YX@XĜ@Xr�@XQ�@Xb@W�w@W�P@W\)@W;d@W+@V�@VE�@U/@Tz�@T1@SC�@R�!@R^5@R=q@Q�#@Qhs@Q�@PQ�@O��@O+@O
=@N�@N��@N$�@M@M`B@L�@Lz�@LI�@L(�@L1@K�
@K��@KdZ@K33@J�@J��@J~�@Jn�@Jn�@JJ@IG�@H��@HĜ@Hr�@HQ�@HA�@H  @G�;@G��@GK�@F��@F@E@E�-@E�@EV@D��@DI�@C�
@CdZ@C"�@B�H@B�!@B�\@BM�@A�@Ax�@@�`@@��@@�9@@A�@@  @?�@?\)@>V@>E�@=�h@=/@<�@<j@;ƨ@;��@;S�@:�H@:��@:��@:n�@:�@9�7@9hs@9&�@8��@8r�@8A�@8  @7�P@7l�@6��@6$�@5�@5�-@5O�@4�@4�j@4j@41@3�
@3��@3��@3C�@2��@2-@1�#@1��@1x�@0�`@0r�@0A�@0  @/�;@/|�@/
=@.�R@.V@-��@-?}@,�j@,�D@,(�@+�m@+�@+@*~�@*-@)�@)�^@)�7@)X@)%@(��@(�u@(Q�@(bN@(bN@(Q�@( �@'�@'+@&ȴ@&v�@&5?@&{@%�@%@%`B@$��@$��@$�D@$z�@$Z@$(�@#�
@#ƨ@#"�@"�H@"��@"n�@"M�@"�@!�^@!x�@!&�@ �9@ Q�@ b@��@��@l�@;d@ȴ@��@v�@V@�@�-@��@�@`B@`B@?}@�/@z�@I�@9X@(�@(�@�@�
@��@dZ@33@"�@o@@�@�@��@��@~�@n�@M�@=q@�@��@��@x�@x�@hs@G�@%@�9@�u@�u@�@r�@r�@bN@1'@1'@1'@1'@1'@b@  @  @�;@�@\)@�R@��@�+@E�@��@�h@`B@/@�@��@�/@��@z�@(�@�m@ƨ@�@33@o@�@�!@�\@M�@-@=qA��A��A��A���A���A��A���A���A��A���A�A�A�A�  A�  A�A�  A���A�  A�  A���A���A���A���A���A���A���A���A���A���A���A���A��A��A��A���A�  A��A���A�A�1A���A�A�A�A�%A�  A�1A�JA�bA�bA�bA�
=A�JA�oA�
=A�%A�%A�
=A�1A�A�A�1A�A���A�%A�
=A�VA�%A�A�A���A���A���A���A��A���A��A��A��A��A���A�  A�A���A���A��A��A��A��A��A��A��A��A��A��A��A��yA��mA��mA��A��A��mA��mA��yA���A��A��yA��A��
A�ƨA�A��
Aϲ-AϬAϩ�Aϙ�Aϟ�Aϕ�Aϙ�Aϛ�Aχ+A�|�A�z�A�x�A�t�A�t�A�v�A�x�A�v�A�r�A�r�A�t�A�t�A�p�A�n�A�n�A�p�A�p�A�p�A�l�A�jA�n�A�n�A�l�A�jA�jA�l�A�l�A�jA�ffA�ffA�hsA�hsA�dZA�`BA�bNA�bNA�dZA�bNA�^5A�`BA�bNA�dZA�bNA�^5A�^5A�bNA�bNA�`BA�^5A�^5A�`BA�bNA�bNA�`BA�^5A�^5A�bNA�`BA�^5A�\)A�\)A�`BA�`BA�^5A�\)A�\)A�^5A�`BA�\)A�ZA�ZA�\)A�\)A�^5A�ZA�XA�XA�ZA�\)A�\)A�XA�XA�XA�ZA�\)A�\)A�XA�VA�S�A�S�A�S�A�S�A�O�A�M�A�M�A�Q�A�Q�A�Q�A�M�A�M�A�O�A�Q�A�S�A�O�A�M�A�O�A�Q�A�Q�A�O�A�M�A�M�A�Q�A�Q�A�Q�A�O�A�K�A�M�A�O�A�Q�A�O�A�I�A�E�A�G�A�I�A�G�A�E�A�?}A�9XA�33A�33A�5?A�-A�JA��#AΣ�A�S�A��A�v�A�;dA��ȂhA���A˝�A�z�A�/A��A�  A�Q�A�I�A�VA��
A�r�A�(�A�
=A��/AøRA×�AÁA�jA�E�A�(�A�bA��yA��TA��#A£�A�jA�G�A�(�A��A��FA�1'A��A�~�A�ƨA���A�hsA�M�A��A���A��HA���A�l�A�=qA��A��^A���A�r�A�\)A�E�A�$�A�%A��HA��9A�~�A�Q�A�$�A���A��mA��wA��+A�p�A�9XA�\)A��\A�7LA���A��TA���A��jA���A�~�A�ZA�\)A�^5A�^5A�`BA�^5A�`BA�VA�M�A�;dA�/A�(�A�&�A�$�A�"�A��A�%A��A��A��yA��#A�A���A�x�A�hsA�G�A��A���A��mA���A���A�z�A�S�A�"�A�oA�1A�A���A���A���A��A��/A��-A��\A�p�A�`BA�M�A�;dA�oA�|�A�;dA��A��A��7A�`BA�+A��A���A���A��9A��9A��A���A��A�G�A�+A�(�A� �A���A�A���A�hsA�;dA�bA��;A�jA�`BA�\)A�A�I�A���A�;dA�ffA���A��RA���A��DA�ffA��A�z�A��^A�+A��RA�G�A��RA�`BA��`A�oA�O�A���A���A�r�A�M�A�"�A�VA��yA��/A���A��A��+A�ffA�G�A�oA��yA��A���A���A���A��A�l�A�O�A�$�A��`A��A�hsA� �A��wA�"�A�5?A�O�A��A�?}A��;A��\A�\)A�/A�JA��A��RA���A��A�t�A�bNA�VA�Q�A�O�A�M�A�K�A�G�A�G�A�C�A�A�A�C�A�C�A�7LA�/A�bA��;A���A��!A�ffA�E�A�33A�(�A��A�VA��A���A�1'A��A��A��A��7A�t�A�XA�
=A��A���A�ZA���A���A�M�A��A��9A��A�
=A��;A�p�A��yA��A�(�A���A���A�r�A��A��7A��#A�JA�~�A��yA��\A��+A�\)A�/A�&�A��A�JA�
=A�A���A��yA��#A���A�A��-A���A�x�A�VA��A��mA��uA��RA��A��^A��A��uA��`A�E�A��#A�~�A��A���A��!A���A�S�A��;A���A�ƨA�|�A���A�r�A�Q�A��A��A�A�v�A���A�7LA��#A�v�A��AAS�A~�A~VA~�A}��A}��A}S�A}oA|��A|r�A|-A{�A{��A{O�A{33A{�A{%Az��Az�Az�RAz~�Az=qAyG�Av��At�Aq��Ap�!Ao��An��AnM�An�Am��Al��Aln�Al^5AlVAl{AlAl  Ak�Ak�TAk��Ak��Ak\)Ak"�Aj�`Aj��Aj~�AjffAj9XAi��Ai��Ai|�Ai?}Ah��Ag��Af�/AfjAe��Aep�Ae+Ad�Ad�\Ad-Ac��Ac\)Ab��Ab��AbI�Aa��Aa�PAa�A`�A`jA`$�A_�A^��A]��A]C�A\��A\bNA[ƨA[�-A[7LA[;dA[AZz�AY��AW�AV^5AV  AV1AV�AV{AVJAVJAVJAVbAVJAV  AU�AU�
AUp�AT�!ATVAT �AS�
AR��ARA�ARv�AR�AR�`ARĜAR��AR��AR�DARI�AQ�-AQ|�AQhsAQ?}AQ�AP�AP��APJAOVAOVAOoAOoAOAN�+AL��AK��AK�FAK��AKp�AKhsAKdZAKl�AKdZAK�AJ��AJ�+AJA�AJ-AJ5?AJ9XAJ5?AJ5?AJ5?AJ5?AJ(�AI`BAHjAG?}AFffAFJAE�wAE\)AE33AD��AC�TAC��AC�ACt�ACt�AChsAC\)AC
=AB$�AAhsA@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                           G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
!-B
 �B
 �B
 �B
 �B
 �B
!-B
!�B
 �B
 �B
!�B
 �B
 �B
 �B
 �B
 �B
!-B
 �B
!bB
!-B
 �B
 �B
"hB
"hB
"�B
"�B
#:B
#B
#:B
#B
#B
#B
#:B
#:B
#nB
#:B
#:B
#�B
#�B
#�B
#�B
#nB
#:B
"�B
!bB
�B
OB
5�B
o�B
�<B
�tB
�6B
�
B?}BEmBB[B=B?�B)_B�B!B�BB
ƨB
`vB
AUB
E�B
T�B
D�B
;�B
5�B
?}B
4B
2-B
�B

�B
B
�B
�B	�B	�BB	�B	��B	��B	�bB	sB	i�B	f2B	`�B	RTB	MjB	TaB	~]B	�B	�VB	�B
�B
�B
�B
B	�xB	��B	�QB	�BB	��B	��B	��B	��B	��B	� B	��B	�.B	�OB	�nB	�B	��B	��B	�B	��B	��B	B	��B	֡B
JB
'B
"B

=B
B
hB
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
$B
	B
B
B
�B
%�B
(�B
'�B
)_B
,B
*�B
,�B
1'B
33B
3�B
5�B
;0B
D�B
D�B
F�B
H�B
HKB
C�B
H�B
L�B
F?B
D�B
GB
HB
HKB
GzB
C�B
CaB
C�B
GEB
H�B
J�B
K�B
M6B
MB
L�B
M�B
N<B
L�B
MjB
M�B
NB
M�B
P�B
QNB
Q�B
S[B
TaB
U�B
TaB
R�B
R�B
VmB
W�B
XB
^jB
\]B
[�B
XyB
V9B
Y�B
\)B
ZQB
YB
VmB
TaB
T�B
R�B
S�B
T�B
T,B
S�B
QNB
R�B
S&B
QB
K�B
J�B
PHB
U�B
U�B
QNB
M�B
LdB
GzB
GzB
F?B
D�B
E9B
FtB
C�B
C�B
?�B
?}B
>�B
>�B
=B
<�B
=�B
?HB
C-B
B�B
C�B
B[B
C�B
A B
B[B
?�B
=qB
<jB
=<B
<�B
9�B
7B
7LB
7B
6FB
2-B
0!B
0�B
/�B
/�B
.}B
)�B
)�B
+�B
(�B
($B
(�B
)*B
(�B
(�B
)�B
)�B
'�B
%B
!�B
!�B
!�B
"�B
"�B
!-B
!�B
�B
B
�B
B
B
B
�B
�B
�B
qB
qB
�B
1B
1B
�B
$B
SB
�B
SB
B
FB
�B
B
�B
B
B
oB
B
oB
�B
�B
.B
�B
�B
PB
PB
�B
xB
DB

=B
	lB
�B
	7B
	�B
	�B
	B
�B
	7B
	�B
�B
:B
 B
.B
\B
�B
\B
�B
�B
(B
4B
�B
�B
VB
�B
�B
PB
�B
�B
VB
"B
\B
�B
�B
�B
bB
�B
 B
 B
�B
hB
�B
�B
�B
�B
:B
�B
�B
�B
�B
�B
�B
YB
SB
�B
FB
�B
�B
�B
B
B
�B
{B
B
�B
_B
CB
~B
CB
qB
�B
�B
CB
qB
=B
�B
�B
�B
kB
qB
�B
=B
�B
xB
xB
B
!B
�B
�B
VB
 �B
!�B
!�B
!�B
!�B
!bB
!�B
!�B
!-B
 �B
 �B
 \B
 �B
!�B
!bB
!-B
 �B
 �B
!bB
!�B
!�B
!�B
!bB
!�B
!bB
!bB
!�B
"�B
"4B
"�B
"hB
#B
#B
"�B
"�B
#:B
"4B
"�B
"�B
#:B
#B
#B
#:B
#nB
$B
#�B
$tB
$�B
$tB
%B
%zB
%zB
%�B
%zB
%�B
%zB
%zB
%�B
&B
&B
&�B
&�B
&�B
'�B
'�B
($B
'�B
'�B
'�B
'�B
'�B
'�B
(XB
(�B
)_B
(�B
)*B
)�B
*0B
)�B
)�B
)�B
)�B
*0B
*�B
+B
+B
+B
,B
+�B
+�B
,B
,B
,=B
,=B
-CB
,�B
-CB
-B
-�B
-�B
-�B
-CB
.}B
.}B
.B
-�B
/B
.�B
.�B
/�B
/OB
/OB
/�B
1'B
0�B
0�B
0�B
0�B
0UB
0�B
0�B
0�B
1[B
0UB
0UB
0UB
0�B
0�B
1�B
1[B
1�B
2-B
2-B
2aB
2�B
2aB
3hB
33B
3�B
49B
49B
4�B
4�B
5?B
5?B
5?B
5tB
5?B
5�B
6zB
6�B
7�B
7B
6�B
7�B
8RB
9�B
9�B
9�B
9XB
:�B
;0B
;�B
:�B
:�B
<jB
<B
<6B
<�B
<�B
=B
=B
=B
=�B
=<B
=qB
>B
=�B
>B
>B
>B
=�B
?�B
?}B
?}B
?}B
?�B
@B
@B
@OB
@�B
@�B
@�B
A�B
A�B
A�B
B[B
B'B
B�B
B�B
C-B
CaB
B�B
C�B
D3B
C�B
C�B
DgB
D�B
D�B
E9B
E9B
EmB
FB
FB
GB
GB
GB
GEB
GEB
GEB
GEB
GzB
G�B
G�B
H�B
IRB
IRB
IB
IRB
IRB
IRB
I�B
I�B
JXB
I�B
J�B
J�B
J�B
J�B
K�B
L0B
K�B
LdB
L�B
L�B
L�B
M�B
MB
MjB
MjB
M�B
M�B
NB
N<B
N�B
OB
OBB
PB
OvB
PB
O�B
PHB
PHB
PHB
P�B
P�B
QNB
Q�B
Q�B
Q�B
Q�B
Q�B
R B
Q�B
Q�B
RTB
RTB
R�B
R�B
R�B
S&B
R�B
S[B
S�B
S�B
T,B
S�B
S�B
T�B
TaB
U2B
UgB
T�B
T�B
U�B
U�B
VB
VB
VmB
V�B
V�B
V�B
VmB
V�B
V�B
W?B
WsB
WsB
W�B
W�B
W�B
W�B
W�B
W�B
XB
W�B
W�B
XyB
X�B
X�B
YB
Y�B
YB
Y�B
ZB
ZB
ZQB
ZQB
ZB
Y�B
Z�B
Z�B
\]B
\)B
\�B
]dB
]�B
]�B
^B
^B
^5B
^B
^�B
_;B
_pB
_pB
_pB
_pB
_�B
_�B
_�B
`BB
`�B
`�B
`�B
`�B
`�B
aB
aHB
a|B
a�B
a�B
a�B
a�B
a�B
a�B
bNB
bB
bNB
b�B
b�B
bNB
b�B
c B
c�B
c�B
d�B
d�B
d�B
d�B
d�B
e`B
e�B
e�B
ffB
f�B
f�B
f�B
gB
gB
g8B
gmB
gmB
gmB
g8B
g8B
gmB
gmB
h>B
h
B
gmB
g�B
h
B
h>B
h>B
h>B
h�B
iyB
jB
jB
jB
i�B
i�B
j�B
j�B
jB
kB
k�B
l�B
lWB
lWB
lWB
m)B
m�B
ncB
n/B
n�B
n�B
n�B
n�B
n�B
n�B
n/B
ncB
o5B
o�B
pB
p�B
p�B
p�B
qB
q�B
q�B
q�B
rB
rGB
sB
sMB
sB
s�B
r�B
s�B
s�B
tB
tB
s�B
s�B
sMB
s�B
sMB
s�B
s�B
s�B
tTB
t�B
u%B
u�B
u�B
u�B
v`B
v+B
v�B
v�B
wfB
v�B
wfB
w�B
w�B
w�B
w�B
xlB
x�B
x�B
y	B
y	B
y>B
yrB
yrB
y>B
zDB
zxB
z�B
{B
{B
{JB
|B
|PB
|�B
}VB
}�B
}�B
}�B
}�B
~(B
}�B
~�B
~�B
~�B
.B
�B
�B
�B
� B
�iB
��B
��B
�B
��B
��B
�iB
��B
�iB
�iB
��B
��B
�;B
��B
��B
�AB
�uB
��B
��B
�{B
�{B
��B
��B
��B
��B
��B
��B
�B
�MB
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�%B
�%B
�%B
�%B
��B
�%B
�%B
��B
�%B
�%B
��B
��B
�_B
��B
��B
��B
�1B
�fB
��B
��B
��B
��B
��B
�7B
��B
��B
�	B
�=B
��B
�rB
��B
��B
�B
�DB
�DB
�=B
!-B
"hB
 �B
 �B
 �B
"�B
�B
 \B
!bB
!-B
 �B
�B
 'B
!�B
 �B
�B
 \B
 \B
 �B
 'B
!�B
!bB
!bB
�B
 \B
"4B
!�B
 'B
!-B
"4B
!�B
"�B
 �B
 �B
!�B
 �B
!B
 �B
 �B
!�B
�B
"�B
�B
!�B
 \B
�B
!-B
�B
 �B
�B
�B
 �B
#�B
!bB
�B
!�B
!�B
!-B
VB
 \B
!bB
!bB
�B
#B
 �B
!bB
 �B
�B
#B
 �B
"�B
"�B
�B
"hB
 �B
"hB
!bB
�B
!�B
!�B
~B
 'B
 \B
 �B
!�B
VB
$tB
"hB
 \B
 �B
!�B
!�B
 �B
!B
 �B
!�B
!-B
 \B
 �B
!�B
!-B
�B
!�B
 �B
 �B
!B
!B
"�B
_B
 �B
%FB
!-B
�B
*eB
 �B
"hB
"�B
�B
%�B
#nB
�B
+�B
'RB
"4B
!�B
#:B
#�B
"�B
"4B
"�B
#nB
#�B
"�B
!�B
#B
$B
#�B
"�B
!�B
!�B
#�B
#�B
"�B
!�B
"�B
#�B
#�B
"�B
"4B
"�B
#�B
$B
"�B
!�B
#:B
$B
#nB
"�B
!�B
"�B
#�B
#�B
"�B
!�B
"�B
#�B
#�B
"�B
"4B
#B
#�B
$@B
#B
"4B
"�B
#�B
#�B
$B
"�B
"hB
#B
$B
#�B
"�B
"hB
#:B
$@B
$B
"�B
"hB
#:B
$B
$B
#nB
"�B
"4B
#:B
$�B
$B
#B
"�B
#B
$B
$�B
$@B
#nB
"hB
#:B
#nB
$@B
$�B
$tB
#nB
#:B
$B
$�B
$tB
#nB
"�B
#B
$B
$@B
#nB
"4B
"4B
#�B
$B
#B
"hB
"4B
#B
#�B
#nB
"4B
!bB
!bB
!�B
"�B
!�B
!bB
 'B
 \B
!�B
!bB
 �B
VB
OB
�B
�B
�B
�B
�B
=B
�B
B
�B
%B
'�B
+6B
=B
/B
<jB
5�B
NpB
C-B
7B
J#B
o B
gmB
�wB
�B
�}B
��B
��B
��B
�OB
ÖB
ĜB
�mB
�3B
�B
��B
ʌB
͟B
�0B
�B
�#B
��B
�B
�B
�&B
�sB
�WB
��B�BI�B<6B8B?�B;�BA�B@�BB'BD�BG�BA BEmBIRBD3BC-BE9B=<BCaB:^B<jB=�B9�B=qB>�B=�B<BE9B=<B?�BC�BW�B4nB+B1B(B�BuBMB�B�BkB�B�B	BB�BB�B�B�B=B�B�B�B�B�B�B�B�BB!�B"hB!�B�B#nB"hB"�B 'B�B!-B�B �B�BIBB�BxB�B	B�B�B�B�B�B_B�BB7B2�B&�B,B(�B(�B(�B.B/OB%zB'B,�B$@B$�B"�B%FB#�B�B�B�B�B�B�B�B1B
�VB
��B�B�B
�B
�yB
�XB
�B
� B
��B
�B
�JB
�MB
��B
��B
�+B
��B
�AB
}�B
iyB
`�B
[WB
Q�B
XB
R�B
K�B
K)B
?�B
A�B
AUB
>BB
=qB
A B
=B
<6B
EmB
D�B
A�B
EmB
I�B
FtB
B[B
DgB
EmB
B[B
CaB
EmB
FtB
GEB
I�B
E�B
G�B
K�B
M6B
\�B
iDB
`B
X�B
Z�B
J#B
K^B
CaB
A�B
E9B
GB
F�B
EmB
E�B
A�B
A�B
>�B
=�B
<B
;dB
;0B
:*B
8�B
9XB
8�B
6�B
6FB
6zB
3�B
7B
4B
0�B
3�B
9$B
;dB
;�B
:�B
:�B
=�B
@OB
T�B
B�B
?HB
<6B
<B
5�B
/B
1�B
,qB
+kB
)�B
AUB
1�B
-�B
6�B
9�B
+6B
/B
&�B
$�B
FB
;0B
VB
#B
�B
�B
�B
�B
$B
�B
oB
+B
�B	�8B	��B
�B
B	�cB
 iB
�B	��B
;B
GB
�B
�B
{B
uB
�B
�B

rB

�B
�B
JB
=B
"�B
(B
�B	��B
B
"hB
B

=B
B
SB	��B	� B	�B	��B	��B	�B	��B	�B	��B	� B	�#B	چB	�gB	֡B	�?B	�B	��B	�dB	��B	��B	��B	�UB	�B	��B	�LB	�zB	��B	�9B	�-B	��B	��B	��B	�!B	��B	��B	��B	��B	�:B	�4B	�'B	�!B	��B	��B	��B	�'B	�=B	�*B	� B	�.B	� B	v`B	o�B	w2B	|�B	m]B	h�B	j�B	o�B	jB	h�B	h�B	gmB	gmB	h�B	h�B	i�B	h�B	h�B	e�B	d&B	c�B	b�B	a|B	`vB	^jB	f2B	jB	a|B	W
B	Y�B	Z�B	R�B	S�B	PB	T,B	R�B	OvB	M6B	J�B	G�B	GB	N�B	QB	S�B	P�B	\�B	[�B	a�B	W�B	GzB	L0B	Q�B	IB	R�B	gmB	w�B	��B	��B	�B	�UB	��B	r|B	s�B	zB	�4B	�_B	�{B	��B	�B	�~B	��B	�hB	��B	��B	�0B	�B	�B	�AB
fB	�B	�B	��B

�B
.B
 B
\B
FB
�B
!-B
1B
�B
!bB
B
$tB
IB
(�B
1B
(B
�B
bB
�B
#nB
5�B
�B
+B
 iB

�B	�xB	��B	�B	�B	�lB
 iB	�JB	�iB	��B	�DB	�QB	�KB	�B	�DB	�mB	��B
 iB	��B	��B	�/B	ΥB	�HB	��B	�#B	�B	ʌB	�-B	��B	��B	�6B	��B	��B	��B	��B	��B	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                           B
�B
�B
BB
wB
�B
�B
�B
HB
�B
wB
HB
wB
BB
�B
BB
�B
�B
wB
B
�B
�B
wB
B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 B
�B
�B
UB
UB
UB
�B
 B
�B
NB
B
<B
B
/�B
iPB
��B
�&B
��B
�B9/B?B<B6�B9cB#B�B�B<B�B
�ZB
Z(B
;B
?�B
NGB
>NB
5KB
/ZB
9/B
-�B
+�B
jB
XB	��B
�B
�B	�:B	��B	��B	�NB	�3B	�B	l�B	c�B	_�B	Z\B	LB	GB	NB	xB	z�B	�B	�cB
 tB
�B
LB
�B	�*B	�xB	�B	��B	��B	��B	�zB	�qB	�gB	��B	��B	��B	�B	� B	��B	��B	�9B	��B	�BB	�2B	�AB	ǅB	�SB
�B
 �B
�B
�B
�B
B
	wB
<B
�B
�B
�B
�B
[B
�B
UB
9B
�B
�B
�B
�B
�B
�B
"?B
!�B
#B
%�B
$�B
&WB
*�B
,�B
-NB
/ZB
4�B
>�B
>�B
@ZB
B2B
A�B
=�B
B�B
FB
?�B
>NB
@�B
A�B
A�B
A,B
=HB
=B
=HB
@�B
B�B
DsB
EDB
F�B
F�B
FB
G�B
G�B
FB
GB
G�B
G�B
GQB
JcB
K B
K�B
MB
NB
OMB
NB
L�B
LoB
PB
QZB
Q�B
XB
VB
UrB
R+B
O�B
S�B
U�B
TB
S1B
PB
NB
N�B
LoB
MuB
N|B
M�B
MuB
K B
LoB
L�B
J�B
EyB
D�B
I�B
OMB
OMB
K B
G�B
FB
A,B
A,B
?�B
>NB
>�B
@&B
=HB
=�B
9cB
9/B
8�B
8]B
6�B
6QB
7�B
8�B
<�B
<�B
=�B
<B
=|B
:�B
<B
9cB
7#B
6B
6�B
6�B
3>B
0�B
0�B
0�B
/�B
+�B
)�B
*<B
)5B
)jB
(/B
#�B
#yB
%QB
"sB
!�B
"?B
"�B
"?B
"�B
#yB
#yB
!�B
�B
�B
HB
�B
NB
NB
�B
HB
6B
�B
dB
�B
�B
�B
�B
jB
^B
#B
#B
�B
�B
�B
�B
�B
B
9B
B
�B
�B
[B
�B
UB
�B
�B
!B
�B
!B
�B
OB
	�B
	�B
kB
B
B
�B
*B
�B
�B
B
LB
�B
RB
RB
�B
LB
�B
RB
�B
�B

�B
	�B
	B
	CB
	B
�B
qB
�B

�B
OB

}B
B
�B
kB
B
eB
6B
B
�B
	B

}B

}B

IB

B
	�B

�B

�B
�B
B

}B

}B

}B

}B
�B
gB
�B
�B
3B
gB
gB
B
B
gB
�B
�B
aB
aB
�B
�B
[B
-B
�B
�B
B
�B
0B
�B
#B
dB
^B
�B
#B
�B
RB
LB
LB
B
#B
RB
�B
XB
*B
*B
�B
�B
�B
jB
B
�B
}B
}B
HB
HB
B
}B
}B
�B
�B
BB
B
�B
HB
B
�B
wB
wB
B
}B
HB
HB
B
HB
B
B
}B
NB
�B
NB
B
�B
�B
NB
�B
�B
�B
NB
NB
�B
�B
�B
�B
 B
�B
�B
&B
[B
&B
�B
,B
,B
�B
,B
aB
,B
,B
�B
�B
�B
 3B
 3B
 �B
!mB
!9B
!�B
!mB
!mB
!9B
!mB
!�B
!mB
"
B
"�B
#B
"�B
"�B
#yB
#�B
#yB
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
'^B
'�B
'^B
&�B
(/B
(/B
'�B
'�B
(�B
(�B
(�B
)5B
)B
)B
)�B
*�B
*�B
*�B
*pB
*<B
*B
*pB
*<B
*<B
+B
*B
*B
*B
*<B
*<B
+vB
+B
+vB
+�B
+�B
,B
,HB
,B
-B
,�B
-�B
-�B
-�B
.�B
.�B
.�B
.�B
.�B
/&B
.�B
/�B
0,B
0`B
12B
0�B
0�B
1gB
2B
3>B
3>B
3>B
3
B
4EB
4�B
5KB
4�B
4yB
6B
5�B
5�B
6QB
6�B
6�B
6�B
6�B
7WB
6�B
7#B
7�B
7�B
7�B
7�B
7�B
7�B
9cB
9/B
9/B
9/B
9cB
9�B
9�B
:B
:�B
:�B
:�B
;;B
;;B
;�B
<B
;�B
<AB
<�B
<�B
=B
<�B
=HB
=�B
=|B
=�B
>B
>NB
>NB
>�B
>�B
?B
?�B
?�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A,B
A`B
A`B
B2B
CB
CB
B�B
CB
CB
CB
C8B
ClB
D
B
C�B
D>B
DsB
D�B
D>B
EyB
E�B
EyB
FB
FJB
FJB
FJB
G�B
F�B
GB
GB
GQB
GQB
G�B
G�B
H�B
H�B
H�B
I�B
I(B
I�B
I�B
I�B
I�B
I�B
J�B
JcB
K B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
LB
LB
LoB
L;B
LoB
L�B
LoB
MB
M�B
MuB
M�B
M�B
MAB
NGB
NB
N�B
OB
N�B
N�B
O�B
OMB
O�B
O�B
PB
PSB
P�B
PSB
PB
PSB
P�B
P�B
Q%B
Q%B
Q�B
QZB
QZB
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R+B
R`B
R�B
R�B
SfB
S1B
SfB
S�B
S�B
TB
TB
S�B
S�B
T8B
T�B
VB
U�B
VDB
WB
W~B
W~B
W�B
W�B
W�B
W�B
X�B
X�B
Y"B
Y"B
Y"B
Y"B
YVB
YVB
Y�B
Y�B
Z�B
Z�B
Z\B
Z�B
Z�B
Z�B
Z�B
[.B
[cB
[�B
[�B
[cB
[cB
[�B
\ B
[�B
\ B
\4B
\4B
\ B
\iB
\�B
]:B
]oB
^AB
^uB
^uB
^uB
^�B
_B
_{B
_�B
`B
`MB
`�B
`�B
`�B
`�B
`�B
aB
aB
aB
`�B
`�B
aB
aB
a�B
a�B
aB
a�B
a�B
a�B
a�B
a�B
b�B
c+B
c�B
d1B
c�B
c�B
c�B
deB
deB
d1B
d�B
e7B
f=B
f	B
f	B
f	B
f�B
gxB
hB
g�B
hJB
h~B
h~B
h~B
h~B
hJB
g�B
hB
h�B
i�B
i�B
j�B
jVB
jVB
j�B
k�B
k�B
k\B
k�B
k�B
l�B
l�B
l�B
m4B
l�B
m�B
mhB
m�B
m�B
mhB
m4B
l�B
m4B
l�B
m4B
mhB
m�B
nB
nnB
n�B
o@B
o�B
o�B
pB
o�B
p{B
p�B
qB
p�B
qB
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
s$B
s$B
r�B
s�B
t*B
t�B
t�B
t�B
t�B
u�B
vB
vkB
wB
wqB
wqB
w�B
w�B
w�B
w�B
xwB
xwB
x�B
x�B
yIB
yIB
y~B
y�B
zB
zOB
zOB
z�B
z�B
zOB
zB
zOB
zB
zB
z�B
z�B
z�B
{�B
{�B
{�B
|'B
|�B
|�B
}-B
}-B
}bB
}bB
}bB
}bB
}bB
}�B
}�B
}�B
}�B
}�B
~3B
~�B
:B
nB
nB
nB
nB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�tB
�FB
�B
�FB
�zB
��B
��B
�B
�LB
�LB
��B
��B
��B
��B
�RB
��B
��B
��B
�XB
�$B
��B
��B
��B
��B
��B
��B
�B
B
wB
BB
wB
NB
pB
B
B
�B
BB
�B
�B
}B
BB
pB
B
B
BB
�B
}B
B
B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
}B
wB
�B
�B
wB
HB
�B
NB
�B
�B
B
�B
�B
jB
�B
�B
6B
wB
�B
B
�B
}B
�B
�B
B
B
B
B
<B
�B
wB
B
wB
�B
�B
wB
NB
NB
<B
B
�B
B
B
�B
�B
HB
0B
�B
B
wB
�B
B
&B
B
B
wB
�B
HB
�B
�B
�B
�B
�B
B
�B
}B
�B
pB
HB
�B
�B
�B
�B
NB
B
wB
�B
�B
XB
$B
wB
B
NB
�B
aB
 B
<B
%QB
!B
�B
�B
�B
UB
NB
�B
NB
 B
�B
NB
�B
�B
�B
�B
NB
�B
�B
UB
�B
NB
�B
�B
UB
UB
NB
�B
�B
�B
�B
NB
�B
�B
�B
 B
�B
�B
NB
�B
�B
�B
�B
NB
�B
�B
NB
�B
�B
�B
�B
�B
�B
NB
�B
�B
�B
NB
B
�B
�B
�B
�B
B
�B
�B
�B
�B
B
�B
�B
�B
 B
NB
�B
�B
[B
�B
�B
NB
�B
�B
[B
�B
 B
B
�B
 B
�B
�B
&B
 B
�B
�B
�B
&B
 B
�B
�B
�B
�B
 B
�B
�B
UB
�B
�B
B
�B
�B
UB
 B
�B
B
B
�B
�B
}B
B
�B
B
�B
B
BB
B
B
jB
�B
�B
jB
^B
�B
XB
�B
pB
�B
!mB
$�B
6�B
(�B
6B
/�B
H"B
<�B
0�B
C�B
h�B
aB
�)B
��B
�/B
�>B
��B
�pB
�B
�HB
�NB
�B
��B
��B
�sB
�>B
�QB
��B
��B
��B
̤B
��B
��B
��B
�%B
�	B
�~BOBC8B5�B1�B9cB5B;pB:jB;�B>�BA�B:�B?BCB=�B<�B>�B6�B=B4B6B7�B3>B7#B8�B7WB5�B>�B6�B9�B=HBQ�B. B$�B�B�BOB'B�BLBLBB�BXB�B�BRB�BXB�B�B�B�BLBzB�BLB�B�B�B�BHBBHBpB BBNB�B<B�BjBBB�B�B�B^B*B�B�B�B�B<BpBzBB9B�B�B,�B �B%�B"�B"�B"�B'�B)B,B �B&�B�B[BNB�B�B9BaB3B9B[B�B
��B�B
�B
�=B�B�B
�1B
�+B
�
B
��B
��B
�NB
��B
��B
}�B
nB
�B
��B
nB
{�B
w�B
c+B
Z\B
U	B
KiB
Q�B
LoB
E�B
D�B
9�B
;�B
;B
7�B
7#B
:�B
6�B
5�B
?B
>NB
;;B
?B
C8B
@&B
<B
>B
?B
<B
=B
?B
@&B
@�B
C8B
?TB
A`B
EDB
F�B
VDB
b�B
Y�B
R�B
T�B
C�B
EB
=B
;�B
>�B
@�B
@�B
?B
?TB
;pB
;�B
8�B
7�B
5�B
5B
4�B
3�B
28B
3
B
28B
0`B
/�B
0,B
-�B
0�B
-�B
*pB
-NB
2�B
5B
5KB
4EB
4�B
7WB
:B
N|B
<vB
8�B
5�B
5�B
/ZB
(�B
+�B
&#B
%B
#yB
;B
+vB
'^B
0`B
3>B
$�B
(�B
 �B
[B
�B
4�B
B
�B
�B
nB
�B
?B
�B
�B
!B
 �B
6B	��B	�_B
�B	��B	�B	�B	�UB	�IB	��B	��B	��B	��B	�-B	�'B
�B	�nB
$B
XB
qB
�B
�B
�B
�B	��B	��B	��B
B
�B
�B	��B	�B	�SB	�B	�bB	�B	�B	�AB	ܝB	�hB	�B	��B	��B	�8B	�B	�SB	��B	�MB	�uB	�B	��B	��B	��B	�B	��B	�2B	��B	�,B	�TB	��B	��B	��B	��B	�^B	��B	�dB	�QB	��B	��B	��B	��B	��B	��B	�dB	�RB	��B	��B	��B	��B	y�B	��B	y�B	pB	iPB	p�B	vkB	gB	bYB	d�B	iPB	c�B	bYB	bYB	aB	aB	b�B	b�B	c_B	bYB	b�B	_{B	]�B	]oB	\�B	[.B	Z(B	XB	_�B	d1B	[.B	P�B	S�B	T8B	L;B	M�B	I�B	M�B	LoB	I(B	F�B	DsB	A`B	@�B	H�B	J�B	MAB	JcB	VxB	UrB	[�B	QZB	A,B	E�B	K5B	B�B	LoB	aB	qLB	�RB	�zB	��B	�B	�6B	l.B	m4B	s�B	y�B	�B	�-B	�tB	��B	�0B	�pB	�B	�mB	�KB	��B	�1B	�bB	��B
B	�cB	�cB	�B
XB
	�B

�B
	B
�B
RB
�B
�B
6B
B
�B
&B
�B
"sB
�B
�B
[B

B

}B
 B
/�B
�B
 �B	�B
XB	�*B	�{B	�hB	�1B	�B	�B	��B	�B	�xB	��B	�B	��B	�eB	��B	�B	�uB	�B	�B	�B	��B	�WB	��B	ŭB	��B	ǹB	�>B	��B	��B	��B	��B	��B	�>B	�5B	��B	�B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                           G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0061566                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0061566                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 24 11 2022 179 -0.0061566 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 24 11 2022 179 -0.0061566 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230621070056                            20230621070056AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023062107005620230621070056  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023062107005620230621070056QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F83E           783E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023062107005620230621070056QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               