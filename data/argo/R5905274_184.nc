CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-04-26T22:32:25Z creation      
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
_FillValue                 �  [x   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  c`   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �x   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �x   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 8�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � @�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � `(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �\   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �d   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �l   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �t   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �|   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �@   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �P   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �XArgo profile    3.1 1.2 19500101000000  20230426223225  20230426223225  5905274 5905274 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO7315                            7315                            2B  2B  AA  SOLO_II                         SOLO_II                         8643                            8643                            SBE602 11Jan18                  SBE602 11Jan18                  853 853 @����@����11  @��UUh @��UUh @/�8�<!@/�8�<!�d/{��d/{�11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  ?��@   @=p�@�  @��R@�  @��
A ��A�A!�A-p�A@��Aa�A���A�G�A���A��A�Q�A�Q�A�  A�Q�B (�BQ�B(�B�
B   B(Q�B0  B7�
B@(�BH(�BP(�BX  B_�
Bh  Bp  Bx(�B�(�B�{B�  B��B��B��
B��B��B�B��B�  B��B�  B��B��B�{B�{B�=qB�{B�(�B�{B�  B�  B�  B�{B�{B��B�B��
B��
B��B�  C   C��C��C
=C(�C
  C��C��C  C  C  C  C  C
=C
=C
=C   C!��C$
=C&
=C(  C*  C,{C.{C0  C2  C4{C6{C8
=C:
=C<
=C=��C?��CB
=CD  CE��CH
=CJ{CL  CN  CP  CQ��CT  CV  CX  CZ
=C\�C^{C`  Cb  Cd  Cf
=Ch{Cj{Cl
=Cn  Cp
=Cr  Cs��Cv  Cx  Cy��C|
=C~{C��C��C���C���C���C���C�
=C�\C�C���C���C�C�C�C�  C�  C�  C�  C�C�
=C���C���C�  C�
=C���C���C�C�  C���C���C�C�
=C�C�C�  C��C���C���C���C�  C�C�  C�C�
=C�
=C�
=C�
=C�
=C�
=C�  C�  C���C���C�C�C���C�  C�
=C�C�
=C�  C�  C���C��C���C���C�  C�C�  C�C�
=C���C���C���C�C�
=C�C�  C�C�  C�  C�
=C���C���C�  C���C���C�  C�  C�C���C�  C�
=C�  C�  C�  C���C���C���C���C�  C�  C�  C�  C���C���C�  C�C�C�C���C���C���C�C�C���C��C�  C�
=C�C�C�
=C���C���C�C�
=C�C�D   D }qDD��D�qD}qD�qD}qD�qD}qD�qD� DD}qD�RDz�D��DxRD�qD	� D	��D
xRD
��Dz�D�qDz�D  D}qD�qD}qD�D��D��Dz�D  D��D�D��D�D��D  D}qD  D��D  D}qD�qDxRD�qD� D�qD� D�D� D�RD}qD�qDz�D  D� D�D��D  D��D D � D �qD!z�D!�qD"�D#D#�D$�D$}qD%  D%� D%��D&}qD'�D'�D(  D(}qD(�RD)z�D)�qD*}qD+�D+�D,D,��D,�qD-��D.D.��D/�D/� D/��D0� D1D1��D2  D2xRD3  D3�D3��D4}qD5�D5}qD6  D6� D6��D7� D7�qD8��D9  D9z�D:  D:� D:�qD;� D<D<}qD<�RD=z�D>  D>�D?�D?� D@�D@}qDADA�DB�DB��DC  DC� DD�DD}qDEDE}qDF  DF��DG�DG� DG�qDH}qDI�DI� DI��DJ}qDK�DK� DL�DL�DM�DM� DN  DN� DN��DO� DP�DP��DQ  DQ� DR�DR� DR��DSz�DS��DTz�DT�qDU��DVDV�DWDW� DW��DXz�DX��DY}qDZ�DZ��D[  D[� D[�qD\� D]�D]� D^  D^}qD_  D_� D_�qD`z�D`�qDa� Da�qDb� Dc  Dc}qDc�qDd��De  De��Df�Df� Df�qDgz�Dg��Dh� Di  Di� Dj�Dj��Dk�Dk�DlDl� Dl�qDm��Dn�Dn��Dn�qDo� Dp�Dp� Dq�Dq��Dr  Dr}qDr�qDs��DtDt}qDu  Du� Du�qDv�Dw�Dw��Dx�Dx��DyDy��Dz  Dz}qD{�D{z�D|  D|��D|��D}}qD~D~��D~�qD� D��D�>�D�~�D��HD�  D�>�D�� D��HD���D�@ D��HD��HD�  D�AHD�� D�� D�  D�=qD�~�D���D�  D�>�D�� D���D�HD�AHD�� D�� D��qD�AHD�~�D���D�HD�@ D�� D�� D�  D�@ D��HD�� D�  D�AHD��HD�� D�HD�AHD�� D��HD��D�AHD�� D���D�  D�AHD��HD��HD�HD�@ D�� D��HD�HD�AHD�� D��HD�HD�@ D�� D�� D�  D�=qD�� D�� D�  D�AHD�~�D�� D���D�>�D�� D��HD��D�B�D��HD��HD�HD�@ D��HD�� D�HD�AHD�~�D��qD�  D�@ D��HD�� D�HD�@ D�}qD���D�  D�AHD���D��HD�  D�@ D��HD��HD�  D�>�D�~�D��HD�  D�>�D�� D�� D�  D�@ D�� D�� D�  D�@ D�~�D���D���D�=qD��HD��HD�HD�AHD�~�D�� D�HD�AHD�~�D�� D�  D�@ D��HD�D�HD�AHD��HD�� D��qD�@ D�� D�� D�HD�AHD�� D���D��D�B�D��HD��HD�  D�>�D�}qD���D�  D�@ D�� D���D���D�@ D��HD�� D���D�AHD�� D���D�  D�AHD��HD�� D��qD�>�D�� D���D��qD�>�D�� D��HD��D�B�D��HD��HD�  D�>�D��HD�� D���D�@ D���D�� D���D�=qD�~�D��HD�HD�AHD�� D���D���D�=qD�� D���D���D�B�D�� D���D�HD�@ D�}qD�� D�HD�AHD��HD��HD�  D�AHD�� D��HD�  D�>�D�~�D���D���D�@ D��HD�D��D�@ D�}qD���D���D�@ D�� D�� D�HD�AHD�� D��qD���D�=qD�}qD�� D�  D�>�D�� D���D���D�>�D�}qD�� D�  D�AHD�� D��HD�  D�B�D�� D���D�  D�AHD D��HD�  D�>�DÀ D��HD�HD�@ DĀ D��HD�HD�AHD�~�DŽqD��qD�>�D�~�D��HD�HD�@ Dǂ�D�� D�HD�>�D�~�D�� D�  D�AHD�~�DɽqD��D�>�DʁHD�� D���D�>�Dˀ D�� D�  D�@ D̀ D̾�D���D�<)D̀ D�� D�HD�@ D΀ D�� D�  D�>�D�~�DϾ�D�HD�AHDЁHD�� D�HD�AHDсHD�� D�HD�@ D�~�D�� D�HD�>�D�~�D�� D�HD�AHDԀ D�� D�HD�>�DՀ D�� D���D�@ D�~�D�� D�  D�@ D�}qD��HD�  D�AHD�~�DؽqD���D�@ Dـ D��HD���D�=qDځHD�D�  D�>�Dۀ D�� D�  D�@ D܁HDܽqD�HD�AHD݁HD�� D��qD�>�Dހ D��HD�  D�@ D߁HD�� D�  D�@ D��HD��HD��qD�@ D�~�DᾸD�  D�AHD�~�D�qD�HD�B�D� D�� D�  D�>�D�HD�D��D�AHD�~�D徸D�  D�>�D� D��HD��D�>�D�~�D�� D�  D�>�D�~�D辸D��D�=qD�HD��HD��qD�@ D�~�D꾸D�  D�@ D�HD�� D�  D�@ D�~�D�� D�HD�AHD킏D��HD��D�>�D�~�D�� D�  D�AHD� D��HD�  D�@ D�� D�� D�HD�>�D�~�D�D�HD�AHD� D�D�  D�>�D� D��HD�HD�AHD�~�D���D�HD�AHD�~�D���D�  D�@ D�� D��HD��D�@ D�� D�D��D�AHD�� D���D�HD�@ D�~�D�� D�HD�@ D�� D�� D�HD�/\>�\)?8Q�?�z�?�p�?�@\)@!G�@=p�@Tz�@n{@�ff@�@��\@��@�  @���@�
=@��@�33@��RA�A\)AffAp�A%�A,(�A2�\A8Q�A@  AG�AN{AW
=A_\)AfffAmp�As33Az=qA�Q�A�(�A��A��A�  A��A�
=A�=qA�p�A���A���A�  A��
A�  A�(�A��A�33A�\)A��A�p�A���A�(�A���A�(�A׮Aڏ\A���A�  A�\A�A���A�A�{A�Q�A�\A�z�A�A�Q�A�=qA�(�A�ffB z�BB�RB�
B��B�B�RB�Bz�B	��B
�RB�Bz�B�B
=B(�B�B{B
=B�B��B��B�\B�Bz�B�B
=B  B�B=qB33B Q�B ��B!B"�HB$  B$��B%�B'
=B((�B)�B*�\B+�B,��B-B.�RB/�
B0��B1�B2�\B3\)B4z�B5p�B6=qB7�B8��B9B:�HB;�B<z�B=p�B>ffB?\)B@z�BA��BB�RBC�
BD��BEBF�HBH  BHz�BIBJffBK�BLz�BMp�BN�\BO�
BP��BQ�BR�HBS�BTQ�BUG�BV=qBW
=BXQ�BYp�BZffB[\)B\Q�B]p�B^{B^�HB_�
B`��BaBc
=BdQ�BeG�BfffBg\)Bhz�Bi��Bj{Bk
=Bl(�Bm�Bn{Bo33Bp��Bqp�Br�\Bs�Bt  Bt��Bu�Bv�HBw�Bx��By��Bz�HB{�B|��B}��B~{B~�RB�B�(�B��\B���B�p�B��B�Q�B���B�
=B�\)B��B��B�=qB��\B���B��B��
B�=qB�=qB���B��HB�p�B��
B�{B�z�B���B��B�33B��B��
B�(�B�z�B�
=B�\)B��B�{B�(�B�z�B���B�33B��B�{B�ffB���B�33B�\)B��B�  B�ffB���B�p�B��B�(�B�z�B��HB�G�B��
B�ffB��HB�G�B���B�{B��\B�G�B��
B�z�B��RB�\)B��
B���B��B��B�{B��\B�G�B�  B��\B��HB��B�{B���B�p�B�{B�Q�B���B���B�Q�B���B�\)B��
B�z�B�G�B��B�ffB�
=B��B�z�B��B��B�(�B��RB���B�=qB��RB�G�B�{B���B�33B��
B��\B�\)B��
B��\B�G�B�  B�z�B��B��B��RB�G�B��
B���B�p�B��B��\B��B�=qB��\B�G�B�=qB��HB�\)B�  B���BÙ�B�(�B���Bř�B�ffB���BǙ�B�z�B�33B�B�ffB�G�B�{Ḅ�B�G�B�  B���BϮB�=qB��HB��
Bң�B��B�  B���B�p�B�{B��HB�B�Q�B���B�  BڸRB�33B��B���BݮB�(�B���B�B��\B�
=B�B�RB�p�B�  B��HB�B�=qB�
=B��
B�ffB�
=B�{B��B�\)B�{B���B�B�Q�B�G�B��B�z�B�G�B�(�B��B�\)B�Q�B���B���B��\B�G�B��
B��RB��B�  B���B�B�=qB���B��B�ffB�33C 
=C Q�C �C�C�CC�C��C�
C33C�C�CG�C�RC  CQ�C��C
=Cp�CC��C=qC�\CC�C=qCz�C�\C�C��C	�C	(�C	Q�C	�C	��C	�RC	��C
�C
(�C
\)C
�\C
�\C
�C
��C{C�C=qCz�C�C�C�C{C{C=qCz�C��C�C��C
=C�C=qCz�C�\C��C��C
=C�C=qC�C��C�C�HC{C�C=qC�C�C�C�
C{C=qCQ�C�\C�CC�HC�C33CQ�C��C��CC{C33CG�C�\C�RC��C  C=qCG�Cp�C�RC��C  C33CG�CffC�RC��C��C=qCQ�Cz�C�RC�C
=C33Cz�C��CC  CG�CQ�C�C��C�C{CffC�\C�C��C(�CG�Cz�C�RC�
C�CQ�Cp�C�C�HC  C=qC�C��C��C{C(�CffC�CC  C=qC\)C��C�
C��C33CffC�C��C�HC{CffCz�CC��C 
=C \)C z�C ��C �C!  C!(�C!z�C!�\C!C"
=C"{C"ffC"z�C"�C#  C#{C#\)C#�\C#��C#�C$  C$G�C$z�C$��C$�HC$��C%33C%p�C%z�C%��C%�HC&
=C&\)C&p�C&��C&�
C&��C'=qC'Q�C'��C'�RC'�
C((�C(=qC(�C(��C(C)
=C)�C)p�C)�C)��C)��C*
=C*Q�C*p�C*�C*��C+  C+G�C+Q�C+��C+�C+��C,{C,\)C,ffC,�C,�C,��C-G�C-\)C-��C-�C.  C.{C.Q�C.ffC.�RC.C/  C/33C/=qC/�\C/��C/�HC/��C0=qC0G�C0�\C0��C0�HC1  C133C1p�C1z�C1��C1�
C2(�C233C2z�C2��C2�HC2�C3=qC3Q�C3��C3��C3�C4
=C4G�C4\)C4��C4C5  C5{C5Q�C5z�C5��C5�HC5��C6=qC6G�C6�\C6��C6�C6��C7G�C7\)C7��C7�C7��C8
=C8Q�C8ffC8�C8��C9  C933C9Q�C9��C9�C9��C:{C:Q�C:z�C:��C:�
C;{C;=qC;z�C;��C;�C;�C<G�C<ffC<��C<C=  C=(�C=p�C=z�C=�
C=�HC>=qC>G�C>��C>�RC?
=C?�C?p�C?�\C?�
C?��C@=qC@ffC@�C@��CA{CA=qCA�CA��CA��CB{CBffCB�CB�
CC  CCG�CCffCC�RCC�HCD(�CDG�CD��CDCE�CE(�CE�CE��CF  CF{CF\)CF�\CFCG{CG33CG�CG�CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111114114111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                   ?��@   @=p�@�  @��R@�  @��
A ��A�A!�A-p�A@��Aa�A���A�G�A���A��A�Q�A�Q�A�  A�Q�B (�BQ�B(�B�
B   B(Q�B0  B7�
B@(�BH(�BP(�BX  B_�
Bh  Bp  Bx(�B�(�B�{B�  B��B��B��
B��B��B�B��B�  B��B�  B��B��B�{B�{B�=qB�{B�(�B�{B�  B�  B�  B�{B�{B��B�B��
B��
B��B�  C   C��C��C
=C(�C
  C��C��C  C  C  C  C  C
=C
=C
=C   C!��C$
=C&
=C(  C*  C,{C.{C0  C2  C4{C6{C8
=C:
=C<
=C=��C?��CB
=CD  CE��CH
=CJ{CL  CN  CP  CQ��CT  CV  CX  CZ
=C\�C^{C`  Cb  Cd  Cf
=Ch{Cj{Cl
=Cn  Cp
=Cr  Cs��Cv  Cx  Cy��C|
=C~{C��C��C���C���C���C���C�
=C�\C�C���C���C�C�C�C�  C�  C�  C�  C�C�
=C���C���C�  C�
=C���C���C�C�  C���C���C�C�
=C�C�C�  C��C���C���C���C�  C�C�  C�C�
=C�
=C�
=C�
=C�
=C�
=C�  C�  C���C���C�C�C���C�  C�
=C�C�
=C�  C�  C���C��C���C���C�  C�C�  C�C�
=C���C���C���C�C�
=C�C�  C�C�  C�  C�
=C���C���C�  C���C���C�  C�  C�C���C�  C�
=C�  C�  C�  C���C���C���C���C�  C�  C�  C�  C���C���C�  C�C�C�C���C���C���C�C�C���C��C�  C�
=C�C�C�
=C���C���C�C�
=C�C�D   D }qDD��D�qD}qD�qD}qD�qD}qD�qD� DD}qD�RDz�D��DxRD�qD	� D	��D
xRD
��Dz�D�qDz�D  D}qD�qD}qD�D��D��Dz�D  D��D�D��D�D��D  D}qD  D��D  D}qD�qDxRD�qD� D�qD� D�D� D�RD}qD�qDz�D  D� D�D��D  D��D D � D �qD!z�D!�qD"�D#D#�D$�D$}qD%  D%� D%��D&}qD'�D'�D(  D(}qD(�RD)z�D)�qD*}qD+�D+�D,D,��D,�qD-��D.D.��D/�D/� D/��D0� D1D1��D2  D2xRD3  D3�D3��D4}qD5�D5}qD6  D6� D6��D7� D7�qD8��D9  D9z�D:  D:� D:�qD;� D<D<}qD<�RD=z�D>  D>�D?�D?� D@�D@}qDADA�DB�DB��DC  DC� DD�DD}qDEDE}qDF  DF��DG�DG� DG�qDH}qDI�DI� DI��DJ}qDK�DK� DL�DL�DM�DM� DN  DN� DN��DO� DP�DP��DQ  DQ� DR�DR� DR��DSz�DS��DTz�DT�qDU��DVDV�DWDW� DW��DXz�DX��DY}qDZ�DZ��D[  D[� D[�qD\� D]�D]� D^  D^}qD_  D_� D_�qD`z�D`�qDa� Da�qDb� Dc  Dc}qDc�qDd��De  De��Df�Df� Df�qDgz�Dg��Dh� Di  Di� Dj�Dj��Dk�Dk�DlDl� Dl�qDm��Dn�Dn��Dn�qDo� Dp�Dp� Dq�Dq��Dr  Dr}qDr�qDs��DtDt}qDu  Du� Du�qDv�Dw�Dw��Dx�Dx��DyDy��Dz  Dz}qD{�D{z�D|  D|��D|��D}}qD~D~��D~�qD� D��D�>�D�~�D��HD�  D�>�D�� D��HD���D�@ D��HD��HD�  D�AHD�� D�� D�  D�=qD�~�D���D�  D�>�D�� D���D�HD�AHD�� D�� D��qD�AHD�~�D���D�HD�@ D�� D�� D�  D�@ D��HD�� D�  D�AHD��HD�� D�HD�AHD�� D��HD��D�AHD�� D���D�  D�AHD��HD��HD�HD�@ D�� D��HD�HD�AHD�� D��HD�HD�@ D�� D�� D�  D�=qD�� D�� D�  D�AHD�~�D�� D���D�>�D�� D��HD��D�B�D��HD��HD�HD�@ D��HD�� D�HD�AHD�~�D��qD�  D�@ D��HD�� D�HD�@ D�}qD���D�  D�AHD���D��HD�  D�@ D��HD��HD�  D�>�D�~�D��HD�  D�>�D�� D�� D�  D�@ D�� D�� D�  D�@ D�~�D���D���D�=qD��HD��HD�HD�AHD�~�D�� D�HD�AHD�~�D�� D�  D�@ D��HD�D�HD�AHD��HD�� D��qD�@ D�� D�� D�HD�AHD�� D���D��D�B�D��HD��HD�  D�>�D�}qD���D�  D�@ D�� D���D���D�@ D��HD�� D���D�AHD�� D���D�  D�AHD��HD�� D��qD�>�D�� D���D��qD�>�D�� D��HD��D�B�D��HD��HD�  D�>�D��HD�� D���D�@ D���D�� D���D�=qD�~�D��HD�HD�AHD�� D���D���D�=qD�� D���D���D�B�D�� D���D�HD�@ D�}qD�� D�HD�AHD��HD��HD�  D�AHD�� D��HD�  D�>�D�~�D���D���D�@ D��HD�D��D�@ D�}qD���D���D�@ D�� D�� D�HD�AHD�� D��qD���D�=qD�}qD�� D�  D�>�D�� D���D���D�>�D�}qD�� D�  D�AHD�� D��HD�  D�B�D�� D���D�  D�AHD D��HD�  D�>�DÀ D��HD�HD�@ DĀ D��HD�HD�AHD�~�DŽqD��qD�>�D�~�D��HD�HD�@ Dǂ�D�� D�HD�>�D�~�D�� D�  D�AHD�~�DɽqD��D�>�DʁHD�� D���D�>�Dˀ D�� D�  D�@ D̀ D̾�D���D�<)D̀ D�� D�HD�@ D΀ D�� D�  D�>�D�~�DϾ�D�HD�AHDЁHD�� D�HD�AHDсHD�� D�HD�@ D�~�D�� D�HD�>�D�~�D�� D�HD�AHDԀ D�� D�HD�>�DՀ D�� D���D�@ D�~�D�� D�  D�@ D�}qD��HD�  D�AHD�~�DؽqD���D�@ Dـ D��HD���D�=qDځHD�D�  D�>�Dۀ D�� D�  D�@ D܁HDܽqD�HD�AHD݁HD�� D��qD�>�Dހ D��HD�  D�@ D߁HD�� D�  D�@ D��HD��HD��qD�@ D�~�DᾸD�  D�AHD�~�D�qD�HD�B�D� D�� D�  D�>�D�HD�D��D�AHD�~�D徸D�  D�>�D� D��HD��D�>�D�~�D�� D�  D�>�D�~�D辸D��D�=qD�HD��HD��qD�@ D�~�D꾸D�  D�@ D�HD�� D�  D�@ D�~�D�� D�HD�AHD킏D��HD��D�>�D�~�D�� D�  D�AHD� D��HD�  D�@ D�� D�� D�HD�>�D�~�D�D�HD�AHD� D�D�  D�>�D� D��HD�HD�AHD�~�D���D�HD�AHD�~�D���D�  D�@ D�� D��HD��D�@ D�� D�D��D�AHD�� D���D�HD�@ D�~�D�� D�HD�@ D�� D�� D�HD�/\>�\)?8Q�?�z�?�p�?�@\)@!G�@=p�@Tz�@n{@�ff@�@��\@��@�  @���@�
=@��@�33@��RA�A\)AffAp�A%�A,(�A2�\A8Q�A@  AG�AN{AW
=A_\)AfffAmp�As33Az=qA�Q�A�(�A��A��A�  A��A�
=A�=qA�p�A���A���A�  A��
A�  A�(�A��A�33A�\)A��A�p�A���A�(�A���A�(�A׮Aڏ\A���A�  A�\A�A���A�A�{A�Q�A�\A�z�A�A�Q�A�=qA�(�A�ffB z�BB�RB�
B��B�B�RB�Bz�B	��B
�RB�Bz�B�B
=B(�B�B{B
=B�B��B��B�\B�Bz�B�B
=B  B�B=qB33B Q�B ��B!B"�HB$  B$��B%�B'
=B((�B)�B*�\B+�B,��B-B.�RB/�
B0��B1�B2�\B3\)B4z�B5p�B6=qB7�B8��B9B:�HB;�B<z�B=p�B>ffB?\)B@z�BA��BB�RBC�
BD��BEBF�HBH  BHz�BIBJffBK�BLz�BMp�BN�\BO�
BP��BQ�BR�HBS�BTQ�BUG�BV=qBW
=BXQ�BYp�BZffB[\)B\Q�B]p�B^{B^�HB_�
B`��BaBc
=BdQ�BeG�BfffBg\)Bhz�Bi��Bj{Bk
=Bl(�Bm�Bn{Bo33Bp��Bqp�Br�\Bs�Bt  Bt��Bu�Bv�HBw�Bx��By��Bz�HB{�B|��B}��B~{B~�RB�B�(�B��\B���B�p�B��B�Q�B���B�
=B�\)B��B��B�=qB��\B���B��B��
B�=qB�=qB���B��HB�p�B��
B�{B�z�B���B��B�33B��B��
B�(�B�z�B�
=B�\)B��B�{B�(�B�z�B���B�33B��B�{B�ffB���B�33B�\)B��B�  B�ffB���B�p�B��B�(�B�z�B��HB�G�B��
B�ffB��HB�G�B���B�{B��\B�G�B��
B�z�B��RB�\)B��
B���B��B��B�{B��\B�G�B�  B��\B��HB��B�{B���B�p�B�{B�Q�B���B���B�Q�B���B�\)B��
B�z�B�G�B��B�ffB�
=B��B�z�B��B��B�(�B��RB���B�=qB��RB�G�B�{B���B�33B��
B��\B�\)B��
B��\B�G�B�  B�z�B��B��B��RB�G�B��
B���B�p�B��B��\B��B�=qB��\B�G�B�=qB��HB�\)B�  B���BÙ�B�(�B���Bř�B�ffB���BǙ�B�z�B�33B�B�ffB�G�B�{Ḅ�B�G�B�  B���BϮB�=qB��HB��
Bң�B��B�  B���B�p�B�{B��HB�B�Q�B���B�  BڸRB�33B��B���BݮB�(�B���B�B��\B�
=B�B�RB�p�B�  B��HB�B�=qB�
=B��
B�ffB�
=B�{B��B�\)B�{B���B�B�Q�B�G�B��B�z�B�G�B�(�B��B�\)B�Q�B���B���B��\B�G�B��
B��RB��B�  B���B�B�=qB���B��B�ffB�33C 
=C Q�C �C�C�CC�C��C�
C33C�C�CG�C�RC  CQ�C��C
=Cp�CC��C=qC�\CC�C=qCz�C�\C�C��C	�C	(�C	Q�C	�C	��C	�RC	��C
�C
(�C
\)C
�\C
�\C
�C
��C{C�C=qCz�C�C�C�C{C{C=qCz�C��C�C��C
=C�C=qCz�C�\C��C��C
=C�C=qC�C��C�C�HC{C�C=qC�C�C�C�
C{C=qCQ�C�\C�CC�HC�C33CQ�C��C��CC{C33CG�C�\C�RC��C  C=qCG�Cp�C�RC��C  C33CG�CffC�RC��C��C=qCQ�Cz�C�RC�C
=C33Cz�C��CC  CG�CQ�C�C��C�C{CffC�\C�C��C(�CG�Cz�C�RC�
C�CQ�Cp�C�C�HC  C=qC�C��C��C{C(�CffC�CC  C=qC\)C��C�
C��C33CffC�C��C�HC{CffCz�CC��C 
=C \)C z�C ��C �C!  C!(�C!z�C!�\C!C"
=C"{C"ffC"z�C"�C#  C#{C#\)C#�\C#��C#�C$  C$G�C$z�C$��C$�HC$��C%33C%p�C%z�C%��C%�HC&
=C&\)C&p�C&��C&�
C&��C'=qC'Q�C'��C'�RC'�
C((�C(=qC(�C(��C(C)
=C)�C)p�C)�C)��C)��C*
=C*Q�C*p�C*�C*��C+  C+G�C+Q�C+��C+�C+��C,{C,\)C,ffC,�C,�C,��C-G�C-\)C-��C-�C.  C.{C.Q�C.ffC.�RC.C/  C/33C/=qC/�\C/��C/�HC/��C0=qC0G�C0�\C0��C0�HC1  C133C1p�C1z�C1��C1�
C2(�C233C2z�C2��C2�HC2�C3=qC3Q�C3��C3��C3�C4
=C4G�C4\)C4��C4C5  C5{C5Q�C5z�C5��C5�HC5��C6=qC6G�C6�\C6��C6�C6��C7G�C7\)C7��C7�C7��C8
=C8Q�C8ffC8�C8��C9  C933C9Q�C9��C9�C9��C:{C:Q�C:z�C:��C:�
C;{C;=qC;z�C;��C;�C;�C<G�C<ffC<��C<C=  C=(�C=p�C=z�C=�
C=�HC>=qC>G�C>��C>�RC?
=C?�C?p�C?�\C?�
C?��C@=qC@ffC@�C@��CA{CA=qCA�CA��CA��CB{CBffCB�CB�
CC  CCG�CCffCC�RCC�HCD(�CDG�CD��CDCE�CE(�CE�CE��CF  CF{CF\)CF�\CFCG{CG33CG�CG�CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111114114111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AЉ7AГuAП�AП�AП�AП�AП�AС�AС�AП�AС�AС�AС�AУ�AХ�AХ�AХ�AЧ�AЩ�AЬAЬAЮAЩ�AЧ�AЬAЮAЮAа!Aа!Aв-Aв-Aв-Aд9AжFAиRAмjAоwAоwAмjAд9A�t�A��A�
=A�p�A�l�A���Aá�A�ZA���A�M�A�  A�ZA��A�M�A�9XA�;dA�XA��7A���A�?}A�(�A�+A�JA��A���A��RA�7LA���A�r�A���A�Q�A�E�A���A���A�r�A��A�;dA��/A�bA�bA��A���A�O�A��+A��A�p�A�I�A���A�ĜA�~�A���A���A�A��
A���A}��A|Q�Ax��At^5Ao;dAmp�Ah��Ad�Ab-A]ƨA\ffAY��AX�AQ
=AIAE�FACƨACt�A?33A=�-A<��A<$�A;��A:�A7K�A1K�A0�A0�uA0VA0-A0bA/��A0A0r�A/�;A.ffA'��A%hsA$��A#��A#��A$�A$��A$��A#��A#S�A!�TA�FA�hA�TAĜA+A�A��A �A\)A�A|�A��A-A��AO�AoAoAp�A�A��A�+A�#AVAVA	�PA�/A	C�A	�A�`A��A�9Az�A-A�A$�AM�AM�A�RA"�A33AK�AXA&�AffAbA��Ax�AdZA+A�`AȴA�!AA�A��A�AȴAE�A��AG�A ��A ��A ZA (�@���@��#@���@��@� �@��@�+@�=q@��/@���@��@�1'@��H@�=q@��-@��T@��^@�Q�@�@���@�@�hs@�7L@���@�I�@�b@�1'@�ƨ@�F@�|�@�"�@�\@�@���@�9X@�dZ@�
=@��@�7L@��@���@�bN@�33@���@��y@旍@��T@�hs@�Ĝ@�bN@���@�P@�;d@�+@�@�@���@߶F@�@�~�@��#@݉7@݁@݁@�X@���@܃@� �@۶F@ڧ�@���@���@ؼj@�Z@ם�@�|�@�l�@�\)@�33@��@֗�@���@�G�@ԋD@�dZ@���@�~�@�{@��@ѩ�@�?}@ϕ�@Η�@�n�@�V@�@̼j@�1'@ˮ@�|�@�dZ@�K�@���@�V@��@�bN@��m@�t�@�@�E�@��@�Q�@� �@Ý�@��@��H@���@�@�-@���@�`B@�Ĝ@�j@�  @��w@�\)@��y@�{@��@�`B@�7L@���@��@�Z@�9X@�b@���@�^5@���@�X@�V@��9@�bN@�  @���@�t�@�;d@���@���@�$�@��T@��7@���@�r�@�(�@��F@���@�^5@���@�X@��@�z�@�I�@�b@���@�+@��+@�v�@�^5@�{@�G�@��@���@��`@���@��j@���@���@���@��u@��m@�C�@�v�@�{@��@���@�7L@��/@��j@�r�@�9X@�b@��F@�dZ@�o@���@�M�@�J@��@��#@���@���@�x�@�O�@��/@�A�@�ƨ@��F@���@���@�l�@��@��!@�~�@�^5@�=q@�-@�J@���@�hs@�/@���@�z�@�1'@��m@��@�dZ@�o@��@��+@�M�@�$�@��T@���@�x�@�%@��9@��@���@��@�(�@�1@��
@��@�dZ@�+@���@�-@�@���@���@��7@�x�@�X@�&�@���@��9@�9X@��w@�dZ@�;d@�33@�"�@���@��\@�=q@��@���@��7@�x�@�7L@��9@���@�Q�@� �@�b@��m@�K�@�+@��@�
=@��R@���@�V@�@�p�@�&�@���@���@�Z@��m@��@�K�@���@���@���@�v�@��@��-@�X@���@�Ĝ@��@���@��@�Z@��@��w@���@��P@�33@�-@���@��#@��7@��9@���@��@�Z@�9X@��
@�|�@�t�@�K�@�;d@�
=@��y@���@��@��@�O�@�?}@�%@�Ĝ@�Z@�Q�@�A�@��;@�+@��H@���@��@��7@�V@��@�@�@|�@\)@�@~��@~$�@~@}�@}�-@|�@|j@|1@{C�@z=q@y�7@y�@x�@w�;@w��@wl�@w;d@v�y@v�y@vȴ@v��@v5?@u�@u�@t�@tZ@s��@sC�@r��@r-@q��@q7L@pbN@o�@o
=@nv�@n{@m�@m��@m/@l�@l(�@k�F@k33@j~�@j=q@i��@hbN@h �@g��@g
=@fv�@fE�@f@e�@d�/@d��@d��@dI�@c��@cC�@co@b��@bM�@bJ@a�^@aX@`Ĝ@`�@`b@_|�@_K�@^��@]��@\��@\9X@[�F@[C�@Z�!@Z^5@Y��@Y&�@Xr�@W�@W�@W�;@W��@V��@V5?@U�@U�@T��@T��@S�m@S�@SC�@R�@R��@R~�@R^5@R=q@R�@Q��@Q�@QX@PbN@P �@O�@O�w@O|�@O+@N��@N�@Nff@M�-@M�@M?}@MV@L�/@L�j@L�D@LI�@K��@K��@K�@KS�@KC�@Ko@J�H@J�H@J~�@J=q@JJ@Ix�@HĜ@Hr�@H  @G�@G\)@GK�@G+@G�@F�@Fff@E��@E`B@EO�@EO�@E�@D�/@D�j@Dj@D9X@D(�@D1@C�m@Ct�@CC�@C"�@C33@Co@A��@AX@A%@@��@@A�@@1'@@1'@@  @?�;@?��@>��@=�h@=�@<��@<��@;�m@;o@:�!@:M�@9�#@9�7@9�@8��@8��@8�@8�@8�@8�@81'@7��@7�@7�P@7K�@7K�@7;d@6ȴ@6�+@6v�@6V@5�@5@5�h@5?}@4�D@4I�@4(�@3�m@3�F@3��@3dZ@3@2�\@2M�@1�#@1��@1hs@1�@0��@0  @/�@/�P@/|�@/K�@.�y@.ff@-�@-V@,�j@,z�@,9X@+��@+�F@+S�@+o@*�H@*^5@)��@)��@)7L@(�`@( �@'\)@';d@'+@&�@&ff@%�T@%/@$�j@$9X@#�m@#��@#33@"�H@"~�@"M�@!�@!��@!G�@ �`@ �`@ �u@ A�@�;@|�@�@�R@��@��@v�@�+@v�@v�@ff@ff@ff@5?@�T@�-@�@?}@�@�@V@V@��@j@I�@9X@�@�m@ƨ@��@�@dZ@S�@33@�@��@�\@=q@=q@=q@J@�7@hs@G�@&�@�@��@Ĝ@Ĝ@�9@��@�@r�@r�@bN@Q�@A�@b@�@�@�;@�;@�;@�;@�;@�w@�w@�@��@��@�P@l�@K�@
=@�y@ȴ@�+@V@�T@��@@��@O�@�@�@I�@�@�
@t�@@��@~�@~�@~�@M�@�@�7@x�@7L@&�@�@��@Ĝ@�9@��@�@bN@ �@  @��@�w@�w@�w@�@�@�@�P@|�@K�@K�@K�@;d@+@�@�@�y@��@�+@v�@ff@V@V@5?@{@�T@��@@@�-@��@�h@�@`B@O�@?}@/@�@��@�/@�j@�@��@z�@Z@Z@I�@(�@1@ƨ@��@C�@"�@o@
�@
��@
�\@
-@
�@	�@	�^@	��@	X@	G�@	7L@	�@��@��@r�@b@�@�;@�@;d@�@��@�@ȴ@�+@v�@{@@�-AЍPA�v�AЗ�AЕ�A�x�AЛ�AН�AП�AС�AС�AС�AН�AН�AН�AП�AН�AП�AС�AУ�AУ�AУ�AС�AП�AН�AН�AП�AП�AС�AХ�AХ�AХ�AУ�AП�AН�AП�AУ�AХ�AЧ�AЧ�AХ�AХ�AУ�AУ�AС�AС�AХ�AХ�AЧ�AЧ�AЧ�AХ�AУ�AХ�AУ�AХ�AЩ�AЬAЬAЬAЧ�AЧ�AЧ�AЩ�AЬAЮAЮAЬAЬAЩ�AЩ�AЩ�AЩ�AЩ�AЮAЮAа!Aа!Aа!AЮAЬAЬAЬAЬAЬAЬAЬAЬAЧ�AЧ�AЮAЧ�AЧ�AХ�AУ�AХ�AЧ�AХ�AЩ�AЬAа!Aа!Aа!AЮAЬAЬAЩ�AЩ�AЬAЩ�AЩ�AЬAЮAа!Aа!Aа!Aв-Aа!Aа!Aв-Aа!AЮAЮAЮAЬAЮAЮAЮAа!Aа!Aв-Aв-Aв-Aа!Aа!AЮAЮAа!Aд9Aд9Aд9Aд9Aд9Aв-Aв-Aв-Aа!Aа!Aа!Aа!Aв-Aв-Aд9Aд9Aд9Aд9Aд9Aв-Aа!Aа!Aа!Aв-Aв-Aв-AжFAжFAжFAд9Aд9Aд9Aв-Aд9Aд9AжFAжFAжFAиRAжFAиRAиRAиRAжFAд9AжFAжFAмjAмjAмjAоwAоwAмjAк^Aк^Aк^Aк^AоwA���A���AоwA���AоwAмjAмjAк^Aк^AмjAоwAоwA���A���A���A���AоwAмjAк^AиRAк^AмjAмjA���AоwAоwAмjAиRAд9Aд9AжFAжFAд9Aв-Aв-Aа!Aа!Aв-AжFAжFAжFAжFAв-AЩ�AЛ�AЇ+A�dZA�dZA�`BA�S�A�9XA��A��A���Aϛ�A�/AθRA�{A�VA�7LA�VA�A�E�A�ƨA�oA�/A�ffA�M�AƓuA�JA�S�A�$�A��/A�z�A�t�Aº^A�hsAA�7LA�dZA�l�A���A���A���A�(�A�`BA�n�A�ffA�G�A��`A���Aİ!Aĝ�Aď\A�n�A�E�A�$�Aç�A�1'A�A��A�bA��`A�$�A�%A��A��A��#A��-A���A��A�r�A�VA�=qA�1A���A��DA��A��7A��#A�XA�"�A���A���A���A��!A���A��7A�jA�&�A�
=A�A��TA���A��mA��A���A�"�A�JA���A���A��;A�v�A��A��
A���A�dZA���A�r�A�"�A���A��hA�A��RA�7LA�%A��FA�~�A�Q�A���A�v�A�9XA���A��-A�A��A�5?A��`A��9A���A��A�r�A�dZA�A�A�1'A�(�A�%A�-A�x�A���A�z�A�dZA�VA�A�A�-A��A��A��A�bA���A���A���A�33A�?}A�5?A�+A�1A���A��yA��
A��TA��A�&�A�?}A�A�A���A��+A�p�A�dZA�ZA�Q�A�C�A�&�A� �A��A��A�1A���A��A��HA���A���A�jA�VA�Q�A�K�A�A�A�33A�1A�ĜA���A���A��uA��PA�M�A��A���A��jA�hsA�33A���A��/A��FA��-A���A���A���A�n�A�XA�E�A�33A� �A�ȴA���A��\A��\A��hA��PA��DA��PA��7A��A��A�~�A�t�A�p�A�p�A�jA�dZA�\)A�ZA�Q�A�K�A�G�A�?}A�33A�+A�"�A��A�oA�bA�bA�JA�1A�1A�A���A��;A�ȴA��9A���A��uA�z�A�`BA�I�A�(�A��HA��\A�VA��A�A���A���A��A��A��TA��TA��#A���A���A���A�A�A���A��jA��wA��^A��RA��-A��-A�|�A�JA�hsA�33A�
=A���A��A���A�|�A�^5A�;dA� �A�%A��A��A�ƨA��A���A�|�A�VA�1'A�JA��`A��wA���A�r�A�Q�A� �A���A��HA���A�S�A��A���A�dZA�;dA��A�A���A���A��!A��hA�z�A�jA�VA�?}A�5?A�/A��A�bA�VA�A��A��wA��7A�n�A�hsA�VA�5?A��A�  A��A��wA���A��PA��A�l�A�\)A�XA�C�A��A���A���A��PA��A�p�A�ffA�S�A�C�A�7LA�&�A�VA��A�ĜA��PA�p�A�XA�7LA�{A��`A��+A�Q�A�7LA�A���A�S�A��;A�^5A�/A��A�VA��A���A��A�M�A��A��#A��FA���A���A�p�A�`BA�S�A�I�A�oA�r�A��A��A��^A��PA�~�A�n�A�bNA�XA�C�A�A�A�/A�
=A���A��uA�+A�|�A���A�dZA�Q�A��FA�?}A�$�A���A�  A��/A�ƨA��jA��!A���A�p�A�"�A��^A� �A��
A�ĜA���A���A��DA��A�x�A�`BA�Q�A�=qA�-A��A��A�%A���A���A��A�ĜA�z�A���A�E�A��/A���A�Q�A���A�ȴA��A�5?A��A%A~�!A~��A~��A~bA}��A}��A}�-A}�A}hsA}C�A}&�A|��A|�A|�/A|�A|��A|��A|��A|v�A|A�A|=qA|�A{�A{��A{oAz �Ay��AydZAy7LAy%Ax��Ax�\AxI�Ax  Aw��Aw33Aw
=Av��Au��At5?As�
As�-As�7AsXAr�yAr�Aq��Ap�Apv�Ap1Ao�^An��An$�AnbAm��Am�Am�mAm�TAm��Am��Am�^Am��Aml�Am7LAmAl��Al-Ak�Ak7LAj�\AiK�Ahr�Ag��AghsAf�jAf �Ae�PAeO�Ae&�Ad��Ad��AdZAdAc�FAc�-Ac��Ac�PAchsAc?}Ab�HAb��Aa��A`�A_�A^�9A^��A^~�A^A]��A]dZA]S�A]7LA]A\��A\�!A\��A\�\A\~�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                   AЉ7AГuAП�AП�AП�AП�AП�AС�AС�AП�AС�AС�AС�AУ�AХ�AХ�AХ�AЧ�AЩ�AЬAЬAЮAЩ�AЧ�AЬAЮAЮAа!Aа!Aв-Aв-Aв-Aд9AжFAиRAмjAоwAоwAмjAд9A�t�A��A�
=A�p�A�l�A���Aá�A�ZA���A�M�A�  A�ZA��A�M�A�9XA�;dA�XA��7A���A�?}A�(�A�+A�JA��A���A��RA�7LA���A�r�A���A�Q�A�E�A���A���A�r�A��A�;dA��/A�bA�bA��A���A�O�A��+A��A�p�A�I�A���A�ĜA�~�A���A���A�A��
A���A}��A|Q�Ax��At^5Ao;dAmp�Ah��Ad�Ab-A]ƨA\ffAY��AX�AQ
=AIAE�FACƨACt�A?33A=�-A<��A<$�A;��A:�A7K�A1K�A0�A0�uA0VA0-A0bA/��A0A0r�A/�;A.ffA'��A%hsA$��A#��A#��A$�A$��A$��A#��A#S�A!�TA�FA�hA�TAĜA+A�A��A �A\)A�A|�A��A-A��AO�AoAoAp�A�A��A�+A�#AVAVA	�PA�/A	C�A	�A�`A��A�9Az�A-A�A$�AM�AM�A�RA"�A33AK�AXA&�AffAbA��Ax�AdZA+A�`AȴA�!AA�A��A�AȴAE�A��AG�A ��A ��A ZA (�@���@��#@���@��@� �@��@�+@�=q@��/@���@��@�1'@��H@�=q@��-@��T@��^@�Q�@�@���@�@�hs@�7L@���@�I�@�b@�1'@�ƨ@�F@�|�@�"�@�\@�@���@�9X@�dZ@�
=@��@�7L@��@���@�bN@�33@���@��y@旍@��T@�hs@�Ĝ@�bN@���@�P@�;d@�+@�@�@���@߶F@�@�~�@��#@݉7@݁@݁@�X@���@܃@� �@۶F@ڧ�@���@���@ؼj@�Z@ם�@�|�@�l�@�\)@�33@��@֗�@���@�G�@ԋD@�dZ@���@�~�@�{@��@ѩ�@�?}@ϕ�@Η�@�n�@�V@�@̼j@�1'@ˮ@�|�@�dZ@�K�@���@�V@��@�bN@��m@�t�@�@�E�@��@�Q�@� �@Ý�@��@��H@���@�@�-@���@�`B@�Ĝ@�j@�  @��w@�\)@��y@�{@��@�`B@�7L@���@��@�Z@�9X@�b@���@�^5@���@�X@�V@��9@�bN@�  @���@�t�@�;d@���@���@�$�@��T@��7@���@�r�@�(�@��F@���@�^5@���@�X@��@�z�@�I�@�b@���@�+@��+@�v�@�^5@�{@�G�@��@���@��`@���@��j@���@���@���@��u@��m@�C�@�v�@�{@��@���@�7L@��/@��j@�r�@�9X@�b@��F@�dZ@�o@���@�M�@�J@��@��#@���@���@�x�@�O�@��/@�A�@�ƨ@��F@���@���@�l�@��@��!@�~�@�^5@�=q@�-@�J@���@�hs@�/@���@�z�@�1'@��m@��@�dZ@�o@��@��+@�M�@�$�@��T@���@�x�@�%@��9@��@���@��@�(�@�1@��
@��@�dZ@�+@���@�-@�@���@���@��7@�x�@�X@�&�@���@��9@�9X@��w@�dZ@�;d@�33@�"�@���@��\@�=q@��@���@��7@�x�@�7L@��9@���@�Q�@� �@�b@��m@�K�@�+@��@�
=@��R@���@�V@�@�p�@�&�@���@���@�Z@��m@��@�K�@���@���@���@�v�@��@��-@�X@���@�Ĝ@��@���@��@�Z@��@��w@���@��P@�33@�-@���@��#@��7@��9@���@��@�Z@�9X@��
@�|�@�t�@�K�@�;d@�
=@��y@���@��@��@�O�@�?}@�%@�Ĝ@�Z@�Q�@�A�@��;@�+@��H@���@��@��7@�V@��@�@�@|�@\)@�@~��@~$�@~@}�@}�-@|�@|j@|1@{C�@z=q@y�7@y�@x�@w�;@w��@wl�@w;d@v�y@v�y@vȴ@v��@v5?@u�@u�@t�@tZ@s��@sC�@r��@r-@q��@q7L@pbN@o�@o
=@nv�@n{@m�@m��@m/@l�@l(�@k�F@k33@j~�@j=q@i��@hbN@h �@g��@g
=@fv�@fE�@f@e�@d�/@d��@d��@dI�@c��@cC�@co@b��@bM�@bJ@a�^@aX@`Ĝ@`�@`b@_|�@_K�@^��@]��@\��@\9X@[�F@[C�@Z�!@Z^5@Y��@Y&�@Xr�@W�@W�@W�;@W��@V��@V5?@U�@U�@T��@T��@S�m@S�@SC�@R�@R��@R~�@R^5@R=q@R�@Q��@Q�@QX@PbN@P �@O�@O�w@O|�@O+@N��@N�@Nff@M�-@M�@M?}@MV@L�/@L�j@L�D@LI�@K��@K��@K�@KS�@KC�@Ko@J�H@J�H@J~�@J=q@JJ@Ix�@HĜ@Hr�@H  @G�@G\)@GK�@G+@G�@F�@Fff@E��@E`B@EO�@EO�@E�@D�/@D�j@Dj@D9X@D(�@D1@C�m@Ct�@CC�@C"�@C33@Co@A��@AX@A%@@��@@A�@@1'@@1'@@  @?�;@?��@>��@=�h@=�@<��@<��@;�m@;o@:�!@:M�@9�#@9�7@9�@8��@8��@8�@8�@8�@8�@81'@7��@7�@7�P@7K�@7K�@7;d@6ȴ@6�+@6v�@6V@5�@5@5�h@5?}@4�D@4I�@4(�@3�m@3�F@3��@3dZ@3@2�\@2M�@1�#@1��@1hs@1�@0��@0  @/�@/�P@/|�@/K�@.�y@.ff@-�@-V@,�j@,z�@,9X@+��@+�F@+S�@+o@*�H@*^5@)��@)��@)7L@(�`@( �@'\)@';d@'+@&�@&ff@%�T@%/@$�j@$9X@#�m@#��@#33@"�H@"~�@"M�@!�@!��@!G�@ �`@ �`@ �u@ A�@�;@|�@�@�R@��@��@v�@�+@v�@v�@ff@ff@ff@5?@�T@�-@�@?}@�@�@V@V@��@j@I�@9X@�@�m@ƨ@��@�@dZ@S�@33@�@��@�\@=q@=q@=q@J@�7@hs@G�@&�@�@��@Ĝ@Ĝ@�9@��@�@r�@r�@bN@Q�@A�@b@�@�@�;@�;@�;@�;@�;@�w@�w@�@��@��@�P@l�@K�@
=@�y@ȴ@�+@V@�T@��@@��@O�@�@�@I�@�@�
@t�@@��@~�@~�@~�@M�@�@�7@x�@7L@&�@�@��@Ĝ@�9@��@�@bN@ �@  @��@�w@�w@�w@�@�@�@�P@|�@K�@K�@K�@;d@+@�@�@�y@��@�+@v�@ff@V@V@5?@{@�T@��@@@�-@��@�h@�@`B@O�@?}@/@�@��@�/@�j@�@��@z�@Z@Z@I�@(�@1@ƨ@��@C�@"�@o@
�@
��@
�\@
-@
�@	�@	�^@	��@	X@	G�@	7L@	�@��@��@r�@b@�@�;@�@;d@�@��@�@ȴ@�+@v�@{@@�-AЍPA�v�AЗ�AЕ�A�x�AЛ�AН�AП�AС�AС�AС�AН�AН�AН�AП�AН�AП�AС�AУ�AУ�AУ�AС�AП�AН�AН�AП�AП�AС�AХ�AХ�AХ�AУ�AП�AН�AП�AУ�AХ�AЧ�AЧ�AХ�AХ�AУ�AУ�AС�AС�AХ�AХ�AЧ�AЧ�AЧ�AХ�AУ�AХ�AУ�AХ�AЩ�AЬAЬAЬAЧ�AЧ�AЧ�AЩ�AЬAЮAЮAЬAЬAЩ�AЩ�AЩ�AЩ�AЩ�AЮAЮAа!Aа!Aа!AЮAЬAЬAЬAЬAЬAЬAЬAЬAЧ�AЧ�AЮAЧ�AЧ�AХ�AУ�AХ�AЧ�AХ�AЩ�AЬAа!Aа!Aа!AЮAЬAЬAЩ�AЩ�AЬAЩ�AЩ�AЬAЮAа!Aа!Aа!Aв-Aа!Aа!Aв-Aа!AЮAЮAЮAЬAЮAЮAЮAа!Aа!Aв-Aв-Aв-Aа!Aа!AЮAЮAа!Aд9Aд9Aд9Aд9Aд9Aв-Aв-Aв-Aа!Aа!Aа!Aа!Aв-Aв-Aд9Aд9Aд9Aд9Aд9Aв-Aа!Aа!Aа!Aв-Aв-Aв-AжFAжFAжFAд9Aд9Aд9Aв-Aд9Aд9AжFAжFAжFAиRAжFAиRAиRAиRAжFAд9AжFAжFAмjAмjAмjAоwAоwAмjAк^Aк^Aк^Aк^AоwA���A���AоwA���AоwAмjAмjAк^Aк^AмjAоwAоwA���A���A���A���AоwAмjAк^AиRAк^AмjAмjA���AоwAоwAмjAиRAд9Aд9AжFAжFAд9Aв-Aв-Aа!Aа!Aв-AжFAжFAжFAжFAв-AЩ�AЛ�AЇ+A�dZA�dZA�`BA�S�A�9XA��A��A���Aϛ�A�/AθRA�{A�VA�7LA�VA�A�E�A�ƨA�oA�/A�ffA�M�AƓuA�JA�S�A�$�A��/A�z�A�t�Aº^A�hsAA�7LA�dZA�l�A���A���A���A�(�A�`BA�n�A�ffA�G�A��`A���Aİ!Aĝ�Aď\A�n�A�E�A�$�Aç�A�1'A�A��A�bA��`A�$�A�%A��A��A��#A��-A���A��A�r�A�VA�=qA�1A���A��DA��A��7A��#A�XA�"�A���A���A���A��!A���A��7A�jA�&�A�
=A�A��TA���A��mA��A���A�"�A�JA���A���A��;A�v�A��A��
A���A�dZA���A�r�A�"�A���A��hA�A��RA�7LA�%A��FA�~�A�Q�A���A�v�A�9XA���A��-A�A��A�5?A��`A��9A���A��A�r�A�dZA�A�A�1'A�(�A�%A�-A�x�A���A�z�A�dZA�VA�A�A�-A��A��A��A�bA���A���A���A�33A�?}A�5?A�+A�1A���A��yA��
A��TA��A�&�A�?}A�A�A���A��+A�p�A�dZA�ZA�Q�A�C�A�&�A� �A��A��A�1A���A��A��HA���A���A�jA�VA�Q�A�K�A�A�A�33A�1A�ĜA���A���A��uA��PA�M�A��A���A��jA�hsA�33A���A��/A��FA��-A���A���A���A�n�A�XA�E�A�33A� �A�ȴA���A��\A��\A��hA��PA��DA��PA��7A��A��A�~�A�t�A�p�A�p�A�jA�dZA�\)A�ZA�Q�A�K�A�G�A�?}A�33A�+A�"�A��A�oA�bA�bA�JA�1A�1A�A���A��;A�ȴA��9A���A��uA�z�A�`BA�I�A�(�A��HA��\A�VA��A�A���A���A��A��A��TA��TA��#A���A���A���A�A�A���A��jA��wA��^A��RA��-A��-A�|�A�JA�hsA�33A�
=A���A��A���A�|�A�^5A�;dA� �A�%A��A��A�ƨA��A���A�|�A�VA�1'A�JA��`A��wA���A�r�A�Q�A� �A���A��HA���A�S�A��A���A�dZA�;dA��A�A���A���A��!A��hA�z�A�jA�VA�?}A�5?A�/A��A�bA�VA�A��A��wA��7A�n�A�hsA�VA�5?A��A�  A��A��wA���A��PA��A�l�A�\)A�XA�C�A��A���A���A��PA��A�p�A�ffA�S�A�C�A�7LA�&�A�VA��A�ĜA��PA�p�A�XA�7LA�{A��`A��+A�Q�A�7LA�A���A�S�A��;A�^5A�/A��A�VA��A���A��A�M�A��A��#A��FA���A���A�p�A�`BA�S�A�I�A�oA�r�A��A��A��^A��PA�~�A�n�A�bNA�XA�C�A�A�A�/A�
=A���A��uA�+A�|�A���A�dZA�Q�A��FA�?}A�$�A���A�  A��/A�ƨA��jA��!A���A�p�A�"�A��^A� �A��
A�ĜA���A���A��DA��A�x�A�`BA�Q�A�=qA�-A��A��A�%A���A���A��A�ĜA�z�A���A�E�A��/A���A�Q�A���A�ȴA��A�5?A��A%A~�!A~��A~��A~bA}��A}��A}�-A}�A}hsA}C�A}&�A|��A|�A|�/A|�A|��A|��A|��A|v�A|A�A|=qA|�A{�A{��A{oAz �Ay��AydZAy7LAy%Ax��Ax�\AxI�Ax  Aw��Aw33Aw
=Av��Au��At5?As�
As�-As�7AsXAr�yAr�Aq��Ap�Apv�Ap1Ao�^An��An$�AnbAm��Am�Am�mAm�TAm��Am��Am�^Am��Aml�Am7LAmAl��Al-Ak�Ak7LAj�\AiK�Ahr�Ag��AghsAf�jAf �Ae�PAeO�Ae&�Ad��Ad��AdZAdAc�FAc�-Ac��Ac�PAchsAc?}Ab�HAb��Aa��A`�A_�A^�9A^��A^~�A^A]��A]dZA]S�A]7LA]A\��A\�!A\��A\�\A\~�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	1�B	2-B	2aB	2aB	2-B	2�B	2aB	2-B	2aB	1�B	1�B	2-B	2�B	2�B	2aB	1�B	1�B	2-B	2aB	2�B	2�B	33B	2�B	2�B	2�B	2�B	2�B	2�B	2�B	2�B	2�B	3hB	33B	2�B	33B	3�B	4B	3�B	2-B	0UB	V�B	��B
%zB
_�Be,B�B�BB�B\B��B�BRTB�+B�By�B.Bw2BXBD�B6B!bB1'BiBj�B�B��B�uB}�Bv�Bm�B\]BG�B=qB5�B/�B&�B�B�B��B��B�B��B�~Bw2B`BBOBB$�B
��B
�<B
��B
i�B
L�B
3�B	�cB	ޞB	�aB	�<B	��B	�(B	��B	{B	e,B	`vB	P�B	H�B	=�B	/�B	�B��B�%B�B��B�B�>B��B	 �B	�B	
	B	
�B�B��BںBچB�QBیB�B��B�B	B	�B��B�BB�B��B��B	�B	 �B	0!B	33B	;�B	9�B	.�B	!�B	�B	eB	�B	B	_B	VB	 �B	�B	�B	�B	�B	($B	>wB	@OB	<jB	)*B	#:B	VB	%FB	�B		B	MB��B�]B	�B	eB	�B	IB	�B	�B	CB	!�B	3�B	:�B	EB	L�B	\]B	]dB	_�B	c�B	o�B	uZB	u%B	v`B	v+B	v`B	zxB	|B	|�B	}VB	�B	�oB	��B	�GB	��B	�YB	�B	�{B	��B	��B	�;B	�B	��B	�1B	�fB	�lB	�lB	�fB	�lB	��B	�%B	��B	�@B	�bB	��B	��B	��B	��B	��B	��B	�	B	��B	�B	��B	��B	�FB	�B	�eB	�B	��B	��B	��B	��B	��B	�nB	�?B	�B	��B	��B	��B	�LB	�B	�XB	��B	��B	��B	��B	�qB	�B	�}B	�OB	��B	��B	��B	�9B	��B	ŢB	ȴB	�zB	ȴB	��B	�XB	ɆB	ɆB	�RB	ɺB	�#B	��B	��B	��B	��B	��B	��B	�6B	�jB	�vB	�B	��B	�B	�B	�BB	�B	�NB	уB	�&B	�2B	ԕB	�2B	��B	ԕB	��B	�,B	��B	֡B	�9B	�9B	��B	��B	چB	�QB	�QB	�B	��B	��B	�QB	�#B	�jB	یB	��B	یB	�WB	ܒB	یB	ںB	ܒB	��B	ܒB	��B	�]B	�dB	�/B	�B	ޞB	�pB	�pB	�vB	��B	��B	�,B	��B	��B	�B	�B	��B	�
B	�B	�2B	��B	�B	�sB	�B	�B	�yB	��B	�B	�B	�KB	��B	�B	��B	��B	�B	��B	�cB	�B	�B	��B	�B	��B	�|B	�B	�+B	��B	�`B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�B	��B	�B	�B	�B	��B	�VB	��B	�(B	��B	��B	�(B	��B	�cB	��B
 4B
  B
 iB
B
�B
GB
{B
B
MB
MB
B
MB
�B
MB
�B
SB
�B
+B
+B
�B
�B
+B
�B
�B
�B
�B
�B
�B
�B
1B
�B
�B
	B
	lB
	�B

	B

=B

�B

�B
B
�B
�B
B
JB
JB
�B
"B
VB
VB
VB
"B
\B
(B
(B
\B
�B
�B
�B
�B
�B
B
:B
oB
�B
�B
�B
FB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
eB
eB
1B
B
kB
kB
	B
	B
=B
=B
�B
�B
�B
~B
~B
IB
VB
�B
�B
!B
VB
�B
�B
!bB
"4B
#nB
#�B
$�B
%�B
%FB
%�B
&�B
'�B
(�B
(�B
(�B
(XB
(�B
)*B
)_B
)�B
)_B
(�B
)_B
)_B
*0B
*�B
+B
,=B
+�B
,B
,=B
-B
,�B
.B
.�B
/�B
/B
0UB
0�B
0�B
1'B
1�B
1�B
1[B
1�B
2�B
2�B
2aB
2-B
4B
3�B
3�B
33B
3hB
2�B
4�B
5?B
6B
5�B
6zB
6B
6FB
7�B
7LB
6zB
6zB
6�B
7�B
7LB
7�B
8�B
:^B
:�B
:�B
;�B
;�B
;�B
<B
;�B
<B
;�B
;�B
;�B
<�B
=<B
<�B
<�B
=<B
=<B
>B
>�B
?HB
?HB
?}B
?�B
?�B
AUB
AUB
A�B
AUB
A B
B'B
A�B
B�B
A�B
B�B
C-B
B�B
CaB
CaB
B�B
C�B
D�B
E�B
EB
E�B
F?B
E9B
EmB
D�B
E�B
E�B
FtB
FtB
G�B
G�B
HKB
IB
H�B
H�B
HKB
H�B
I�B
IB
J#B
J�B
J�B
K�B
K�B
MB
M6B
M6B
N<B
NpB
OvB
OB
N�B
OB
O�B
PHB
QNB
Q�B
Q�B
QNB
QNB
RTB
R B
RTB
S&B
S�B
S�B
S[B
S�B
S�B
R�B
R�B
TaB
TaB
S�B
T�B
S�B
TaB
T�B
T,B
S�B
U�B
U�B
UgB
VmB
VmB
VmB
U�B
U�B
U�B
V9B
VmB
VmB
V�B
WsB
W�B
W�B
WsB
W�B
XB
YB
YB
YB
YKB
Y�B
Y�B
ZB
Y�B
Y�B
YB
YKB
Y�B
Y�B
Y�B
Y�B
Y�B
Z�B
Z�B
Z�B
[�B
[�B
\]B
\�B
]/B
^B
^B
]�B
]dB
]/B
]�B
]dB
]�B
^�B
^�B
_;B
_pB
_pB
_;B
`B
a|B
`�B
aB
aHB
a�B
c�B
d�B
e,B
d�B
e�B
e`B
e`B
e,B
e�B
e�B
f2B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
ffB
f2B
ffB
f�B
gmB
g8B
gB
h
B
h�B
h>B
h>B
h�B
iDB
iDB
iDB
iB
jB
i�B
jB
jB
jKB
kB
kB
k�B
lWB
l�B
k�B
l�B
l�B
l�B
m�B
m�B
m�B
n/B
n�B
o B
oiB
o�B
p�B
p;B
p;B
p�B
qB
rB
q�B
r|B
r�B
sMB
r�B
r|B
sMB
s�B
t�B
tTB
u%B
u%B
t�B
u�B
u%B
u�B
u�B
v+B
v`B
v`B
w2B
v�B
v�B
w�B
xB
w�B
y	B
y	B
x�B
x�B
y>B
x�B
y	B
x�B
y	B
x�B
x�B
x�B
y�B
y>B
y>B
y�B
y�B
zDB
zB
yrB
y�B
zxB
zDB
zB
{B
{B
z�B
z�B
{JB
{JB
z�B
z�B
{JB
{B
|B
|�B
|PB
{�B
|�B
}"B
|�B
}�B
}�B
}VB
}VB
~(B
}�B
~]B
~�B
~�B
~�B
~�B
.B
cB
�B
�4B
�4B
� B
� B
�4B
� B
� B
� B
�4B
�4B
� B
�4B
� B
��B
�4B
��B
�oB
��B
�;B
��B
�oB
�AB
�AB
�AB
�uB
�uB
��B
��B
�GB
�uB
�{B
�GB
��B
�MB
��B
��B
�{B
�MB
��B
�B
�SB
�%B
��B
�YB
�%B
��B
��B
��B
��B
��B
��B
�YB
��B
��B
��B
�+B
�+B
�+B
�_B
�_B
��B
��B
��B
�1B
��B
�fB
��B
�fB
��B
�lB
�B
�B
��B
��B
��B
�B
�B
�7B
��B
��B
�B
��B
��B
�lB
��B
�=B
�=B
�	B
�	B
�	B
��B
��B
�rB
��B
�rB
�=B
��B
��B
��B
��B
�B
�xB
��B
�JB
�JB
�JB
�~B
��B
�B
��B
��B
�PB
�PB
��B
��B
��B
��B
�"B
�"B
��B
�(B
��B
�\B
�\B
��B
�.B
��B
��B
��B
� B
� B
��B
�4B
�4B
�hB	+�B	2aB	.IB	0�B	8B	33B	2�B	1�B	1�B	1�B	1�B	2�B	2�B	3hB	33B	3�B	2�B	1�B	1�B	1[B	2-B	2aB	2aB	2�B	2�B	2�B	2�B	1'B	1�B	1[B	1�B	2�B	2�B	3�B	3�B	2aB	2-B	1�B	1�B	1�B	1�B	2�B	33B	2�B	2-B	1'B	0�B	0UB	0�B	0�B	1�B	1�B	1�B	3�B	1�B	1�B	1�B	1[B	1�B	2�B	33B	3hB	3hB	2aB	1�B	2-B	2�B	2�B	3�B	4B	3�B	4B	3�B	2aB	2aB	2aB	2�B	2�B	2�B	4B	49B	4B	3�B	3�B	3hB	3hB	3�B	2�B	1�B	0!B	3hB	1�B	2�B	2�B	2�B	3�B	3�B	2-B	2-B	1[B	2aB	1�B	2aB	2�B	3�B	3�B	3hB	4B	3�B	3hB	33B	2�B	2-B	1�B	1�B	1�B	2-B	2aB	2aB	33B	3hB	4B	4B	49B	3�B	3�B	3�B	33B	2�B	2aB	2-B	1�B	2�B	3�B	4B	3�B	33B	1�B	1�B	1�B	2aB	2�B	33B	2�B	33B	3�B	3�B	3�B	3hB	33B	3�B	2�B	2�B	2aB	2�B	2�B	3�B	49B	49B	4nB	4B	3�B	3�B	2-B	2aB	2�B	33B	33B	2�B	3�B	3hB	3hB	2�B	2�B	2-B	1�B	2aB	2aB	2aB	3�B	4B	4B	4nB	2�B	2aB	2aB	2�B	33B	33B	3�B	4nB	4�B	4nB	3�B	2�B	33B	3hB	33B	33B	3hB	4�B	5?B	5B	5tB	4nB	3�B	2�B	2�B	2aB	2aB	2�B	33B	3�B	33B	33B	1�B	2�B	1�B	/�B	1�B	1'B	2aB	2�B	2aB	0�B	/OB	.IB	/�B	/B	0!B	0�B	0!B	1[B	0UB	.B	4�B	5B	7�B	<6B	FtB	Q�B	a�B	a|B	a�B	d�B	poB	y	B	�"B	��B	��B	՛B	�B
,�B
(�B
B
�B
�B
'�B
~B
,�B
#�B
3�B
B
�B
�B
!�B
B
"�B
�B
P}B
<�B
1'B
��B
��B�BB�BB�Bn/B��B��B�B�B�sB�lB�;B�oB�oB�iB�vB�B��B_BSB+B �B�B%B  B�lB�B$tBB4B(B�BxBxB
�B
	B	BBBDB�B�5B�B�2B��B�cB%B �B��B�B"B;�B>wB>�B<�Bd&B`Ba|Br�B�B}�B�B�LB�oB�\B~]By>B�+Bz�By>Bu�Bu�BzBs�Bv`Bw2B|�B�oB|�B}VB� BwfBzBy�By�Bs�Be,Bm)BW?BVBP}BM�BI�BGEBH�BA�B?B@�Bg�B:^B:^B"hBB�BB!�B"4B"�B"�B)�B/OB,qB+6B5tBd�BiDBk�BgmBe�BiDBg�Bd�BffBg�B� B�GB�kB�xB�	B�_B��B�_B�_B��B��B�GB�{B��B�oB��B�;B��B�{B��B|�Bw�BxlBw�Bu�B}�Bu�BoiBl�Bk�Bk�Bn�BlWByrBs�BY�BZQBZBPHBNpBGBHBGzBHKBM�BDgB@�BA�BA�BH�B:^B9�B7�B6FB7�B7�B6FB5�B7B5�B5�B6B6�B5?B5?B6B5B3hB33B49B2�B1�B4B1[B0�B/�B0�B.�B-wB.}B.}B,�B,B.IB.�B+B+B*eB&�B'�B&�B$@B#�B'�B$tB�B�B{BB�B4B�B�B�B�B�B�B�B�BJB
�BJB
�B	7B	7B�B%B�BB_B��B�	B��B�5B�B�|B�B�)B�B�sB�8B�&B�|B� B�jB��B��B��B��B��B�B��B�}B��B�jB��B��B�B��B�[B��B�9B�UB��B��B��B�_B��B�@B�-B��B�'B��B��B�kB��B�SB�{B��B��B��B��B�xB��B��B��B��B��B�oB|�B{�ByrBtTBsMBq�BrGBqvBt�Bl�Bh�B`�B]�Ba|B[WB^5B[�BY�BY�BZQBV�B\)BT�BOBPHBJ�BIRBF�BI�B;�B>B?�B/OB-B7�B&LB1B{B�B4B�B�B�BMBB
��B
��B
�B
�	B
��B
�KB
�
B
��B
�JB
��B
�`B
�?B
�^B
�tB
�tB
��B
�B
�!B
�B
��B
��B
��B
�B
��B
��B
�$B
��B
�$B
zB
�;B
m�B
n�B
gB
kQB
h
B
c�B
cTB
_;B
d�B
aB
s�B
[�B
Q�B
L�B
H�B
H�B
E�B
@�B
A B
?}B
=�B
=qB
:�B
7�B
4nB
5B
1�B
0UB
.}B
/OB
/B
9�B
!�B
.B
�B
"B
�B	�(B	�"B	��B	�AB	�VB	��B	�B	��B	�B	� B	��B	�;B	�B	�5B	��B	��B	��B	�yB	�yB	�B	�
B	��B	��B	�9B	ӏB	ϫB	�NB	�HB	ΥB	�sB	֡B	��B	�'B	�B	�B	��B	��B	��B	�hB	��B	��B	�0B	��B	�gB	�B	��B	��B	�YB	��B	��B	��B	�'B	�YB	��B	�.B	��B	��B	�bB	�B	�B	��B	��B	�B	��B	�B	�uB	�MB	�B	�;B	�B	�%B	~(B	��B	|�B	{�B	��B	zB	q�B	v+B	u�B	m�B	qvB	iB	c�B	h
B	iDB	h>B	b�B	h
B	b�B	^B	\]B	[�B	\�B	_�B	ZB	k�B	WsB	\�B	a�B	N�B	PB	YB	Q�B	M�B	K�B	NB	M6B	M�B	JXB	I�B	G�B	H�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                   B	*B	*9B	*mB	*mB	*9B	*�B	*mB	*9B	*mB	)�B	*B	*9B	*�B	*�B	*mB	)�B	)�B	*9B	*mB	*�B	+B	+?B	+B	*�B	*�B	*�B	+B	+B	+B	+B	+B	+tB	+?B	+B	+?B	+�B	,B	+�B	*9B	(aB	N�B	��B
�B
W�B]8B��B�BB�BhB��B	�BJ`B7Bw�Bq�Bw:Bo>BPB<�B.BnB)3BaBb�B�B~�Bz�Bu�Bn�Be�BTiB?�B5}B-�B'�B�B�B��B��B��B�B��B��Bo>BXNBGNB�B
��B
�HB
��B
a�B
D�B
+�B	�oB	֪B	�mB	�HB	��B	�4B	y�B	s"B	]8B	X�B	H�B	@�B	5�B	'�B	�B�B�1B�B��B۔B�JB�B��B��B	B	�BضB��B��BҒB�]BӘB�B��B�B	!B	B�B�NB�B��B��B	�B	B	(-B	+?B	3�B	1�B	&�B	�B	�B	qB	�B	'B�kB	bB	B	�B	�B	�B	�B	 0B	6�B	8[B	4vB	!6B	FB	bB	RB	B	B�YB��B�iB��B	qB	�B	UB	�B	B	OB	B	+�B	2�B	=B	D�B	TiB	UpB	W�B	[�B	g�B	mfB	m1B	nlB	n7B	nlB	r�B	t(B	t�B	ubB	yB	y{B	|�B	{SB	}�B	~eB	|%B	{�B	y�B	y�B	yGB	{B	~�B	�=B	�rB	�xB	�xB	�rB	�xB	|�B	~1B	�B	�LB	�nB	��B	��B	��B	��B	�B	��B	�B	��B	�B	��B	��B	�RB	�$B	�qB	�B	��B	��B	��B	�B	��B	�zB	�KB	�B	��B	��B	��B	�XB	�#B	�dB	��B	�B	��B	��B	�}B	� B	��B	�[B	��B	��B	��B	�EB	��B	��B	��B	��B	��B	��B	�dB	��B	��B	�^B	��B	�/B	�B	��B	��B	��B	��B	��B	�BB	�vB	ǂB	�B	��B	�B	�B	�NB	� B	�ZB	ɏB	�2B	�>B	̡B	�>B	�
B	̡B	��B	�8B	��B	έB	�EB	�EB	��B	��B	ҒB	�]B	�]B	�)B	��B	��B	�]B	�/B	�vB	ӘB	�B	ӘB	�cB	ԞB	ӘB	��B	ԞB	��B	ԞB	��B	�iB	�pB	�;B	�B	֪B	�|B	�|B	؂B	��B	��B	�8B	��B	�
B	�B	�B	��B	�B	�B	�>B	�B	�B	�B	�B	�B	�B	��B	�"B	�B	�WB	��B	�(B	��B	��B	�B	��B	�oB	�B	�B	��B	�B	��B	�B	�B	�7B	�B	�lB	�B	��B	�B	�B	�	B	�B	�B	�B	�B	�B	�B	�B	�B	�"B	�"B	�"B	�B	�bB	��B	�4B	��B	��B	�4B	�B	�oB	��B	�@B	�B	�uB	�B	��B	�SB	��B	�%B	�YB	�YB	�%B	�YB	��B	�YB	��B	�_B	�B	�7B	�7B	�B	��B	�7B
 	B
 	B
 	B
 	B	��B
 	B
 	B
 =B
 �B
 �B
B
xB
�B
B
IB
�B
�B
B
�B
�B
!B
VB
VB
�B
.B
bB
bB
bB
.B
hB
4B
4B
hB
B
B
�B
	�B
	�B

B

FB

{B

�B

�B

�B
RB
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
qB
qB
=B
B
wB
wB
B
B
IB
IB
�B
�B
�B
�B
�B
UB
bB
�B
�B
-B
bB
�B
�B
nB
@B
zB
�B
�B
�B
RB
�B
�B
�B
 �B
 �B
!B
 dB
 �B
!6B
!kB
!�B
!kB
!B
!kB
!kB
"<B
"�B
#B
$IB
#�B
$B
$IB
%B
$�B
& B
&�B
'�B
''B
(aB
(�B
(�B
)3B
*B
*B
)gB
)�B
*�B
+B
*mB
*9B
,B
+�B
+�B
+?B
+tB
+B
,�B
-KB
.B
-�B
.�B
.B
.RB
/�B
/XB
.�B
.�B
.�B
/�B
/XB
/�B
0�B
2jB
2�B
3B
3�B
3�B
3�B
4B
3�B
4B
3�B
3�B
3�B
4�B
5HB
4�B
4�B
5HB
5HB
6B
6�B
7TB
7TB
7�B
7�B
7�B
9aB
9aB
9�B
9aB
9,B
:3B
9�B
:�B
9�B
;B
;9B
;B
;mB
;mB
:�B
<
B
<�B
=�B
=B
=�B
>KB
=EB
=yB
<�B
=�B
=�B
>�B
>�B
?�B
?�B
@WB
A)B
@�B
@�B
@WB
@�B
A�B
A)B
B/B
B�B
B�B
DB
DB
EB
EBB
EBB
FHB
F|B
G�B
GB
F�B
GB
G�B
HTB
IZB
I�B
I�B
IZB
IZB
J`B
J,B
J`B
K2B
LB
K�B
KgB
LB
K�B
J�B
J�B
LmB
LmB
LB
L�B
K�B
LmB
L�B
L8B
LB
M�B
M�B
MsB
NyB
NyB
NyB
M�B
M�B
M�B
NEB
NyB
NyB
N�B
OB
O�B
O�B
OB
O�B
PB
Q#B
Q�B
Q#B
QWB
Q�B
Q�B
R)B
Q�B
Q�B
Q�B
QWB
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
S�B
TB
TiB
T�B
U;B
VB
VB
U�B
UpB
U;B
U�B
UpB
U�B
V�B
V�B
WGB
W|B
W|B
WGB
XB
Y�B
X�B
YB
YTB
Y�B
[�B
\�B
]8B
]B
]�B
]lB
]lB
]8B
]�B
^
B
^>B
^
B
]�B
]�B
]�B
]�B
]�B
^
B
]�B
^
B
^rB
^>B
^rB
^�B
_yB
_DB
_B
`B
`�B
`JB
`JB
`�B
aPB
aPB
aPB
aB
b"B
a�B
b"B
b�B
bWB
c(B
c(B
c�B
dcB
d�B
c�B
d�B
d�B
e B
e�B
e�B
e�B
f;B
f�B
gB
guB
g�B
h�B
hGB
hGB
h�B
iB
jB
i�B
j�B
j�B
kYB
j�B
j�B
kYB
k�B
l�B
l`B
m1B
m1B
l�B
m�B
m1B
nB
nB
n7B
nlB
nlB
o>B
o	B
o	B
o�B
pB
o�B
qB
qB
p�B
p�B
qJB
p�B
qB
p�B
qB
p�B
p�B
p�B
q�B
qJB
qJB
q�B
q�B
rPB
rB
q~B
q�B
r�B
rPB
rB
s"B
s"B
r�B
r�B
sVB
sVB
r�B
r�B
sVB
s�B
t(B
t�B
t\B
s�B
t�B
u.B
t�B
u�B
u�B
ubB
ubB
v4B
v B
viB
v�B
wB
wB
wB
w:B
woB
w�B
x@B
x@B
xB
xB
x@B
xB
xB
xB
x@B
x@B
xB
x@B
xB
x�B
x@B
x�B
y{B
x�B
yGB
y�B
y{B
zMB
zMB
zMB
z�B
z�B
y�B
z�B
{SB
z�B
{�B
{SB
{�B
|YB
|�B
{�B
{�B
|YB
|�B
}+B
}_B
~1B
~�B
~eB
~1B
~�B
~�B
}�B
~�B
B
~�B
~eB
~�B
�B
B
7B
7B
7B
kB
kB
�B
�	B
�B
�=B
��B
�rB
��B
�rB
��B
�xB
�B
�B
��B
��B
��B
�B
�B
�CB
��B
��B
�B
��B
��B
�xB
��B
�IB
�IB
�B
�B
�B
��B
��B
�~B
��B
�~B
�IB
��B
��B
��B
��B
�B
��B
��B
�VB
�VB
�VB
��B
��B
�!B
��B
��B
�\B
�\B
��B
��B
��B
��B
�.B
�.B
��B
�4B
��B
�hB
�hB
��B
�:B
��B
��B
��B
�B
�B
��B
�@B
�@B
�tB	#�B	*mB	&UB	(�B	0)B	+?B	*�B	)�B	)�B	)�B	*B	+B	+B	+tB	+?B	+�B	*�B	*B	)�B	)gB	*9B	*mB	*mB	*�B	*�B	*�B	*�B	)3B	)�B	)gB	)�B	*�B	*�B	+�B	+�B	*mB	*9B	)�B	*B	*B	*B	*�B	+?B	*�B	*9B	)3B	(�B	(aB	(�B	(�B	)�B	*B	*B	+�B	)�B	*B	)�B	)gB	)�B	*�B	+?B	+tB	+tB	*mB	*B	*9B	*�B	+B	+�B	,B	+�B	,B	+�B	*mB	*mB	*mB	*�B	*�B	+B	,B	,EB	,B	+�B	+�B	+tB	+tB	+�B	*�B	)�B	(-B	+tB	*B	+B	*�B	*�B	+�B	+�B	*9B	*9B	)gB	*mB	*B	*mB	+B	+�B	+�B	+tB	,B	+�B	+tB	+?B	*�B	*9B	)�B	)�B	*B	*9B	*mB	*mB	+?B	+tB	,B	,B	,EB	+�B	+�B	+�B	+?B	*�B	*mB	*9B	*B	*�B	+�B	,B	+�B	+?B	*B	)�B	)�B	*mB	*�B	+?B	*�B	+?B	+�B	+�B	+�B	+tB	+?B	+�B	+B	*�B	*mB	*�B	*�B	+�B	,EB	,EB	,zB	,B	+�B	+�B	*9B	*mB	*�B	+?B	+?B	+B	+�B	+tB	+tB	*�B	+B	*9B	)�B	*mB	*mB	*mB	+�B	,B	,B	,zB	+B	*mB	*mB	*�B	+?B	+?B	+�B	,zB	,�B	,zB	+�B	*�B	+?B	+tB	+?B	+?B	+tB	,�B	-KB	-B	-�B	,zB	+�B	+B	*�B	*mB	*mB	+B	+?B	+�B	+?B	+?B	)�B	*�B	*B	'�B	)�B	)3B	*mB	*�B	*mB	(�B	'[B	&UB	'�B	''B	(-B	(�B	(-B	)gB	(aB	& B	,�B	-B	/�B	4BB	>�B	I�B	Y�B	Y�B	Y�B	\�B	h{B	qB	�.B	��B	��B	ͧB	�B
$�B
!B
B
�B
�B
�B
�B
$�B
�B
+�B
B
�B
�B
�B
$B
�B
�B
H�B
4�B
)3B
|�B
��B�BB�B:�Bf;B��B��B� BݡB�B�xB�GB�{B�{B�uB�B�B�B�kB�_B�7B��B�BB�B�xB�B�B
B	@B4B�B�B�B�BBB!BBPB�B�AB�B�>B��B�oB�1B��B�B��B.B3�B6�B6�B4�B\2BXBY�Bj�B|%Bv B}+B�XBy{B�hBviBqJB7Br�BqJBnBm�BrBk�BnlBo>Bt�By{Bt�BubBxBorBrBq�Bq�Bk�B]8Be5BOKBNBH�BE�BA�B?QB@�B9�B7 B8�B_�B2jB2jBtB'B�B'BB@B�B�B!�B'[B$}B#BB-�B]BaPBc�B_yB]�BaPB_�B\�B^rB_�BxB{SB�wB��B�BkB�BkBkBB|�B{SB{�B{�By{By�ByGBz�B{�Bx�Bt�Bo�BpxBo�BnBv BnBguBd�Bc�Bc�Bf�BdcBq~Bk�BQ�BR]BR)BHTBF|B?B@#B?�B@WBE�B<sB8�B9�B9�B@�B2jB1�B/�B.RB/�B/�B.RB-�B/#B-�B-�B.B.�B-KB-KB.B-B+tB+?B,EB*�B)�B,B)gB(�B'�B(�B&�B%�B&�B&�B$�B$B&UB&�B#B#B"qB�B�B�BLB�B�B�B�B�B�BB	�B	@B	�B�B�BB�B�B�B�BVB�BVB�BCBCB��B�1B�B$B�kB��B�B��B�AB�B�B��B�5B߭B�B�DB�2BوB�,B�vB�B��B��B��B��B�B��BȉB�B�vB��B��B�#B��B�gB��B�EB�aB��B��B��B�kB��B�LB�9B��B�3B��B��B�wB��B�_B��B��B��B��B��B��B}�B~�BB{�B|�By{Bt�Bs�Bq~Bl`BkYBi�BjSBi�Bl�Be B`�BX�BU�BY�BScBVABS�BQ�BQ�BR]BN�BT5BL�BGBHTBCBA^B>�BA�B3�B6B7�B'[B%B/�BXB=B�B�B	@B�B�B
��B
�YB
�B
�B
��B
�B
�B
� B
�WB
�B
��B
�VB
��B
�lB
�KB
�jB
��B
��B
��B
�'B
�-B
�B
��B
��B
��B
�*B
��B
��B
�0B
��B
�0B
rB
yGB
fB
f�B
_B
c]B
`B
[�B
[`B
WGB
]B
YB
k�B
S�B
I�B
D�B
@�B
@�B
=�B
8�B
9,B
7�B
5�B
5}B
2�B
/�B
,zB
-B
)�B
(aB
&�B
'[B
''B
1�B
B
:B
�B
.B	��B	�4B	�.B	�B	�MB	�bB	��B	�%B	��B	�B	�,B	��B	�GB	�B	�AB	�B	��B	��B	ЅB	ЅB	�B	�B	��B	��B	�EB	˛B	ǷB	�ZB	�TB	ƱB	�B	έB	��B	�3B	�B	�B	��B	��B	��B	�tB	��B	��B	�<B	��B	�sB	�B	��B	��B	�eB	��B	��B	�B	�3B	�eB	��B	�:B	��B	��B	�nB	}+B	}+B	{�B	|�B	zB	{�B	{B	z�B	|YB	{B	yGB	w�B	~1B	v4B	x�B	t�B	s�B	��B	rB	i�B	n7B	nB	e�B	i�B	aB	[�B	`B	aPB	`JB	Z�B	`B	Z�B	VB	TiB	S�B	T�B	W�B	R)B	c�B	OB	T�B	Y�B	F�B	H B	Q#B	I�B	E�B	DB	FB	EBB	E�B	BdB	A�B	?�B	@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0077649                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0077649                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 23 11 2022 179 -0.0077649 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 23 11 2022 179 -0.0077649 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230426223225                            20230426223225AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023042622322520230426223225  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622322520230426223225QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F83E           783E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622322520230426223225QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               