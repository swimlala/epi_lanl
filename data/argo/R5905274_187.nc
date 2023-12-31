CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-04-26T22:32:30Z creation      
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
_FillValue                 �  [p   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  cT   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �X   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 8`   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � @D   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � _�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � g�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` �H   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20230426223230  20230426223230  5905274 5905274 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO7315                            7315                            2B  2B  AA  SOLO_II                         SOLO_II                         8643                            8643                            SBE602 11Jan18                  SBE602 11Jan18                  853 853 @�[�^$@�[�^$11  @��k�@��k�@/��Q�@/��Q��dZ���l��dZ���l�11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  ?��?�@=p�@}p�@�G�@�G�@޸R@�p�A\)A   A,(�A@��AaG�A\)A�  A�Q�A�Q�A�Q�A�  A�  A�  B   B�
B�
B  B�
B'�B/�B7�
B?�
BH(�BP(�BXQ�B`Q�Bh(�Bp(�Bx  B�
B�  B�(�B�{B�  B��B��
B�  B�(�B�  B�  B�  B��B�{B��B��B��B�B��B�(�B�(�B�  B�  B�{B��B�{B�(�B�(�B�  B�  B�(�B�=qC 
=C  C  C  C  C	��C��C  C
=C
=C��C  C{C
=C
=C  C   C"  C$
=C&  C'��C*  C,{C.�C0
=C2  C3��C5�HC7��C:  C<  C>  C?��CB
=CD
=CF
=CH{CJ{CL{CN{CP
=CR  CT  CV  CX  CY��C\  C^
=C`{Cb  Cd  Cf
=Ch
=Cj
=Cl
=Cm�Cp  Cr  Cs�Cv  Cx{Cz  C{��C~
=C�  C���C�  C�  C���C���C��C�  C�C���C�  C���C�  C�
=C�
=C�  C�  C�
=C�C���C���C�C�  C�  C�  C�  C���C���C�C�
=C�
=C�C�C�
=C�C���C���C���C��C���C�  C�C�
=C�C�C�C���C���C���C���C���C�  C�C���C���C���C�  C�C�  C�C�  C�C�C�C�  C���C���C�  C�C�C�  C���C�  C�  C�C�C���C���C�  C�  C�  C�C�  C���C���C���C���C���C���C���C�C�\C�C�  C�C�C�  C�
=C�C���C�  C�C�  C�  C���C���C���C���C��C���C���C���C�  C�  C���C���C���C���C�  C���C���C���C��C��C��C��C���C�  C���D � D�D}qD�RDz�D  D�D�D�D  Dz�D��D}qD  D�D�D� D	�D	��D
  D
��DD�D�qDz�D  D�D�Dz�D  D�D  D��D�D}qD�qD}qD  D��D  D��D�qD��D  D�D�qD� D�D� D�D��D  D�D  D��D�qD��D�D��D
=D� D  Dz�D D ��D �qD!�D"�D"}qD#D#}qD$�D$}qD%  D%�=D&  D&� D'�D'��D(�D(�=D)  D)�D)�qD*� D*��D+z�D,D,z�D,�qD-�D.  D.xRD.�qD/�D0D0��D1�D1� D2  D2� D3�D3}qD3��D4� D5  D5z�D6  D6��D7  D7�D8�D8�D9  D9�D:D:� D;D;� D;�qD<�D=D=}qD=��D>}qD?  D?��D@  D@}qDA�DA� DA�qDBxRDB�RDC}qDC�qDD� DEDE��DF�DF��DG�DG��DH�DH� DI  DI��DJ  DJ}qDJ�qDK}qDL  DL}qDL�qDM� DM��DNxRDN��DO}qDP  DP��DQ  DQ� DR  DR� DS  DS}qDT�DT}qDT�qDU}qDU�qDV� DW  DW��DX  DX�DY  DYz�DZ  DZ� D[  D[��D\  D\}qD]  D]��D^�D^� D_  D_}qD_�qD`� D`�qDa� Db  Db��Dc�Dc� Dd�Dd}qDe  De� Df  Df� Dg  Dg��Dh�Dh� Dh�qDi}qDj  Dj� Dk  Dk��DlDl�Dl�qDm}qDm�qDn}qDn�qDo}qDp  Dp}qDp�qDq}qDq�qDr}qDs  Ds� Dt�Dt� Du�Du� Dv�Dv� Dw  Dw}qDx  Dx� Dx�qDy� Dy��Dz� D{�D{}qD{��D|� D}  D}z�D~  D~��D  Dz�D�  D�AHD��HD�� D�  D�AHD��HD�� D�HD�@ D�~�D��HD�HD�AHD�� D��)D���D�>�D�}qD�D�HD�B�D��HD���D��D�AHD��HD��qD���D�=qD�}qD���D�  D�>�D�~�D��HD��D�>�D��HD��HD�HD�@ D�}qD�D�HD�>�D��HD���D�HD�AHD��HD���D��qD�@ D�� D�� D�HD�@ D�~�D��HD�HD�>�D�~�D�� D��D�B�D�� D���D�  D�@ D��HD�� D�  D�AHD�� D��HD��qD�AHD��HD�D��qD�>�D�~�D���D���D�>�D�~�D�� D�HD�=qD�� D�� D�  D�>�D�� D�D�  D�AHD�� D��qD�  D�@ D�~�D��HD�  D�@ D�� D��HD�HD�>�D��HD�� D���D�@ D��HD��HD���D�>�D�~�D�� D�  D�@ D�� D�� D�  D�AHD�� D�� D�  D�>�D�� D��HD��qD�=qD�}qD��qD��qD�=qD��HD�D��D�B�D�� D���D�HD�@ D�~�D��HD��D�AHD�~�D�� D���D�>�D�~�D�� D�  D�C�D�~�D���D���D�AHD�� D�D��D�B�D�}qD�� D�  D�@ D�~�D���D��D�>�D�}qD�� D�HD�>�D�� D��HD�HD�@ D���D��HD�  D�>�D��HD��HD���D�@ D�}qD��qD��D�AHD���D�D���D�@ D��HD�� D�  D�>�D�~�D��qD���D�@ D���D���D���D�AHD���D��qD�HD�@ D��HD��HD��qD�>�D�}qD��qD��D�@ D��HD��)D���D�>�D�� D��HD�  D�C�D��HD��HD�  D�AHD�� D�� D�  D�C�D�� D�D��D�B�D���D��HD�HD�>�D�~�D�� D���D�=qD�� D�D�  D�>�D�� D�� D�HD�AHD�� D�� D�HD�@ D�� D�D�  D�AHD D�� D�  D�>�DÂ�D�� D�HD�@ DĀ DĽqD�  D�AHDŀ D��HD�  D�>�DƁHD�D�HD�@ Dǂ�D�D��D�C�DȀ DȾ�D�HD�AHDɀ D��HD�HD�@ Dʀ D�� D�  D�>�D�}qD�� D�  D�@ D�~�D��HD�HD�@ D̀ D;�D�  D�>�D�~�Dξ�D�  D�=qD�~�D��HD���D�=qD�~�Dо�D�HD�C�DсHD��HD�  D�>�D�}qDҾ�D�HD�@ D�}qDӾ�D�  D�B�DԁHDԾ�D���D�=qD�}qD�� D�HD�AHD�}qD�� D��D�AHDׁHD�� D�  D�@ D�~�DؽqD�  D�AHDفHD��HD���D�<)D�|)Dھ�D�HD�@ Dۀ D�� D���D�>�D�~�D�� D�  D�=qD�~�D��HD��D�@ Dހ D޾�D���D�@ D߀ D߾�D�HD�AHD�� D��HD�HD�AHD� D�� D�  D�@ D�~�D⾸D���D�>�D�~�D㾸D���D�>�D� D��HD���D�>�D� D�� D���D�@ D�~�D�� D�  D�@ D�~�D羸D�  D�AHD�HD辸D��qD�=qD�}qD龸D��qD�@ D� D�� D�  D�>�D� D��HD��D�AHD�~�D쾸D�  D�@ D� D�� D�HD�@ D�}qD�� D�HD�AHD�HD��HD�HD�@ D�~�D�D��D�>�D�}qD�� D�HD�B�D��D�D��D�@ D�}qD�qD�  D�AHD�~�D��)D���D�@ D�� D�� D�  D�@ D���D���D��D�C�D���D�D���D�>�D�~�D�� D�  D�@ D��HD�� D�  D�>�D�}qD��HD��\>�G�?.{?k�?��?��R?\?�
=?��@\)@&ff@333@@  @W
=@fff@n{@}p�@��@���@��H@��@���@�@�  @��
@�{@�@޸R@���@�33@�(�A33A
=A	��A{A�\AQ�Ap�A"�\A'
=A*�HA-p�A1�A7
=A:=qA@  AEAJ=qAO\)AQ�AW
=A[�A^{Ac�
Ag�Al(�An�RAs�
Aw
=A|(�A���A��HA�z�A�ffA���A��HA�p�A�  A��A�(�A��A�\)A���A�(�A��RA���A�=qA�(�A�ffA���A��A�A�\)A���A��
A��RA���A�(�A�A��A��A�p�A�  Aʏ\A�z�A�ffAУ�A��HA�Aأ�A�33A��A޸RA���A�A�ffA�G�A�A�{A�Q�A�=qA�z�A��RA��A���A��B�B{B\)Bz�B�B33B��B	�B
�HB  B��B{B\)Bz�B{B\)B��BB�\B�
B��BffB�
BG�B�B33B (�B!p�B#
=B$(�B%G�B%�B'33B((�B)B*�HB,(�B,��B.{B/33B0Q�B1�B333B4Q�B5p�B6=qB7\)B8z�B9B;33B<Q�B=��B>�RB?�B@��BABC33BDz�BEBF�\BG�BH��BJ�\BK�BL��BM��BN�RBP  BQ�BR�\BS�
BU�BVffBW\)BXz�BYBZ�HB\  B]p�B^�RB`Q�Bap�Bb�\Bc�Bd��BeBg�Bh��Bi�Bj�RBk�
Bl��Bn{Bo\)Bpz�Br{Bs\)Bt��Bup�BvffBw\)Bx��Bz=qB{�B|Q�B}G�B~�\B�  B���B�33B���B�(�B���B�p�B�  B��\B���B�p�B�  B���B�G�B��B�=qB��RB�G�B��
B��\B��B���B�  B�z�B��B�B�Q�B���B��B���B�=qB��HB�\)B��
B�(�B��RB��B��B�ffB���B�33B�B�=qB�
=B���B�{B�ffB���B�p�B�  B���B�G�B��
B�{B��RB�33B��B��\B���B�G�B��B�z�B�
=B��B��
B�ffB�
=B��B�B�Q�B���B�p�B�  B�(�B��\B�33B��B�=qB�ffB��RB�G�B��
B�=qB�z�B��HB�\)B��B�ffB��RB�
=B��B�{B�z�B��HB���B�(�B�z�B��B�B�ffB���B�\)B�(�B���B�33B��
B���B�
=B��B�z�B��B���B�(�B�
=B��B�{B���B��B�  B���B���B�(�B��RB�G�B�(�B\B��B�{Bģ�B�33B�{BƸRB��B�Bȣ�B�33B�Bʣ�B�\)B��
B̸RB�p�B��BΣ�BυB�  B��HBѮB�(�B���B�B�=qB��HB��B�ffB�G�B�(�Bأ�BمB�Q�B���B�B�z�B���B��B�z�B��B�{B��B�\)B�Q�B���B�B�\B�
=B��B���B�\)B�=qB�
=B�B�z�B�
=B�B��B��B��B���B�\)B�(�B�
=B�B�=qB�33B�B���B�\)B��B���B�G�B�{B��HB�33B�  B�z�B��RB��B�B�=qB���B���B�p�B��
B��B�Q�B��\B��RB�33B�\)B��C 
=C 
=C 33C p�C p�C �C �HC �HC{C(�C\)Cz�C�CC�HC�C�C�CG�C�C�\CC�HC�HC�C�CG�C�C�C�C�HC�C�C(�C=qCz�Cz�C�C�HC�
C{C{CQ�C\)Cz�C�C�RC�C
=C{CQ�CQ�Cz�C��C�C�C�C(�C33CQ�C�\C�\CC��C�C(�C(�CffCp�C�\C��C�
C	{C	�C	\)C	ffC	��C	C	�HC
�C
(�C
ffC
��C
�C
��C
=C=qCp�C�C��C�
C�CQ�C\)C�C�RC
=C{C\)Cp�C�RC�
C  CG�CQ�C��C�C��C{CG�C�\C��C�C  C=qCz�C�\C�HC��CG�CffC�\C�
C��C=qC\)C�C�
C�C33C\)Cz�C��C�HC33CG�C��C�RC  C�CffC�CC  C(�Cp�C�\C�HC  CG�CffC�RC�
C{CG�Cp�C��C�
C33CQ�C��C�RC
=C(�Cz�C��C�HC
=CG�Cp�C�RC�HC�CffC�C�
C�CG�CQ�C�C��C{C33Cz�C�C�
C(�CG�C��C�C   C {C ffC �\C �RC!  C!�C!p�C!�C!�
C!��C"Q�C"\)C"�RC"��C#
=C#\)C#p�C#C#�HC$33C$G�C$�C$��C$�C%33C%Q�C%��C%C&
=C&(�C&ffC&��C&C'{C'�C'z�C'��C'C({C((�C(z�C(�\C(��C)
=C)(�C)p�C)�\C)�
C)�HC*33C*\)C*�\C*��C*�C+33C+Q�C+��C+�RC,  C,�C,p�C,z�C,C-  C-�C-p�C-�C-��C-�HC.33C.ffC.z�C.��C.�HC/=qC/\)C/�C/�
C/��C0=qC0Q�C0�C0�
C1{C1Q�C1p�C1C1�C2=qC2Q�C2��C2�C3
=C3\)C3z�C3��C4  C4(�C4z�C4�\C4�C5
=C5Q�C5�\C5�C6
=C6�C6ffC6�C6��C7�C7=qC7��C7�C7�HC833C8\)C8�RC8C9
=C9\)C9z�C9��C:  C:(�C:z�C:��C:��C;(�C;Q�C;��C;C<{C<=qC<ffC<C<�HC=33C=ffC=�C=�HC=��C>G�C>�\C>�C?  C?�C?p�C?�RC?�
C@33C@Q�C@��C@�HCA  CA\)CAz�CACB{CB=qCB�\CB�CB��CCG�CCp�CCCC�CDG�CDz�CD��CE  CE(�CE�CE�CE�CF=qCFp�CF��CF�CG=qCG�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411411411111111141111141411411111114114141111111114111141111411114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                    ?��?�@=p�@}p�@�G�@�G�@޸R@�p�A\)A   A,(�A@��AaG�A\)A�  A�Q�A�Q�A�Q�A�  A�  A�  B   B�
B�
B  B�
B'�B/�B7�
B?�
BH(�BP(�BXQ�B`Q�Bh(�Bp(�Bx  B�
B�  B�(�B�{B�  B��B��
B�  B�(�B�  B�  B�  B��B�{B��B��B��B�B��B�(�B�(�B�  B�  B�{B��B�{B�(�B�(�B�  B�  B�(�B�=qC 
=C  C  C  C  C	��C��C  C
=C
=C��C  C{C
=C
=C  C   C"  C$
=C&  C'��C*  C,{C.�C0
=C2  C3��C5�HC7��C:  C<  C>  C?��CB
=CD
=CF
=CH{CJ{CL{CN{CP
=CR  CT  CV  CX  CY��C\  C^
=C`{Cb  Cd  Cf
=Ch
=Cj
=Cl
=Cm�Cp  Cr  Cs�Cv  Cx{Cz  C{��C~
=C�  C���C�  C�  C���C���C��C�  C�C���C�  C���C�  C�
=C�
=C�  C�  C�
=C�C���C���C�C�  C�  C�  C�  C���C���C�C�
=C�
=C�C�C�
=C�C���C���C���C��C���C�  C�C�
=C�C�C�C���C���C���C���C���C�  C�C���C���C���C�  C�C�  C�C�  C�C�C�C�  C���C���C�  C�C�C�  C���C�  C�  C�C�C���C���C�  C�  C�  C�C�  C���C���C���C���C���C���C���C�C�\C�C�  C�C�C�  C�
=C�C���C�  C�C�  C�  C���C���C���C���C��C���C���C���C�  C�  C���C���C���C���C�  C���C���C���C��C��C��C��C���C�  C���D � D�D}qD�RDz�D  D�D�D�D  Dz�D��D}qD  D�D�D� D	�D	��D
  D
��DD�D�qDz�D  D�D�Dz�D  D�D  D��D�D}qD�qD}qD  D��D  D��D�qD��D  D�D�qD� D�D� D�D��D  D�D  D��D�qD��D�D��D
=D� D  Dz�D D ��D �qD!�D"�D"}qD#D#}qD$�D$}qD%  D%�=D&  D&� D'�D'��D(�D(�=D)  D)�D)�qD*� D*��D+z�D,D,z�D,�qD-�D.  D.xRD.�qD/�D0D0��D1�D1� D2  D2� D3�D3}qD3��D4� D5  D5z�D6  D6��D7  D7�D8�D8�D9  D9�D:D:� D;D;� D;�qD<�D=D=}qD=��D>}qD?  D?��D@  D@}qDA�DA� DA�qDBxRDB�RDC}qDC�qDD� DEDE��DF�DF��DG�DG��DH�DH� DI  DI��DJ  DJ}qDJ�qDK}qDL  DL}qDL�qDM� DM��DNxRDN��DO}qDP  DP��DQ  DQ� DR  DR� DS  DS}qDT�DT}qDT�qDU}qDU�qDV� DW  DW��DX  DX�DY  DYz�DZ  DZ� D[  D[��D\  D\}qD]  D]��D^�D^� D_  D_}qD_�qD`� D`�qDa� Db  Db��Dc�Dc� Dd�Dd}qDe  De� Df  Df� Dg  Dg��Dh�Dh� Dh�qDi}qDj  Dj� Dk  Dk��DlDl�Dl�qDm}qDm�qDn}qDn�qDo}qDp  Dp}qDp�qDq}qDq�qDr}qDs  Ds� Dt�Dt� Du�Du� Dv�Dv� Dw  Dw}qDx  Dx� Dx�qDy� Dy��Dz� D{�D{}qD{��D|� D}  D}z�D~  D~��D  Dz�D�  D�AHD��HD�� D�  D�AHD��HD�� D�HD�@ D�~�D��HD�HD�AHD�� D��)D���D�>�D�}qD�D�HD�B�D��HD���D��D�AHD��HD��qD���D�=qD�}qD���D�  D�>�D�~�D��HD��D�>�D��HD��HD�HD�@ D�}qD�D�HD�>�D��HD���D�HD�AHD��HD���D��qD�@ D�� D�� D�HD�@ D�~�D��HD�HD�>�D�~�D�� D��D�B�D�� D���D�  D�@ D��HD�� D�  D�AHD�� D��HD��qD�AHD��HD�D��qD�>�D�~�D���D���D�>�D�~�D�� D�HD�=qD�� D�� D�  D�>�D�� D�D�  D�AHD�� D��qD�  D�@ D�~�D��HD�  D�@ D�� D��HD�HD�>�D��HD�� D���D�@ D��HD��HD���D�>�D�~�D�� D�  D�@ D�� D�� D�  D�AHD�� D�� D�  D�>�D�� D��HD��qD�=qD�}qD��qD��qD�=qD��HD�D��D�B�D�� D���D�HD�@ D�~�D��HD��D�AHD�~�D�� D���D�>�D�~�D�� D�  D�C�D�~�D���D���D�AHD�� D�D��D�B�D�}qD�� D�  D�@ D�~�D���D��D�>�D�}qD�� D�HD�>�D�� D��HD�HD�@ D���D��HD�  D�>�D��HD��HD���D�@ D�}qD��qD��D�AHD���D�D���D�@ D��HD�� D�  D�>�D�~�D��qD���D�@ D���D���D���D�AHD���D��qD�HD�@ D��HD��HD��qD�>�D�}qD��qD��D�@ D��HD��)D���D�>�D�� D��HD�  D�C�D��HD��HD�  D�AHD�� D�� D�  D�C�D�� D�D��D�B�D���D��HD�HD�>�D�~�D�� D���D�=qD�� D�D�  D�>�D�� D�� D�HD�AHD�� D�� D�HD�@ D�� D�D�  D�AHD D�� D�  D�>�DÂ�D�� D�HD�@ DĀ DĽqD�  D�AHDŀ D��HD�  D�>�DƁHD�D�HD�@ Dǂ�D�D��D�C�DȀ DȾ�D�HD�AHDɀ D��HD�HD�@ Dʀ D�� D�  D�>�D�}qD�� D�  D�@ D�~�D��HD�HD�@ D̀ D;�D�  D�>�D�~�Dξ�D�  D�=qD�~�D��HD���D�=qD�~�Dо�D�HD�C�DсHD��HD�  D�>�D�}qDҾ�D�HD�@ D�}qDӾ�D�  D�B�DԁHDԾ�D���D�=qD�}qD�� D�HD�AHD�}qD�� D��D�AHDׁHD�� D�  D�@ D�~�DؽqD�  D�AHDفHD��HD���D�<)D�|)Dھ�D�HD�@ Dۀ D�� D���D�>�D�~�D�� D�  D�=qD�~�D��HD��D�@ Dހ D޾�D���D�@ D߀ D߾�D�HD�AHD�� D��HD�HD�AHD� D�� D�  D�@ D�~�D⾸D���D�>�D�~�D㾸D���D�>�D� D��HD���D�>�D� D�� D���D�@ D�~�D�� D�  D�@ D�~�D羸D�  D�AHD�HD辸D��qD�=qD�}qD龸D��qD�@ D� D�� D�  D�>�D� D��HD��D�AHD�~�D쾸D�  D�@ D� D�� D�HD�@ D�}qD�� D�HD�AHD�HD��HD�HD�@ D�~�D�D��D�>�D�}qD�� D�HD�B�D��D�D��D�@ D�}qD�qD�  D�AHD�~�D��)D���D�@ D�� D�� D�  D�@ D���D���D��D�C�D���D�D���D�>�D�~�D�� D�  D�@ D��HD�� D�  D�>�D�}qD��HD��\>�G�?.{?k�?��?��R?\?�
=?��@\)@&ff@333@@  @W
=@fff@n{@}p�@��@���@��H@��@���@�@�  @��
@�{@�@޸R@���@�33@�(�A33A
=A	��A{A�\AQ�Ap�A"�\A'
=A*�HA-p�A1�A7
=A:=qA@  AEAJ=qAO\)AQ�AW
=A[�A^{Ac�
Ag�Al(�An�RAs�
Aw
=A|(�A���A��HA�z�A�ffA���A��HA�p�A�  A��A�(�A��A�\)A���A�(�A��RA���A�=qA�(�A�ffA���A��A�A�\)A���A��
A��RA���A�(�A�A��A��A�p�A�  Aʏ\A�z�A�ffAУ�A��HA�Aأ�A�33A��A޸RA���A�A�ffA�G�A�A�{A�Q�A�=qA�z�A��RA��A���A��B�B{B\)Bz�B�B33B��B	�B
�HB  B��B{B\)Bz�B{B\)B��BB�\B�
B��BffB�
BG�B�B33B (�B!p�B#
=B$(�B%G�B%�B'33B((�B)B*�HB,(�B,��B.{B/33B0Q�B1�B333B4Q�B5p�B6=qB7\)B8z�B9B;33B<Q�B=��B>�RB?�B@��BABC33BDz�BEBF�\BG�BH��BJ�\BK�BL��BM��BN�RBP  BQ�BR�\BS�
BU�BVffBW\)BXz�BYBZ�HB\  B]p�B^�RB`Q�Bap�Bb�\Bc�Bd��BeBg�Bh��Bi�Bj�RBk�
Bl��Bn{Bo\)Bpz�Br{Bs\)Bt��Bup�BvffBw\)Bx��Bz=qB{�B|Q�B}G�B~�\B�  B���B�33B���B�(�B���B�p�B�  B��\B���B�p�B�  B���B�G�B��B�=qB��RB�G�B��
B��\B��B���B�  B�z�B��B�B�Q�B���B��B���B�=qB��HB�\)B��
B�(�B��RB��B��B�ffB���B�33B�B�=qB�
=B���B�{B�ffB���B�p�B�  B���B�G�B��
B�{B��RB�33B��B��\B���B�G�B��B�z�B�
=B��B��
B�ffB�
=B��B�B�Q�B���B�p�B�  B�(�B��\B�33B��B�=qB�ffB��RB�G�B��
B�=qB�z�B��HB�\)B��B�ffB��RB�
=B��B�{B�z�B��HB���B�(�B�z�B��B�B�ffB���B�\)B�(�B���B�33B��
B���B�
=B��B�z�B��B���B�(�B�
=B��B�{B���B��B�  B���B���B�(�B��RB�G�B�(�B\B��B�{Bģ�B�33B�{BƸRB��B�Bȣ�B�33B�Bʣ�B�\)B��
B̸RB�p�B��BΣ�BυB�  B��HBѮB�(�B���B�B�=qB��HB��B�ffB�G�B�(�Bأ�BمB�Q�B���B�B�z�B���B��B�z�B��B�{B��B�\)B�Q�B���B�B�\B�
=B��B���B�\)B�=qB�
=B�B�z�B�
=B�B��B��B��B���B�\)B�(�B�
=B�B�=qB�33B�B���B�\)B��B���B�G�B�{B��HB�33B�  B�z�B��RB��B�B�=qB���B���B�p�B��
B��B�Q�B��\B��RB�33B�\)B��C 
=C 
=C 33C p�C p�C �C �HC �HC{C(�C\)Cz�C�CC�HC�C�C�CG�C�C�\CC�HC�HC�C�CG�C�C�C�C�HC�C�C(�C=qCz�Cz�C�C�HC�
C{C{CQ�C\)Cz�C�C�RC�C
=C{CQ�CQ�Cz�C��C�C�C�C(�C33CQ�C�\C�\CC��C�C(�C(�CffCp�C�\C��C�
C	{C	�C	\)C	ffC	��C	C	�HC
�C
(�C
ffC
��C
�C
��C
=C=qCp�C�C��C�
C�CQ�C\)C�C�RC
=C{C\)Cp�C�RC�
C  CG�CQ�C��C�C��C{CG�C�\C��C�C  C=qCz�C�\C�HC��CG�CffC�\C�
C��C=qC\)C�C�
C�C33C\)Cz�C��C�HC33CG�C��C�RC  C�CffC�CC  C(�Cp�C�\C�HC  CG�CffC�RC�
C{CG�Cp�C��C�
C33CQ�C��C�RC
=C(�Cz�C��C�HC
=CG�Cp�C�RC�HC�CffC�C�
C�CG�CQ�C�C��C{C33Cz�C�C�
C(�CG�C��C�C   C {C ffC �\C �RC!  C!�C!p�C!�C!�
C!��C"Q�C"\)C"�RC"��C#
=C#\)C#p�C#C#�HC$33C$G�C$�C$��C$�C%33C%Q�C%��C%C&
=C&(�C&ffC&��C&C'{C'�C'z�C'��C'C({C((�C(z�C(�\C(��C)
=C)(�C)p�C)�\C)�
C)�HC*33C*\)C*�\C*��C*�C+33C+Q�C+��C+�RC,  C,�C,p�C,z�C,C-  C-�C-p�C-�C-��C-�HC.33C.ffC.z�C.��C.�HC/=qC/\)C/�C/�
C/��C0=qC0Q�C0�C0�
C1{C1Q�C1p�C1C1�C2=qC2Q�C2��C2�C3
=C3\)C3z�C3��C4  C4(�C4z�C4�\C4�C5
=C5Q�C5�\C5�C6
=C6�C6ffC6�C6��C7�C7=qC7��C7�C7�HC833C8\)C8�RC8C9
=C9\)C9z�C9��C:  C:(�C:z�C:��C:��C;(�C;Q�C;��C;C<{C<=qC<ffC<C<�HC=33C=ffC=�C=�HC=��C>G�C>�\C>�C?  C?�C?p�C?�RC?�
C@33C@Q�C@��C@�HCA  CA\)CAz�CACB{CB=qCB�\CB�CB��CCG�CCp�CCCC�CDG�CDz�CD��CE  CE(�CE�CE�CE�CF=qCFp�CF��CF�CG=qCG�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411411411111111141111141411411111114114141111111114111141111411114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                    G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�C�A�G�A�Q�A�Q�A�Q�A�Q�A�S�A�S�A�S�A�VA�VA�XA�S�A�VA�VA�VA�S�A�VA�XA�XA�ZA�\)A�\)A�\)A�^5A�bNA�bNA�bNA�^5A�`BA�`BA�bNA�bNA�hsA�ffA�hsA�l�A�n�A�jA�hsA�hsA�hsA�hsA�jA�hsA�bNA�\)A�;dA�oA�dZA��A��A���A�VA�bA�ȴA���A�ĜA��\A���A���A�"�A�x�A��`A��A�5?A�VA��A���A��A�A��mA���A��A�$�A��!A��A�~�A���A��
A��wA��A}VAzM�Aw/As�Ao��Ai�PAf�HAc?}Aa%AZ�AVv�AN��AH�\AG\)AF�AE�ADI�AC�AB�jA<I�A8bNA6E�A5�-A4$�A3oA2��A2M�A1��A1�;A2�uA2I�A.v�A(��A'VA%�hA$��A#A!��A!|�A �A�jAM�A9XA �A  AƨAG�A9XAl�A�A��A��A~�AA�Ax�A�RA�A�-A33A��A$�A�A��A�Ax�A`BA��A�A��A&�A�!AM�A��A%AXAn�AbAS�AQ�A�A�\AA
�A	�#A�uA��A\)A�yA1'A��A��A��A?}A"�A"�A/A�A�yAdZ@���@�x�@���@�K�@��@��@��m@��/@�"�@��u@��;@��w@��P@�@�M�@��-@�bN@�33@�v�@��^@��/@�@�33@�@���@��@@�o@�R@��T@�?}@�z�@��@��;@�w@�C�@�J@�x�@��@���@�bN@�P@�"�@��@��@�"�@�;d@�;d@���@�ff@�@��#@��`@���@�33@�33@�
=@��@��@�ff@�hs@�@�bN@�I�@���@�l�@��@ް!@���@��@�(�@�t�@��y@��@���@��@��H@ڏ\@�n�@�$�@�J@��@���@٩�@�`B@�%@ج@��
@�t�@�
=@��#@�O�@��`@� �@ӝ�@���@ҧ�@҇+@�n�@�V@��@�&�@�z�@�Z@� �@�S�@Ώ\@�E�@���@��@���@ͩ�@�p�@���@�Z@�S�@��@���@ʧ�@�ff@�5?@ɲ-@�Q�@�  @ǝ�@�;d@�ȴ@��@�@�?}@���@ċD@���@�"�@§�@�-@�@��T@��h@�G�@���@���@�Z@��@�v�@�-@�$�@��#@�@�`B@�Ĝ@�1'@��m@���@���@���@��P@�|�@�t�@�\)@�S�@�S�@�"�@�o@�^5@�{@���@���@�bN@�Q�@�1'@��
@�ƨ@���@�C�@���@��!@�V@�-@��@���@�G�@��9@���@�t�@�S�@��@��R@�~�@��@��@���@���@�z�@� �@��@�33@�n�@��@��h@���@�j@�Z@�1'@�t�@�
=@��\@���@�7L@�V@���@�(�@��w@�l�@�K�@�o@��+@�5?@�-@�$�@�@�`B@��j@��D@�j@�1@�dZ@���@�-@��@��@��@�%@���@��9@��@��P@�
=@�V@��@���@��@�G�@�/@���@��u@�I�@�(�@��;@���@�^5@�=q@��@���@��^@�p�@�hs@�?}@��/@���@�z�@�bN@�  @���@��w@��@�33@���@�@���@�p�@�%@���@�bN@�I�@�1'@� �@���@��F@�33@��R@�{@�p�@�V@��@��D@�r�@�A�@��@��w@���@��@�t�@�S�@�"�@��H@���@��\@��+@�~�@�^5@�V@�=q@�-@�$�@��@���@��#@���@��@�G�@�/@��@���@���@���@�bN@���@�S�@��@��H@���@�~�@�ff@�^5@�^5@�M�@�-@��@��@���@��-@��7@�p�@�7L@��j@�9X@��@���@��P@�dZ@�C�@�
=@��H@���@���@�M�@��T@���@���@�hs@�G�@�/@�V@�z�@��F@�l�@�K�@�@�n�@�@��@�O�@�G�@�G�@�7L@�/@�&�@��@�%@���@��@���@�j@�  @�@�@\)@\)@+@~ȴ@~$�@}��@}�@}O�@|��@{�m@{��@{�@{C�@z�@z~�@y�@xA�@w�@wl�@w\)@wK�@w+@v��@vȴ@u�@t�@tz�@tZ@tI�@s�
@s��@st�@s"�@s@r�H@r��@r��@r�\@r~�@rn�@r=q@r=q@r-@r�@qx�@ol�@nE�@m��@m?}@l�/@l��@l�D@l1@kt�@kC�@k"�@j��@j-@i��@i��@i�^@i�7@ihs@iX@hĜ@g�w@g�@f��@f5?@ep�@d�j@dz�@d9X@d�@c��@b��@b=q@a��@a�7@`bN@_��@_K�@^5?@^@]��@]p�@]�@\��@\9X@\�@[��@[��@[��@[��@[dZ@["�@[@Z��@Z~�@Y�#@YG�@X�9@Xr�@XA�@XA�@XA�@X �@X  @W|�@W
=@V�@V��@Vff@VE�@V$�@U@U`B@U/@T��@T�@Tj@S��@R��@R^5@R=q@Q�#@Qhs@P1'@O��@O�@Ol�@O�@Nȴ@N��@N@M�h@MO�@MO�@MO�@M?}@MV@MV@M�@L�D@Kƨ@Ko@Jn�@JM�@J=q@J�@I��@I�#@I�#@I��@I�^@I��@I�^@I��@I�7@IG�@HbN@Hb@G��@F�@E��@E/@D��@Dj@D9X@C��@C�F@C��@Ct�@B-@A&�@@��@@�`@@�`@@1'@?�@?�w@>ȴ@>��@>5?@=@=p�@<��@<�D@<Z@<1@;��@;dZ@;"�@:�\@:M�@:-@9��@9��@9x�@9&�@8bN@7�@7�P@7+@6�y@6��@6$�@5�@4�@4��@4�D@4z�@4Z@4I�@41@3��@3dZ@3C�@333@3o@2��@2��@2n�@2^5@2=q@1��@1�#@1�7@17L@0��@0��@0Ĝ@0 �@/�w@/
=@.��@-��@-`B@-O�@-?}@-/@-�@,�@,�j@,Z@+��@+�m@+ƨ@+��@+��@+t�@+S�@+33@+"�@*�@*^5@*J@)�^@)��@)&�@(Ĝ@(�u@(r�@(A�@( �@(b@'�w@'K�@&��@&��@&E�@&{@&@%��@%�-@%p�@%/@$�@$�j@$�D@$j@$(�@#��@#�F@#@"�!@"�\@"J@!��@!X@!%@ �9@ Q�@ 1'@  �@ b@�@��@�P@l�@\)@�R@E�@E�@E�@{@�@`B@/@�@/@��@�j@�@��@�D@j@(�@1@��@��@��@ƨ@�F@�F@�@��@~�@-@��@��@��@�#@�^@��@hs@G�@&�@%@��@�`@��@�9@bN@A�@A�@ �@�@|�@l�@l�@\)@\)@\)@K�@;d@�y@ff@E�@5?@$�@{@��@�@/@�@V@�@�/@�@�D@�D@z�@j@Z@��@�@dZ@C�@C�@33@"�@��@M�@��@�@�@�#@�#@�^@hs@7L@&�@��@Ĝ@r�@A�@ �@�@��@K�@+@�y@�@�R@�+@v�@ff@5?@�T@@�-@`B@�@�@V@z�@j@Z@I�@I�@I�@(�@�@��@�@dZ@S�@S�@C�@@
��@
�!@
�\@
~�@
~�@
n�@
^5@
�@	��@	X@	%@��@��@�`@�9@�u@�@bN@1'@��@�wA�;dA�A�A�E�A�E�A�E�A�C�A�C�A�G�A�Q�A�O�A�O�A�M�A�M�A�O�A�Q�A�Q�A�S�A�S�A�Q�A�Q�A�Q�A�O�A�O�A�Q�A�S�A�VA�S�A�S�A�S�A�VA�Q�A�S�A�VA�VA�VA�VA�S�A�Q�A�Q�A�S�A�XA�ZA�ZA�\)A�ZA�XA�VA�VA�VA�S�A�S�A�S�A�Q�A�O�A�S�A�VA�VA�ZA�XA�S�A�Q�A�S�A�XA�XA�XA�VA�S�A�Q�A�S�A�XA�XA�XA�S�A�S�A�S�A�XA�VA�XA�VA�S�A�S�A�VA�VA�VA�VA�S�A�Q�A�S�A�XA�XA�VA�S�A�S�A�VA�XA�XA�ZA�XA�XA�VA�VA�ZA�\)A�ZA�ZA�XA�XA�XA�XA�ZA�\)A�^5A�\)A�\)A�ZA�XA�ZA�\)A�^5A�`BA�^5A�\)A�\)A�ZA�\)A�\)A�\)A�^5A�\)A�\)A�ZA�ZA�ZA�^5A�bNA�bNA�bNA�`BA�`BA�`BA�bNA�bNA�bNA�`BA�^5A�`BA�bNA�dZA�dZA�bNA�`BA�bNA�bNA�dZA�bNA�bNA�`BA�`BA�^5A�\)A�^5A�`BA�`BA�`BA�^5A�\)A�\)A�\)A�`BA�bNA�dZA�`BA�^5A�`BA�bNA�bNA�bNA�`BA�^5A�^5A�bNA�dZA�ffA�dZA�dZA�`BA�`BA�`BA�bNA�bNA�bNA�hsA�ffA�jA�hsA�hsA�hsA�ffA�hsA�hsA�jA�ffA�bNA�dZA�ffA�jA�jA�jA�jA�jA�hsA�ffA�ffA�hsA�n�A�p�A�l�A�l�A�l�A�n�A�p�A�r�A�p�A�l�A�l�A�n�A�n�A�jA�jA�hsA�hsA�hsA�l�A�l�A�jA�ffA�ffA�hsA�jA�jA�jA�hsA�hsA�hsA�jA�jA�l�A�ffA�ffA�hsA�jA�jA�jA�hsA�ffA�ffA�hsA�jA�jA�hsA�ffA�jA�jA�l�A�l�A�l�A�hsA�hsA�jA�jA�l�A�l�A�l�A�ffA�ffA�ffA�hsA�ffA�bNA�bNA�bNA�ffA�ffA�bNA�`BA�^5A�dZA�dZA�bNA�^5A�^5A�XA�VA�XA�O�A�S�A�O�A�9XA�33A�33A�9XA�7LA�5?A�/A�/A�/A�"�A��A��A��A��A�%A��;A�bNA˟�A�bAʮA�%A�Q�A�XA��jA�t�A��mA���A��RA�jA��TA�XA��!A���A�l�A��
A��/A��`A���A�A�A�bA���A��\A��^A�7LA���A�5?A��TA���A�hsA��A��A��!A��
A��A�A�  A��-A��A�K�A���A�K�A���A���A�-A�ffA�1'A�r�A���A�jA�/A���A�G�A�?}A�&�A��A��mA���A�33A�VA�VA��!A��A�|�A�XA�+A���A���A�|�A�M�A���A��PA�/A��A���A���A���A���A���A��A�`BA�JA�O�A�%A��A���A��A�hsA�O�A�"�A���A��A���A��RA��7A�E�A���A�G�A���A��-A�S�A�A�K�A�33A���A�ZA�&�A��A��9A���A���A���A���A���A��A��FA�\)A�`BA�ȴA���A���A���A���A���A���A���A�ȴA�ȴA�ȴA��A��7A�t�A�hsA�^5A�Q�A�I�A�?}A�33A�/A�$�A��A�JA���A��A��FA�z�A�=qA��A��-A��\A�x�A�bNA�7LA���A���A�\)A��RA�l�A�S�A�O�A�?}A��A��A�VA�A���A��;A���A�ƨA��A���A�t�A�\)A�$�A��A��uA�A��-A��hA�x�A�jA�G�A�+A�  A�ĜA��7A�`BA�;dA�-A�bA���A��
A��RA��\A�^5A�-A�JA���A�ȴA�-A�dZA��`A�A�A��DA��A�1A��A���A�l�A�$�A���A�S�A��
A�p�A�JA���A�S�A�-A�$�A��A�bA�VA���A��mA��/A�ĜA���A��A�?}A���A�bNA�O�A�7LA�7LA�33A�/A�$�A�1A�ƨA���A�bNA�?}A�  A�v�A�=qA�"�A�
=A���A��A��A���A�A��!A�~�A�E�A��A�
=A���A��;A���A��jA��FA��A���A��uA�|�A�dZA��A��A�r�A�bNA�VA�ZA�K�A�?}A� �A��TA��9A��PA�t�A�^5A�G�A�1'A�{A�;AA~A�A}�A|��A|ffA|�A{��A{K�Az�AzjAzZAzffAzv�Azn�AzM�AzE�AzAyx�AyoAx�+AxJAw�
Av�HAvn�AvJAu�FAt��At�\AtjAtA�AtbAs�#As��AsO�AsAr�!Arr�Ar1'Aq�mAqx�Ap��Ap-AoS�AnQ�Al��Ak�;AkVAj�\Aj^5Aj(�Ai�AihsAi%Ah��AhZAh-Ag�wAgl�Ag/Ag&�Af��AfȴAf�AfI�Ae�#Ae�hAe&�Ad�9Ac��AcoAb�jAbjAbE�Ab{Aa�Aa��Aa�wAa��Aa��Aap�Aa33A`�A`�9A`M�A`1A_�^A^v�A\�/A[��AZȴAZ��AZZAY�AYoAX��AX-AW�-AWdZAW&�AV�`AV��AV$�AV1AU�AU�FAU��AUS�AT��AQ��AP��AO"�AL��AKAI�
AI�AIK�AI7LAIVAH�AH�DAHI�AH{AG��AG�AG�TAG�
AG��AG�wAGx�AG/AF�HAF��AF�!AF��AFffAFE�AF�AE�AE�AE�
AE��AE�#AE��AE��AE�wAE��AE�7AEp�AEdZAE33AEoAD�/AD�uADZAD9XAD5?AD-AD �AD �AD{ADJAD1ADAC��AC�mAC�TAC�;AC�#AC�
AC��ACƨAC��AChsAC"�AB�!AA��A@ĜA@  A?O�A>Q�A=33A;�A;G�A:ĜA:$�A9hsA8�A8�9A8�RA8v�A8jA8A�A81'A8bA7��A6�A6n�A6A�A6 �A6{A61A61A5��A5�A5�#A5�
A5��A5�wA5��A5t�A5"�A4��A4~�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                    A�C�A�G�A�Q�A�Q�A�Q�A�Q�A�S�A�S�A�S�A�VA�VA�XA�S�A�VA�VA�VA�S�A�VA�XA�XA�ZA�\)A�\)A�\)A�^5A�bNA�bNA�bNA�^5A�`BA�`BA�bNA�bNA�hsA�ffA�hsA�l�A�n�A�jA�hsA�hsA�hsA�hsA�jA�hsA�bNA�\)A�;dA�oA�dZA��A��A���A�VA�bA�ȴA���A�ĜA��\A���A���A�"�A�x�A��`A��A�5?A�VA��A���A��A�A��mA���A��A�$�A��!A��A�~�A���A��
A��wA��A}VAzM�Aw/As�Ao��Ai�PAf�HAc?}Aa%AZ�AVv�AN��AH�\AG\)AF�AE�ADI�AC�AB�jA<I�A8bNA6E�A5�-A4$�A3oA2��A2M�A1��A1�;A2�uA2I�A.v�A(��A'VA%�hA$��A#A!��A!|�A �A�jAM�A9XA �A  AƨAG�A9XAl�A�A��A��A~�AA�Ax�A�RA�A�-A33A��A$�A�A��A�Ax�A`BA��A�A��A&�A�!AM�A��A%AXAn�AbAS�AQ�A�A�\AA
�A	�#A�uA��A\)A�yA1'A��A��A��A?}A"�A"�A/A�A�yAdZ@���@�x�@���@�K�@��@��@��m@��/@�"�@��u@��;@��w@��P@�@�M�@��-@�bN@�33@�v�@��^@��/@�@�33@�@���@��@@�o@�R@��T@�?}@�z�@��@��;@�w@�C�@�J@�x�@��@���@�bN@�P@�"�@��@��@�"�@�;d@�;d@���@�ff@�@��#@��`@���@�33@�33@�
=@��@��@�ff@�hs@�@�bN@�I�@���@�l�@��@ް!@���@��@�(�@�t�@��y@��@���@��@��H@ڏ\@�n�@�$�@�J@��@���@٩�@�`B@�%@ج@��
@�t�@�
=@��#@�O�@��`@� �@ӝ�@���@ҧ�@҇+@�n�@�V@��@�&�@�z�@�Z@� �@�S�@Ώ\@�E�@���@��@���@ͩ�@�p�@���@�Z@�S�@��@���@ʧ�@�ff@�5?@ɲ-@�Q�@�  @ǝ�@�;d@�ȴ@��@�@�?}@���@ċD@���@�"�@§�@�-@�@��T@��h@�G�@���@���@�Z@��@�v�@�-@�$�@��#@�@�`B@�Ĝ@�1'@��m@���@���@���@��P@�|�@�t�@�\)@�S�@�S�@�"�@�o@�^5@�{@���@���@�bN@�Q�@�1'@��
@�ƨ@���@�C�@���@��!@�V@�-@��@���@�G�@��9@���@�t�@�S�@��@��R@�~�@��@��@���@���@�z�@� �@��@�33@�n�@��@��h@���@�j@�Z@�1'@�t�@�
=@��\@���@�7L@�V@���@�(�@��w@�l�@�K�@�o@��+@�5?@�-@�$�@�@�`B@��j@��D@�j@�1@�dZ@���@�-@��@��@��@�%@���@��9@��@��P@�
=@�V@��@���@��@�G�@�/@���@��u@�I�@�(�@��;@���@�^5@�=q@��@���@��^@�p�@�hs@�?}@��/@���@�z�@�bN@�  @���@��w@��@�33@���@�@���@�p�@�%@���@�bN@�I�@�1'@� �@���@��F@�33@��R@�{@�p�@�V@��@��D@�r�@�A�@��@��w@���@��@�t�@�S�@�"�@��H@���@��\@��+@�~�@�^5@�V@�=q@�-@�$�@��@���@��#@���@��@�G�@�/@��@���@���@���@�bN@���@�S�@��@��H@���@�~�@�ff@�^5@�^5@�M�@�-@��@��@���@��-@��7@�p�@�7L@��j@�9X@��@���@��P@�dZ@�C�@�
=@��H@���@���@�M�@��T@���@���@�hs@�G�@�/@�V@�z�@��F@�l�@�K�@�@�n�@�@��@�O�@�G�@�G�@�7L@�/@�&�@��@�%@���@��@���@�j@�  @�@�@\)@\)@+@~ȴ@~$�@}��@}�@}O�@|��@{�m@{��@{�@{C�@z�@z~�@y�@xA�@w�@wl�@w\)@wK�@w+@v��@vȴ@u�@t�@tz�@tZ@tI�@s�
@s��@st�@s"�@s@r�H@r��@r��@r�\@r~�@rn�@r=q@r=q@r-@r�@qx�@ol�@nE�@m��@m?}@l�/@l��@l�D@l1@kt�@kC�@k"�@j��@j-@i��@i��@i�^@i�7@ihs@iX@hĜ@g�w@g�@f��@f5?@ep�@d�j@dz�@d9X@d�@c��@b��@b=q@a��@a�7@`bN@_��@_K�@^5?@^@]��@]p�@]�@\��@\9X@\�@[��@[��@[��@[��@[dZ@["�@[@Z��@Z~�@Y�#@YG�@X�9@Xr�@XA�@XA�@XA�@X �@X  @W|�@W
=@V�@V��@Vff@VE�@V$�@U@U`B@U/@T��@T�@Tj@S��@R��@R^5@R=q@Q�#@Qhs@P1'@O��@O�@Ol�@O�@Nȴ@N��@N@M�h@MO�@MO�@MO�@M?}@MV@MV@M�@L�D@Kƨ@Ko@Jn�@JM�@J=q@J�@I��@I�#@I�#@I��@I�^@I��@I�^@I��@I�7@IG�@HbN@Hb@G��@F�@E��@E/@D��@Dj@D9X@C��@C�F@C��@Ct�@B-@A&�@@��@@�`@@�`@@1'@?�@?�w@>ȴ@>��@>5?@=@=p�@<��@<�D@<Z@<1@;��@;dZ@;"�@:�\@:M�@:-@9��@9��@9x�@9&�@8bN@7�@7�P@7+@6�y@6��@6$�@5�@4�@4��@4�D@4z�@4Z@4I�@41@3��@3dZ@3C�@333@3o@2��@2��@2n�@2^5@2=q@1��@1�#@1�7@17L@0��@0��@0Ĝ@0 �@/�w@/
=@.��@-��@-`B@-O�@-?}@-/@-�@,�@,�j@,Z@+��@+�m@+ƨ@+��@+��@+t�@+S�@+33@+"�@*�@*^5@*J@)�^@)��@)&�@(Ĝ@(�u@(r�@(A�@( �@(b@'�w@'K�@&��@&��@&E�@&{@&@%��@%�-@%p�@%/@$�@$�j@$�D@$j@$(�@#��@#�F@#@"�!@"�\@"J@!��@!X@!%@ �9@ Q�@ 1'@  �@ b@�@��@�P@l�@\)@�R@E�@E�@E�@{@�@`B@/@�@/@��@�j@�@��@�D@j@(�@1@��@��@��@ƨ@�F@�F@�@��@~�@-@��@��@��@�#@�^@��@hs@G�@&�@%@��@�`@��@�9@bN@A�@A�@ �@�@|�@l�@l�@\)@\)@\)@K�@;d@�y@ff@E�@5?@$�@{@��@�@/@�@V@�@�/@�@�D@�D@z�@j@Z@��@�@dZ@C�@C�@33@"�@��@M�@��@�@�@�#@�#@�^@hs@7L@&�@��@Ĝ@r�@A�@ �@�@��@K�@+@�y@�@�R@�+@v�@ff@5?@�T@@�-@`B@�@�@V@z�@j@Z@I�@I�@I�@(�@�@��@�@dZ@S�@S�@C�@@
��@
�!@
�\@
~�@
~�@
n�@
^5@
�@	��@	X@	%@��@��@�`@�9@�u@�@bN@1'@��@�wA�;dA�A�A�E�A�E�A�E�A�C�A�C�A�G�A�Q�A�O�A�O�A�M�A�M�A�O�A�Q�A�Q�A�S�A�S�A�Q�A�Q�A�Q�A�O�A�O�A�Q�A�S�A�VA�S�A�S�A�S�A�VA�Q�A�S�A�VA�VA�VA�VA�S�A�Q�A�Q�A�S�A�XA�ZA�ZA�\)A�ZA�XA�VA�VA�VA�S�A�S�A�S�A�Q�A�O�A�S�A�VA�VA�ZA�XA�S�A�Q�A�S�A�XA�XA�XA�VA�S�A�Q�A�S�A�XA�XA�XA�S�A�S�A�S�A�XA�VA�XA�VA�S�A�S�A�VA�VA�VA�VA�S�A�Q�A�S�A�XA�XA�VA�S�A�S�A�VA�XA�XA�ZA�XA�XA�VA�VA�ZA�\)A�ZA�ZA�XA�XA�XA�XA�ZA�\)A�^5A�\)A�\)A�ZA�XA�ZA�\)A�^5A�`BA�^5A�\)A�\)A�ZA�\)A�\)A�\)A�^5A�\)A�\)A�ZA�ZA�ZA�^5A�bNA�bNA�bNA�`BA�`BA�`BA�bNA�bNA�bNA�`BA�^5A�`BA�bNA�dZA�dZA�bNA�`BA�bNA�bNA�dZA�bNA�bNA�`BA�`BA�^5A�\)A�^5A�`BA�`BA�`BA�^5A�\)A�\)A�\)A�`BA�bNA�dZA�`BA�^5A�`BA�bNA�bNA�bNA�`BA�^5A�^5A�bNA�dZA�ffA�dZA�dZA�`BA�`BA�`BA�bNA�bNA�bNA�hsA�ffA�jA�hsA�hsA�hsA�ffA�hsA�hsA�jA�ffA�bNA�dZA�ffA�jA�jA�jA�jA�jA�hsA�ffA�ffA�hsA�n�A�p�A�l�A�l�A�l�A�n�A�p�A�r�A�p�A�l�A�l�A�n�A�n�A�jA�jA�hsA�hsA�hsA�l�A�l�A�jA�ffA�ffA�hsA�jA�jA�jA�hsA�hsA�hsA�jA�jA�l�A�ffA�ffA�hsA�jA�jA�jA�hsA�ffA�ffA�hsA�jA�jA�hsA�ffA�jA�jA�l�A�l�A�l�A�hsA�hsA�jA�jA�l�A�l�A�l�A�ffA�ffA�ffA�hsA�ffA�bNA�bNA�bNA�ffA�ffA�bNA�`BA�^5A�dZA�dZA�bNA�^5A�^5A�XA�VA�XA�O�A�S�A�O�A�9XA�33A�33A�9XA�7LA�5?A�/A�/A�/A�"�A��A��A��A��A�%A��;A�bNA˟�A�bAʮA�%A�Q�A�XA��jA�t�A��mA���A��RA�jA��TA�XA��!A���A�l�A��
A��/A��`A���A�A�A�bA���A��\A��^A�7LA���A�5?A��TA���A�hsA��A��A��!A��
A��A�A�  A��-A��A�K�A���A�K�A���A���A�-A�ffA�1'A�r�A���A�jA�/A���A�G�A�?}A�&�A��A��mA���A�33A�VA�VA��!A��A�|�A�XA�+A���A���A�|�A�M�A���A��PA�/A��A���A���A���A���A���A��A�`BA�JA�O�A�%A��A���A��A�hsA�O�A�"�A���A��A���A��RA��7A�E�A���A�G�A���A��-A�S�A�A�K�A�33A���A�ZA�&�A��A��9A���A���A���A���A���A��A��FA�\)A�`BA�ȴA���A���A���A���A���A���A���A�ȴA�ȴA�ȴA��A��7A�t�A�hsA�^5A�Q�A�I�A�?}A�33A�/A�$�A��A�JA���A��A��FA�z�A�=qA��A��-A��\A�x�A�bNA�7LA���A���A�\)A��RA�l�A�S�A�O�A�?}A��A��A�VA�A���A��;A���A�ƨA��A���A�t�A�\)A�$�A��A��uA�A��-A��hA�x�A�jA�G�A�+A�  A�ĜA��7A�`BA�;dA�-A�bA���A��
A��RA��\A�^5A�-A�JA���A�ȴA�-A�dZA��`A�A�A��DA��A�1A��A���A�l�A�$�A���A�S�A��
A�p�A�JA���A�S�A�-A�$�A��A�bA�VA���A��mA��/A�ĜA���A��A�?}A���A�bNA�O�A�7LA�7LA�33A�/A�$�A�1A�ƨA���A�bNA�?}A�  A�v�A�=qA�"�A�
=A���A��A��A���A�A��!A�~�A�E�A��A�
=A���A��;A���A��jA��FA��A���A��uA�|�A�dZA��A��A�r�A�bNA�VA�ZA�K�A�?}A� �A��TA��9A��PA�t�A�^5A�G�A�1'A�{A�;AA~A�A}�A|��A|ffA|�A{��A{K�Az�AzjAzZAzffAzv�Azn�AzM�AzE�AzAyx�AyoAx�+AxJAw�
Av�HAvn�AvJAu�FAt��At�\AtjAtA�AtbAs�#As��AsO�AsAr�!Arr�Ar1'Aq�mAqx�Ap��Ap-AoS�AnQ�Al��Ak�;AkVAj�\Aj^5Aj(�Ai�AihsAi%Ah��AhZAh-Ag�wAgl�Ag/Ag&�Af��AfȴAf�AfI�Ae�#Ae�hAe&�Ad�9Ac��AcoAb�jAbjAbE�Ab{Aa�Aa��Aa�wAa��Aa��Aap�Aa33A`�A`�9A`M�A`1A_�^A^v�A\�/A[��AZȴAZ��AZZAY�AYoAX��AX-AW�-AWdZAW&�AV�`AV��AV$�AV1AU�AU�FAU��AUS�AT��AQ��AP��AO"�AL��AKAI�
AI�AIK�AI7LAIVAH�AH�DAHI�AH{AG��AG�AG�TAG�
AG��AG�wAGx�AG/AF�HAF��AF�!AF��AFffAFE�AF�AE�AE�AE�
AE��AE�#AE��AE��AE�wAE��AE�7AEp�AEdZAE33AEoAD�/AD�uADZAD9XAD5?AD-AD �AD �AD{ADJAD1ADAC��AC�mAC�TAC�;AC�#AC�
AC��ACƨAC��AChsAC"�AB�!AA��A@ĜA@  A?O�A>Q�A=33A;�A;G�A:ĜA:$�A9hsA8�A8�9A8�RA8v�A8jA8A�A81'A8bA7��A6�A6n�A6A�A6 �A6{A61A61A5��A5�A5�#A5�
A5��A5�wA5��A5t�A5"�A4��A4~�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                    G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	a�B	`�B	aB	aHB	`�B	aHB	aB	aB	aHB	`�B	aB	`�B	aHB	aHB	a|B	a|B	aHB	a�B	a|B	a|B	aHB	aHB	a|B	a|B	aHB	aHB	aHB	aHB	a�B	a|B	a|B	a|B	aHB	aB	aHB	aHB	aB	`�B	a|B	a�B	bB	b�B	bB	a�B	a�B	aHB	`vB	a�B	_�B	͟B
b�BuB'B@B=<B9�B>BB=B_pBiyBs�BgmBe�BwfBn�Bh>BL�B�B!-BqAB_;B-�B
=B
خB
��B
��B
u�B
`vB
FB
7LB
)�B
�B
SB	��B	�B	�XB	��B	�1B	x�B	h�B	U�B	S�B	>�B	(XB��B	B	YB	�B	/�B	*eB	%�B	"B��B�mB�&B�
B��B�B�B��B	�B	B	&�B	"4B�B�fB�B�NB�B�5B�B��B��B�|B�TB�`B�B�B��B�xB	�B	�B	+B	+B	�B	�B	�B	�B	OB	*�B	<�B	C�B	HB	J#B	M�B	N<B	RTB	]/B	i�B	cB	��B	�B	�B	�FB	�B	��B	��B	�{B	�uB	��B	��B	�rB	��B	��B	��B	��B	��B	~]B	y�B	xlB	t�B	s�B	s�B	qAB	p�B	qvB	rB	t�B	w�B	xB	w2B	hsB	f�B	\�B	XEB	Y�B	^�B	d�B	m�B	��B	�OB	�B	�FB	�B	�zB	��B	�B	��B	��B	��B	��B	�zB	��B	��B	��B	�hB	��B	�!B	��B	�VB	��B	�bB	�B	�FB	�zB	�FB	�RB	�0B	�eB	�B	�6B	�CB	��B	��B	��B	��B	��B	�}B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	�UB	��B	�'B	��B	�-B	��B	��B	��B	�B	�B	�B	��B	�LB	��B	��B	�B	�hB	��B	��B	��B	��B	B	��B	�mB	��B	�tB	�B	ȀB	��B	ɆB	��B	�KB	��B	ǮB	��B	�zB	�RB	ȴB	��B	��B	��B	ɺB	ɺB	ʌB	�6B	̘B	̘B	��B	�pB	�B	ΥB	�B	ϫB	�TB	�2B	֡B	�
B	�yB	��B	ٴB	�B	�KB	��B	��B	��B	��B	��B	�/B	ݘB	�5B	ޞB	�pB	�;B	�B	��B	��B	�B	ߤB	�B	��B	�;B	�BB	�B	�vB	��B	�B	�B	�TB	��B	��B	�B	�NB	� B	��B	��B	��B	��B	�B	�B	��B	�`B	�B	��B	�`B	�B	�,B	�fB	��B	��B	�B	��B	�B	��B	��B	�B	�B	�B	�WB	��B	�"B	�B	��B	��B	��B	�]B	�B	�oB	�B	�oB	��B	�oB	�B	�B	�B	�GB	�|B	�|B	�B	�B	�TB	�+B	�+B	��B	�8B	��B	��B	��B	�B	��B	��B	�(B	��B	�(B	�cB
 iB
 �B
oB
;B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
fB
	�B

=B

=B

rB
DB
�B
DB
�B
�B
�B
VB
�B
�B
�B
�B
4B
hB
�B
hB
�B
B
�B
B
�B
MB
MB
B
�B
�B
�B
�B
$B
�B
�B
YB
_B
_B
�B
+B
�B
�B
=B
=B
qB
CB
�B
CB
xB
B
�B
B
B
�B
OB
�B
VB
VB
VB
�B
�B
 �B
!�B
!�B
!�B
!�B
!�B
"4B
"4B
"�B
#nB
#nB
#B
"�B
#:B
"�B
#nB
#�B
$B
$B
$B
#�B
#�B
$�B
$�B
$�B
%zB
%�B
%�B
%zB
&�B
&�B
&�B
'B
'�B
($B
'�B
($B
'�B
'B
'�B
(�B
)*B
)*B
)�B
)�B
)�B
)_B
)�B
)�B
*�B
+B
+�B
+�B
,B
,�B
,�B
,=B
,�B
,�B
,�B
-CB
-B
-wB
-wB
-�B
-�B
-�B
.IB
0�B
/�B
/�B
0�B
0�B
2-B
2�B
33B
2�B
2�B
2�B
2�B
33B
2�B
3hB
33B
33B
3�B
4nB
5?B
4�B
5B
5�B
4�B
5tB
5?B
6zB
7B
6�B
7�B
8RB
8�B
8�B
8�B
9$B
8�B
8�B
:^B
<jB
<B
<jB
;�B
;�B
<B
;�B
<B
=�B
=�B
=�B
>B
=�B
>�B
>wB
>wB
?B
>�B
?HB
?B
?}B
>�B
?}B
?B
?}B
>�B
?B
>�B
?B
A�B
B[B
B[B
B�B
CaB
B�B
C�B
C�B
C�B
D�B
C�B
EmB
EmB
EB
E�B
E�B
EmB
EmB
E9B
F�B
GEB
HKB
H�B
HKB
I�B
J#B
I�B
J#B
I�B
IRB
K)B
K^B
J�B
K^B
M6B
LdB
L�B
M�B
MjB
M�B
NB
N�B
N�B
OB
OB
OBB
N�B
N<B
N<B
NpB
N�B
N�B
OBB
O�B
PHB
QB
QNB
Q�B
R B
Q�B
QNB
Q�B
Q�B
R B
R�B
R�B
S&B
R�B
R�B
R�B
R�B
S�B
S&B
T,B
S[B
S�B
T,B
U�B
U2B
UgB
U2B
U�B
V�B
V9B
VmB
VmB
W?B
W?B
W
B
XB
XEB
XEB
XEB
XyB
X�B
XEB
XyB
W�B
YB
YKB
YKB
ZB
Y�B
Y�B
ZB
Y�B
ZQB
Y�B
ZB
ZQB
YB
Y�B
ZB
YB
Y�B
Z�B
[#B
ZQB
\�B
\�B
]dB
]/B
]�B
]�B
^5B
]�B
^B
]dB
_�B
_B
^�B
_B
^jB
_pB
_pB
_pB
`�B
`B
aB
`�B
`�B
a|B
a|B
a�B
a�B
bB
b�B
bNB
b�B
c�B
b�B
c�B
c B
c�B
d&B
d�B
d�B
d�B
d�B
d�B
d�B
e�B
f�B
f�B
ffB
f2B
ffB
ffB
f�B
g�B
h
B
g�B
g�B
g�B
hsB
iB
iB
h�B
h
B
h�B
hsB
iyB
i�B
jB
jKB
i�B
jB
j�B
k�B
k�B
l�B
m]B
l�B
m�B
m]B
m]B
m)B
m�B
m�B
n/B
n�B
o B
o B
oiB
o B
oiB
o5B
oiB
o5B
o�B
p;B
poB
p�B
p�B
qAB
qvB
q�B
q�B
q�B
rB
q�B
rB
r|B
r�B
sB
s�B
s�B
s�B
s�B
s�B
tB
tB
tTB
t�B
t�B
t�B
t�B
t�B
u%B
v+B
v+B
v+B
v�B
v�B
w2B
wfB
xB
x8B
x8B
x8B
x8B
xlB
x�B
x�B
x�B
xlB
y�B
y�B
y�B
y�B
y�B
zxB
z�B
z�B
z�B
z�B
{B
{B
{B
{JB
{JB
{B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|B
}"B
}"B
}VB
}�B
}�B
}�B
}�B
}�B
}�B
~(B
~]B
~�B
~�B
~�B
~�B
~�B
~�B
cB
.B
.B
.B
� B
�4B
�4B
�4B
�4B
�4B
�4B
�4B
�4B
��B
�B
�;B
�;B
�oB
�B
��B
�B
�B
�AB
�AB
�AB
�uB
��B
��B
��B
��B
��B
��B
��B
�{B
��B
��B
��B
��B
��B
�MB
��B
�B
��B
�B
�B
��B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�+B
�_B
��B
��B
��B
��B
�1B
�fB
�fB
��B
�B
�7B
�7B
�lB
��B
��B
�	B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�xB
��B
��B
��B
��B
�B
�~B
�JB
�~B
�~B
�JB
�JB
�JB
�~B
��B
�"B
��B
��B
��B
��B
�"B
�"B
�VB
��B
�(B
��B
��B	\)B	c�B	bNB	aB	aB	aB	`�B	_B	`vB	aHB	aHB	a�B	bB	a�B	a|B	`�B	`BB	`B	`�B	a|B	a|B	bB	a�B	aHB	`vB	`vB	`vB	`BB	`vB	a|B	bNB	bNB	`BB	`vB	`vB	`BB	a|B	bB	bB	bB	`�B	`BB	_�B	_�B	`BB	`�B	a�B	a�B	a�B	aB	`�B	`vB	a|B	bNB	aHB	a|B	`vB	_pB	a|B	b�B	cTB	a�B	`BB	`BB	`�B	aHB	a�B	bNB	a�B	`�B	`vB	`�B	a|B	a�B	a�B	`�B	`�B	`�B	aHB	bB	a�B	aB	`�B	`�B	aHB	bB	b�B	a�B	`�B	aHB	a�B	b�B	b�B	bB	a|B	aB	`�B	aB	a|B	b�B	bB	aB	`�B	`�B	aB	a�B	bNB	`�B	bNB	aB	`�B	`B	`vB	`�B	bB	bB	a�B	aB	`vB	`BB	aB	a�B	b�B	b�B	bNB	a|B	`�B	`�B	`�B	a|B	b�B	b�B	bB	aB	`BB	`vB	`�B	a�B	bB	aHB	`�B	`�B	`�B	a|B	bNB	a�B	`�B	`�B	`�B	a|B	bNB	a|B	aB	`B	aB	`�B	a|B	b�B	b�B	b�B	a|B	`�B	`�B	`�B	a�B	b�B	b�B	b�B	a|B	`vB	`vB	a�B	b�B	a�B	aHB	`�B	`vB	a�B	bNB	bNB	aHB	aB	`B	`�B	aHB	bNB	bNB	bNB	a�B	aB	aB	_pB	`vB	`B	aHB	`vB	a�B	bNB	aHB	`�B	`BB	aHB	b�B	bNB	aHB	`�B	aB	`BB	`�B	`�B	aHB	bNB	a�B	aHB	aB	`B	a|B	a�B	a|B	`vB	_�B	`B	`�B	a�B	a�B	aHB	`�B	`�B	a|B	a�B	bNB	a�B	`�B	`�B	a�B	b�B	b�B	b�B	a�B	`�B	a�B	b�B	b�B	b�B	a�B	aHB	`�B	cTB	cTB	b�B	a�B	a|B	bB	b�B	c�B	c B	b�B	a|B	a�B	bNB	c B	b�B	bNB	`�B	`�B	a�B	b�B	b�B	a�B	bB	`vB	`�B	aHB	b�B	b�B	a�B	aB	aB	b�B	b�B	bNB	`vB	`�B	aHB	a�B	a�B	_�B	_�B	`vB	`�B	`BB	_�B	`BB	`�B	a�B	_�B	`vB	d&B	d&B	c�B	`vB	`vB	`�B	a|B	aB	`B	b�B	`�B	`BB	_B	[�B	\]B	^�B	j�B	}�B	�B	�%B	��B	רB
`�B
K�B
T�B
D�B
9�B
E9B
^�B
�qB
�B
ΥB
�WB'BB!�B+�B�B$@B \B"4B"hBJ�B9�B>wBD�BA B;0B:*B<6B1'BA�BHKB0�BB�BIB0�B0�B(�BZ�B7B1'B%FB4nB=�BLdBJ#BMjB+6B6�BM�B?�BD�BO�BJ�BJ�Ba|Br�Bv�Bm�Bl"Bg8BaHBo�BuZBu�Bt�Bs�Bn�Bu�Bp�Bm]B_pBaHBg8Ba�BaHBb�Bf�BbNBv+B�iB{Bv�Bv�BpoBo�Bp�Br|Bm�BjBh>BjKBh�Be,BsB[�BU�BN�BH�B?�BMBOvBOB	BBPBB�B�BMB�BB~B	�BG�BJ�B?HBr�Bs�Bs�Br|BtBsMBrGBsMBqBpBxBq�Bl�BkBiDBh�Bh�BgBe`Bb�Bc�Bd�BaBa�B_pB[#B]�BY�BZ�BOBBMjB@�B=B@OBA�B9$B4�BL0B&B"�B"hB$B#�B�BVB�BIB�B�B1B�B�BFB�B�B�B4B�BB
�lB
�fB
��B
�	B
�B
�AB
�/B
��B
�`B
ޞB
�B
�WB
�gB
֡B
�NB
�}B
��B
˒B
ĜB
��B
�B
�TB
�)B
��B
�qB
��B
��B
��B
�B
��B
�SB
�'B
�!B
��B
��B
��B
��B
��B
��B
��B
�GB
��B
�AB
cB
�B
{�B
�4B
|B
|�B
zB
~(B
��B
iyB
a�B
g�B
a�B
b�B
`B
cTB
gmB
d�B
a�B
`�B
[�B
`B
e`B
K^B
J�B
I�B
D�B
D�B
D3B
E�B
CaB
E�B
GzB
DgB
A B
9�B
:*B
8B
6�B
6FB
33B
2�B
5�B
1�B
1[B
-�B
D3B
0�B
&�B
$tB
 �B
VB
#B
VB
%�B
#nB
 �B
IB
�B
�B
�B
MB
{B
B
�B
�B
�B
AB	�rB	�lB	�>B	�B	��B	�MB	�B	�mB	�B	��B	�"B	��B	�iB	��B	�>B	�B	��B	�sB	�B	� B	�B	��B	ɆB	��B	�BB	��B	�qB	��B	��B	�RB	�B	�aB	��B	��B	��B	��B	�OB	�$B	��B	��B	�zB	��B	�MB	�JB	��B	�fB	�+B	�~B	��B	��B	�4B	.B	�AB	}�B	y�B	u�B	xlB	v�B	v�B	sB	yrB	oiB	qvB	p�B	|�B	f2B	d�B	c B	^jB	^�B	[�B	YKB	X�B	YKB	T�B	ZQB	VB	Y�B	T�B	Q�B	P}B	M�B	sB	^5B	[�B	P�B	E�B	LdB	Q�B	J�B	J�B	M6B	E�B	H�B	A B	B�B	B�B	@�B	:�B	:^B	9$B	33B	4B	?�B	S�B	)�B	0UB	)�B	"hB	B��B�B�lB��B��B	�B�VB	�B	 �B	B	B	 �B	  B	;B	�B	�B		�B	MB	�B	B	�B	%B	�B	�B	�B	fB	�B	1B	
�B	JB	bB	�B	B	�B	�B	�B	�B	&B	5B	33B	1'B	/�B	/�B	/�B	-�B	.�B	.B	+�B	,=B	-�B	+B	)*B	($B	(XB	%FB	%FB	%zB	%�B	$B	$B	!�B	3hB	 �B	VB	�B	eB	�B	SB	GB��B	�B�VB�`B�AB�B��B�WB��B��B��B��B��B�QB�B��B�ZB�&B�B��B�B�TB��B�NB�ZB�2B�&B�WB��B�2G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                    B	Y�B	X�B	YB	YTB	X�B	YTB	YB	YB	YTB	X�B	YB	X�B	YTB	YTB	Y�B	Y�B	YTB	Y�B	Y�B	Y�B	YTB	YTB	Y�B	Y�B	YTB	YTB	YTB	YTB	Y�B	Y�B	Y�B	Y�B	YTB	YB	YTB	YTB	YB	X�B	Y�B	Y�B	Z%B	Z�B	Z%B	Y�B	Y�B	YTB	X�B	Y�B	W�B	ūB
Z�B
��B*B8&B5HB1�B6NB5BW|Ba�Bk�B_yB]�BorBf�B`JBD�B�B9BiMBWGB%�BIB
кB
��B
��B
m�B
X�B
>B
/XB
!�B
�B	�_B	��B	�#B	�dB	��B	�=B	p�B	`�B	M�B	K�B	6�B	 dB��B�%B�eB	�B	'�B	"qB	�B	.B��B�yB�2B�B��B��B�B�B��B	*B	�B	@B�+B�rBާB�ZB۔B�AB�B��B��BوB�`B�lB߭B�B�B�B��B	�B	7B	7B	B	�B	�B	�B	[B	"�B	4�B	<
B	@#B	B/B	E�B	FHB	J`B	U;B	a�B	woB	��B	�$B	�B	�RB	�$B	��B	��B	��B	��B	��B	��B	�~B	�	B	�B	~�B	B	|�B	viB	q�B	pxB	l�B	k�B	k�B	iMB	h�B	i�B	jB	l�B	o�B	pB	o>B	`B	^�B	T�B	PQB	Q�B	V�B	\�B	fB	}�B	�[B	�B	�RB	�$B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�B	��B	�tB	�B	�-B	��B	�bB	��B	�nB	�B	�RB	��B	�RB	�^B	�<B	�qB	�B	�BB	�OB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�'B	�'B	��B	��B	�aB	��B	�3B	��B	�9B	��B	�B	��B	�B	�B	�B	��B	�XB	��B	��B	�B	�tB	��B	��B	��B	��B	��B	��B	�yB	��B	��B	�#B	��B	��B	��B	��B	�WB	��B	��B	��B	��B	�^B	��B	��B	��B	��B	��B	��B	B	�BB	ĤB	ĤB	��B	�|B	�B	ƱB	� B	ǷB	�`B	�>B	έB	�B	ЅB	��B	��B	�#B	�WB	��B	��B	��B	��B	�B	�;B	դB	�AB	֪B	�|B	�GB	�B	��B	��B	�B	װB	�B	��B	�GB	�NB	�B	؂B	��B	ٽB	�%B	�`B	��B	��B	��B	�ZB	�,B	�B	��B	�
B	��B	ݡB	ݡB	��B	�lB	ݡB	�
B	�lB	ݡB	�8B	�rB	��B	��B	�B	��B	�B	��B	��B	�B	�B	�B	�cB	��B	�.B	�B	��B	��B	� B	�iB	�B	�{B	�B	�{B	��B	�{B	�B	�B	�%B	�SB	�B	�B	�%B	��B	�`B	�7B	�7B	�	B	�DB	�B	�B	��B	�"B	�B	��B	�4B	��B	�4B	�oB	�uB	��B	�{B	�GB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 rB
�B
IB
IB
~B
PB
�B
PB
�B
�B
�B
bB
�B
B
B
�B
	@B
	tB
	�B
	tB
	�B

B

�B
B
�B
YB
YB
*B
�B
�B
�B
�B
0B
�B
�B
eB
kB
kB
�B
7B
�B
�B
IB
IB
}B
OB
�B
OB
�B
!B
�B
!B
!B
�B
[B
�B
bB
bB
bB
�B
�B
�B
�B
�B
B
�B
�B
@B
@B
�B
zB
zB
B
�B
FB
�B
zB
�B
B
B
B
�B
�B
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
*B
�B
 0B
�B
 0B
�B
*B
�B
 �B
!6B
!6B
!�B
!�B
!�B
!kB
!�B
!�B
"�B
#B
#�B
#�B
$B
$�B
$�B
$IB
$�B
$�B
$�B
%OB
%B
%�B
%�B
%�B
%�B
%�B
&UB
(�B
'�B
'�B
(�B
(�B
*9B
*�B
+?B
*�B
+B
+B
+B
+?B
*�B
+tB
+?B
+?B
+�B
,zB
-KB
,�B
-B
-�B
,�B
-�B
-KB
.�B
/#B
.�B
/�B
0^B
0�B
0�B
0�B
10B
0�B
0�B
2jB
4vB
4B
4vB
3�B
3�B
4B
3�B
4B
5�B
5�B
5�B
6B
5�B
6�B
6�B
6�B
7 B
6�B
7TB
7 B
7�B
6�B
7�B
7 B
7�B
6�B
7 B
6�B
7 B
9�B
:gB
:gB
:�B
;mB
:�B
;�B
;�B
<
B
<�B
;�B
=yB
=yB
=B
=�B
=�B
=yB
=yB
=EB
>�B
?QB
@WB
@�B
@WB
A�B
B/B
A�B
B/B
A�B
A^B
C5B
CjB
CB
CjB
EBB
DpB
D�B
E�B
EvB
E�B
FB
F�B
F�B
GB
GB
GNB
F�B
FHB
FHB
F|B
F�B
F�B
GNB
G�B
HTB
I&B
IZB
I�B
J,B
I�B
IZB
I�B
I�B
J,B
J�B
J�B
K2B
J�B
J�B
J�B
J�B
K�B
K2B
L8B
KgB
K�B
L8B
M�B
M>B
MsB
M>B
M�B
N�B
NEB
NyB
NyB
OKB
OKB
OB
PB
PQB
PQB
PQB
P�B
P�B
PQB
P�B
O�B
Q#B
QWB
QWB
R)B
Q�B
Q�B
R)B
Q�B
R]B
Q�B
R)B
R]B
Q�B
Q�B
R)B
Q�B
Q�B
R�B
S/B
R]B
T�B
T�B
UpB
U;B
U�B
U�B
VAB
U�B
VB
UpB
W�B
WB
V�B
WB
VvB
W|B
W|B
W|B
X�B
XB
YB
X�B
X�B
Y�B
Y�B
Y�B
Y�B
Z%B
Z�B
ZZB
Z�B
[�B
Z�B
[�B
[,B
[�B
\2B
\�B
]B
\�B
]B
\�B
\�B
]�B
^�B
^�B
^rB
^>B
^rB
^rB
^�B
_�B
`B
_�B
_�B
_�B
`B
aB
aB
`�B
`B
`�B
`B
a�B
a�B
b"B
bWB
a�B
b"B
b�B
c�B
c�B
d�B
eiB
e B
e�B
eiB
eiB
e5B
e�B
fB
f;B
f�B
gB
gB
guB
gB
guB
gAB
guB
gAB
g�B
hGB
h{B
h�B
h�B
iMB
i�B
i�B
i�B
i�B
jB
i�B
jB
j�B
j�B
k%B
k�B
k�B
k�B
k�B
k�B
l+B
l+B
l`B
l�B
l�B
l�B
l�B
l�B
m1B
n7B
n7B
n7B
n�B
o	B
o>B
orB
pB
pDB
pDB
pDB
pDB
pxB
p�B
p�B
p�B
pxB
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
s"B
s"B
s"B
sVB
sVB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t(B
u.B
u.B
ubB
u�B
u�B
u�B
u�B
v B
v B
v4B
viB
v�B
v�B
v�B
v�B
v�B
v�B
woB
w:B
w:B
w:B
xB
x@B
x@B
x@B
x@B
x@B
x@B
x@B
x@B
x�B
yB
yGB
yGB
y{B
yB
y�B
zB
zB
zMB
zMB
zMB
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|YB
|�B
}+B
|�B
}+B
}+B
|�B
}+B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
7B
kB
�B
�	B
�B
�	B
�=B
�rB
�rB
��B
�B
�CB
�CB
�xB
��B
��B
�B
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
��B
��B
�!B
��B
�VB
��B
��B
�VB
�VB
�VB
��B
��B
�.B
��B
��B
��B
��B
�.B
�.B
�bB
��B
�4B
�B
��B	T5B	[�B	ZZB	YB	YB	YB	X�B	WB	X�B	YTB	YTB	Y�B	Z%B	Y�B	Y�B	X�B	XNB	XB	X�B	Y�B	Y�B	Z%B	Y�B	YTB	X�B	X�B	X�B	XNB	X�B	Y�B	ZZB	ZZB	XNB	X�B	X�B	XNB	Y�B	Z%B	Z%B	Z%B	X�B	XNB	W�B	W�B	XNB	X�B	Y�B	Y�B	Y�B	YB	X�B	X�B	Y�B	ZZB	YTB	Y�B	X�B	W|B	Y�B	Z�B	[`B	Y�B	XNB	XNB	X�B	YTB	Y�B	ZZB	Y�B	X�B	X�B	X�B	Y�B	Y�B	Y�B	X�B	X�B	X�B	YTB	Z%B	Y�B	YB	X�B	X�B	YTB	Z%B	Z�B	Y�B	X�B	YTB	Y�B	Z�B	Z�B	Z%B	Y�B	YB	X�B	YB	Y�B	Z�B	Z%B	YB	X�B	X�B	YB	Y�B	ZZB	X�B	ZZB	YB	X�B	XB	X�B	X�B	Z%B	Z%B	Y�B	YB	X�B	XNB	YB	Y�B	Z�B	Z�B	ZZB	Y�B	X�B	X�B	X�B	Y�B	Z�B	Z�B	Z%B	YB	XNB	X�B	X�B	Y�B	Z%B	YTB	X�B	X�B	X�B	Y�B	ZZB	Y�B	X�B	X�B	X�B	Y�B	ZZB	Y�B	YB	XB	YB	X�B	Y�B	Z�B	Z�B	Z�B	Y�B	X�B	X�B	X�B	Y�B	Z�B	Z�B	Z�B	Y�B	X�B	X�B	Y�B	Z�B	Y�B	YTB	X�B	X�B	Y�B	ZZB	ZZB	YTB	YB	XB	X�B	YTB	ZZB	ZZB	ZZB	Y�B	YB	YB	W|B	X�B	XB	YTB	X�B	Y�B	ZZB	YTB	X�B	XNB	YTB	Z�B	ZZB	YTB	X�B	YB	XNB	X�B	X�B	YTB	ZZB	Y�B	YTB	YB	XB	Y�B	Y�B	Y�B	X�B	W�B	XB	X�B	Y�B	Y�B	YTB	X�B	X�B	Y�B	Y�B	ZZB	Y�B	X�B	X�B	Y�B	Z�B	Z�B	Z�B	Y�B	X�B	Y�B	Z�B	Z�B	Z�B	Y�B	YTB	X�B	[`B	[`B	Z�B	Y�B	Y�B	Z%B	Z�B	[�B	[,B	Z�B	Y�B	Y�B	ZZB	[,B	Z�B	ZZB	X�B	X�B	Y�B	Z�B	Z�B	Y�B	Z%B	X�B	X�B	YTB	Z�B	Z�B	Y�B	YB	YB	Z�B	Z�B	ZZB	X�B	X�B	YTB	Y�B	Y�B	W�B	W�B	X�B	X�B	XNB	W�B	XNB	X�B	Y�B	W�B	X�B	\2B	\2B	[�B	X�B	X�B	X�B	Y�B	YB	XB	Z�B	X�B	XNB	WB	TB	TiB	V�B	b�B	u�B	|%B	~1B	��B	ϴB
X�B
DB
L�B
<�B
1�B
=EB
V�B
�}B
�#B
ƱB
�cB*B
�B�B#�B�BLBhB@BtBB�B1�B6�B<�B9,B3<B26B4BB)3B9�B@WB(�B;BA)B(�B(�B!BR�B/#B)3BRB,zB5�BDpBB/BEvB#BB.�BE�B7�B<�BG�BCBCBY�Bj�Bo	Be�Bd.B_DBYTBg�BmfBm�Bl�Bk�Bf�Bm�Bh�BeiBW|BYTB_DBY�BYTBZ�B^�BZZBn7BxuBs�Bo	Bn�Bh{Bg�Bh�Bj�BfBb�B`JBbWB`�B]8Bk%BS�BM�BF�B@�B7�BEBG�B[BBB\B'B
��B
��B
�YB
��B!B�B�B?�BCB7TBj�Bk�Bk�Bj�Bl+BkYBjSBkYBiBhBpBi�Be Bc(BaPB`�B`�B_B]lBZ�B[�B]BYBY�BW|BS/BU�BQ�BR�BGNBEvB8�B5B8[B9�B10B,�BD<B$B�BtBB�B�BbB�BUB�BB=B�B
�BRB�B�B
�B	@BB
�B
�xB
�rB
��B
�B
�B
�MB
�;B
��B
�lB
֪B
ыB
�cB
�sB
έB
�ZB
ȉB
��B
ÞB
��B
��B
�B
�`B
�5B
��B
�}B
��B
�B
��B
�B
��B
�_B
�3B
�-B
�B
��B
��B
��B
��B
��B
�B
{SB
z�B
zMB
woB
{B
s�B
x@B
t(B
t�B
rB
v4B
}�B
a�B
Y�B
_�B
Y�B
Z�B
XB
[`B
_yB
]B
Y�B
X�B
S�B
XB
]lB
CjB
B�B
A�B
<�B
<�B
<?B
=�B
;mB
=�B
?�B
<sB
9,B
1�B
26B
0)B
.�B
.RB
+?B
*�B
-�B
*B
)gB
%�B
<?B
(�B
�B
�B
B
bB
B
bB
�B
zB
�B
UB
�B
B
B
YB
�B
$B
�B
�B
�B	�MB	�~B	�xB	�JB	�B	��B	�YB	߭B	�yB	�"B	��B	�.B	��B	�uB	��B	�JB	�B	��B	�B	��B	�,B	�B	��B	��B	��B	�NB	��B	�}B	�B	��B	�^B	�B	�mB	��B	��B	��B	��B	�[B	�0B	��B	��B	��B	��B	�YB	�VB	�B	�rB	7B	��B	}�B	}�B	x@B	w:B	zMB	u�B	q�B	m�B	pxB	n�B	n�B	k%B	q~B	guB	i�B	h�B	t�B	^>B	]B	[,B	VvB	V�B	S�B	QWB	P�B	QWB	L�B	R]B	NB	Q�B	M
B	I�B	H�B	E�B	k%B	VAB	S�B	H�B	=�B	DpB	I�B	B�B	B�B	EBB	=�B	@�B	9,B	:�B	:�B	8�B	2�B	2jB	10B	+?B	,B	7�B	LB	!�B	(aB	!�B	tB	B��B�B�xB��B�B��B�bB��B��B�B�B��B�B�GB��B��B	�B�YB��B�+B�B�1B��B��B��B	 rB��B	 =B	�B	VB	nB		�B	$B	�B	�B	�B	�B	$B	-B	+?B	)3B	'�B	'�B	'�B	%�B	&�B	& B	#�B	$IB	%�B	#B	!6B	 0B	 dB	RB	RB	�B	�B	B	B	�B	+tB	�B	bB	�B	qB	�B	_B�SB��B��B�bB�lB�MB�B��B�cB��B��B��B��B�B�]B�B��B�fB�2B�%B�
B��B�`B��B�ZB�fB�>B�2B�cB��B�>G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                    G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0077649                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0077649                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 23 11 2022 179 -0.0077649 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 23 11 2022 179 -0.0077649 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230426223230                            20230426223230AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023042622323020230426223230  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622323020230426223230QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F83E           783E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622323020230426223230QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               