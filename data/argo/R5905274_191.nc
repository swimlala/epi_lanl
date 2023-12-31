CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-04-26T22:32:37Z creation      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20230426223237  20230426223237  5905274 5905274 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO7315                            7315                            2B  2B  AA  SOLO_II                         SOLO_II                         8643                            8643                            SBE602 11Jan18                  SBE602 11Jan18                  853 853 @��f@��f11  @�UUV�@�UUV�@0J�	��B@0J�	��B�dF�1P���dF�1P��11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  ?�=q@�\@@  @}p�@�  @��R@�p�A   A��A   A,��AAG�A`��A\)A�\)A��A�  A���A�  A߮A�Q�B (�B�
B  BQ�B (�B((�B0(�B8  B@  BH(�BP(�BW�
B_�Bg�
Bp  Bx  B�
B�{B�(�B�  B�  B�{B�  B�(�B�(�B�{B�{B�  B��B��B�  B�{B��B��B��
B�  B�=qB�=qB�{B�{B��B��
B��
B��B�  B�  B�  B��
C   C
=C��C��C
=C
  C��C
=C
=C  C{C
=C  C
=C
=C��C   C"  C#��C&  C(
=C*  C,
=C.
=C0  C1��C3��C6
=C8
=C:
=C;��C>
=C@  CA��CC�CE�CH  CJ
=CK��CN  CP
=CR{CT{CV{CX  CY��C\  C^  C`
=Cb{Cd  Cf  Ch
=Cj  Cl  Cn{Cp
=Cr
=Ct  Cv  Cx
=Cz
=C{��C}��C��C���C���C���C���C���C�  C���C���C���C���C���C��C���C���C�  C�
=C���C���C�C�  C�  C�  C���C�  C�  C���C�  C�  C���C���C�  C���C���C���C���C���C�C�
=C�  C�C�
=C�C�C���C��C���C�  C�C�C�C�
=C�C�  C�C�C�  C�C�  C�  C�C�  C�  C���C���C�C�C���C�  C���C���C�  C���C���C�  C�C�C�  C���C���C�  C���C�  C�C�  C�  C�C�C�
=C�  C�C�  C�C�  C���C�  C�  C�
=C�\C�  C�  C�  C�  C�  C�  C�C�  C���C�  C�
=C�C�C�
=C�
=C���C���C�  C�  C���C�C�  C�  C�C�C�C���C���C�  D   D � DD��D�qD}qD  D� D��D}qD�qD� D�D� D�D��D�qD}qD	  D	z�D	�RD
}qD�D��D��D}qD  D� D  D�D�D��D�D��DD��D�Dz�D��Dz�D�RDz�D  D}qD�qD� D  Dz�D  D�D  D�=D  DxRD�qD�DD�D�D� D�D�D  D}qD�RD z�D!  D!}qD"  D"��D"�qD#xRD#��D$��D%  D%� D%�qD&� D'  D'� D(  D(� D(��D)�D*  D*}qD+  D+� D,  D,��D-�D-��D.  D.��D/�D/��D0D0� D0�RD1}qD1�qD2� D2��D3}qD4D4� D5D5��D6  D6}qD6�qD7��D7�qD8}qD9�D9�D:�D:� D:�qD;}qD;��D<z�D=  D=��D>�D>}qD>�qD?��D?�qD@z�DA�DA�DA�qDB}qDC�DC}qDD  DDz�DD��DE�DF  DF��DF�qDG}qDG��DHxRDI  DI�DI�qDJxRDJ��DK� DL  DL}qDM  DM��DN�DN��DO  DO� DP�DP� DQ�DQ�DR  DR}qDS  DS}qDT�DT�DU�DU��DU�qDVz�DW  DW� DW�qDX}qDY  DY��DZ  DZ� DZ�RD[z�D\�D\}qD\��D]��D^  D^z�D^�qD_}qD`�D`��Da�Da��Db�Db� Db�RDcz�Dd�Dd� Dd��De� Df�Df� Df�qDg��Dh  Dh��Di�Di� Dj�Dj� Dj�qDk� DlDl��Dl��Dmz�Dm�qDn}qDn�qDo}qDo�qDp}qDq�Dq� Dr�Dr�Ds  Ds}qDt  Dt}qDt��Duz�Dv  Dv}qDw  Dw�DxDx��Dy  Dyz�Dz  Dz�D{D{�D|�D|}qD|�qD}��D~D~��D�D�D�qD�>�D�� D���D��qD�@ D�~�D���D�HD�>�D�� D���D�HD�@ D�~�D�� D�HD�@ D�~�D��HD�  D�@ D�~�D��)D���D�AHD���D��HD�  D�AHD�~�D��qD���D�=qD�� D�� D���D�>�D�� D��HD�  D�@ D��HD��HD���D�AHD�� D���D�  D�=qD��HD���D�  D�=qD���D���D�  D�@ D���D���D�  D�@ D�~�D���D��)D�=qD�~�D��HD�  D�>�D�~�D�� D��D�AHD�� D�� D��qD�@ D�~�D�� D��qD�AHD���D�� D�HD�AHD�� D�� D�  D�AHD�� D�� D��D�@ D�� D�� D�  D�AHD���D�� D�HD�@ D�~�D���D��qD�>�D�~�D�� D���D�=qD�~�D�� D���D�>�D�~�D���D���D�AHD�� D���D�  D�@ D�� D��HD��D�B�D��HD���D�  D�>�D�� D��HD�  D�@ D�� D��HD�HD�@ D�~�D���D��qD�@ D��HD�� D���D�>�D�}qD�� D�  D�>�D��HD�� D�HD�AHD�� D�� D�  D�>�D�~�D���D���D�B�D�� D���D���D�>�D�� D�D�HD�>�D�� D��HD���D�@ D��HD��HD���D�=qD�� D��HD�HD�@ D�� D�� D�HD�AHD�� D��qD��qD�AHD��HD��HD���D�@ D�� D���D�HD�AHD���D�� D�  D�@ D��HD�� D�HD�@ D��HD�� D���D�>�D�� D���D�HD�>�D�� D��HD�HD�=qD��HD��HD��D�@ D��HD���D��qD�>�D�� D��HD�HD�@ D�~�D��HD�  D�@ D��HD�� D��qD�AHD��HD��HD�HD�>�D�� D���D�  D�@ D���D�� D���D�@ D�~�D���D��D�>�D���D�� D�HD�>�D��HD�� D�HD�>�D��HD��HD�  D�AHD�D�� D���D�AHD�}qD��HD���D�=qDāHD�� D���D�@ D�~�DŽqD��D�@ Dƀ D�� D���D�AHDǀ DǽqD�  D�AHDȂ�DȾ�D�  D�>�DɁHD�� D�  D�>�Dʀ Dʾ�D�  D�>�D˂�D��HD�  D�AHD�}qD�D���D�@ D�}qD��HD�HD�>�D΂�D�D�  D�=qD�}qD�� D�  D�>�D�}qDо�D���D�@ D�~�DѾ�D���D�@ DҁHD��HD�HD�@ DӀ DӾ�D��qD�>�D�~�DԾ�D�  D�@ D�~�D��HD��D�AHD�~�D־�D���D�=qD�~�D�� D�  D�>�D؀ Dؾ�D���D�>�D�~�Dپ�D���D�AHDځHD��HD�HD�AHDہHD�� D�  D�@ D�~�D�� D�  D�@ D݀ D��HD�  D�>�D�~�D�� D���D�AHD߀ D߾�D���D�>�D�~�DྸD�  D�AHD� DᾸD�HD�@ D� D⾸D���D�>�D�~�D�� D��qD�>�D� D�� D���D�@ D� D��HD�HD�@ D� D�D�  D�@ D� D�� D�  D�@ D�HD��HD�HD�>�D� D��HD�  D�AHD�HD��HD���D�AHD�HD뾸D���D�@ D�HD��HD���D�>�D� D�� D�  D�AHD� D�� D���D�>�D� DﾸD���D�@ D�~�D��HD�HD�@ D�HD��HD�  D�AHD�HD��HD�  D�B�D� D�D��qD�@ D�~�D��HD�  D�@ D���D�� D�  D�@ D�}qD���D�HD�=qD��HD�� D�HD�C�D���D���D���D�>�D�~�D��HD�HD�>�D�p�?L��?aG�?�=q?�33?�(�?�@�@
=@+�@5@G�@Tz�@c�
@p��@}p�@��@�{@�z�@�p�@��
@���@�33@�(�@\@�=q@�33@��H@�G�@���@��@���A   A�
A�A(�A\)Az�A�A�A{A"�\A&ffA*=qA.�RA2�\A7
=A:�HA@  AC33AG
=AK�AP  AR�\AW�A[�A^�RAa�AfffAi��An{Aq�Au�Ay��A}p�A���A��\A�(�A�ffA�Q�A��\A��
A�{A�  A��\A�(�A�ffA���A��\A���A�\)A���A��A�A�  A�=qA��
A�A�
=A���A�33A��A�\)A�G�A�33A��A��A���A��
A�A�  A��A�(�A�{A�Q�A��A�(�A�{A�Q�A�=qA�z�A�ffA��A��HA��A�RA�G�A�33A�p�A�\)A�A�A�A��A���A��
A�A��B ��B{B33BQ�B��B�RB�
B��B	�B
=B  B�B{B�HB�
B�B�B�HB�
B��B�B�HB(�BG�B�\B�B��BB�HB�B ��B!p�B"�RB#�B$��B%��B&�RB'�B(��B)�B*�HB,  B-p�B.�\B/�B0��B1��B2�HB3�B4z�B5��B6�RB7�B8��B9B;
=B<z�B=��B>�\B?�B@��BA�BB�\BC�BD��BEBF�RBG�
BIG�BJffBK�BL��BMBN�RBO�BPz�BQ��BR�\BS�BT��BV{BW33BXz�BYG�BZ{B[
=B\(�B]�B^=qB_33B`Q�Ba��Bb�RBd  Be�Bf{Bg
=BhQ�Bip�BjffBk33Bl  Bm�Bn=qBo33BpQ�Bqp�Br�RBt  Bu�Bv{Bw33Bx  Bx��By�Bz�HB|  B}G�B~�\B�B�Q�B��RB�33B��B�(�B��HB�p�B�{B��\B��B���B��B�z�B��HB��B�=qB���B�\)B��
B�ffB��RB�33B�B�Q�B���B�p�B�{B���B�33B��B�  B���B�\)B��
B�ffB���B�G�B��
B�Q�B��B��B�=qB���B�
=B���B�(�B��RB��B�  B���B���B��B�  B��\B�\)B��B�z�B���B�\)B��
B���B�33B���B�{B��\B��B��B�z�B���B�\)B��
B�ffB�33B�B�Q�B���B��B��B�(�B���B��B�{B�ffB��HB�p�B�{B��RB�G�B�B�(�B���B�33B���B�Q�B��HB�p�B�  B�z�B��HB�G�B�B�=qB��HB�p�B�  B�z�B���B�G�B�B�Q�B���B��B�{B�ffB��HB�G�B��
B�ffB�
=B��B�=qB��\B�
=B��B�=qB��HB�\)B��
B�=qBģ�B�33B��
BƏ\B�
=BǙ�B�(�Bȏ\B���B�p�B�{B��HB�\)B��B�=qB��HB�\)B�  BθRB�\)BϮB�(�BиRB�p�B�  Bҏ\B��HB�p�B��B�z�B�G�B��
B�(�BָRB�33B��
B�z�B��B�\)B��B�z�B���Bۙ�B�Q�B���B�33B�B�ffB��B�B�{B��B�G�B�  B��B�
=B�B��B�RB�G�B�B�=qB���B�B�{B��B���B�B�Q�B��HB�p�B�B�ffB��B�B�{B�RB�33B�{B�RB��B�B�Q�B�33B��
B�=qB��HB��B�=qB���B�p�B�=qB��HB�\)B�  B���B��B��B��\B�p�B�(�B��\B�33C {C ffC ��C �CffCC��CQ�CC(�C\)C�C33C�\CC�C�\C�HC33C�C��C=qC�\C  C\)C�\C�C	\)C	�RC	�C
=qC
C{CG�C��C�CffC�C
=Cz�C�
C{Cp�C�HC(�Cp�C�HCG�C�C�HCQ�C�\C�HCQ�C�\C��C=qC�\C�C��CG�Cz�C��C�
C{C�C=qC�C�C�C�
C  C{C(�CffC�C�\C��C�HC
=C{C33CffC�C�\C�RC�HC{C
=C33Cp�C�\C��C�RC  C�C(�CQ�C�C��C�RC��C�C33CG�C��CCC��C33C33C\)C��CC�
C  C33CQ�Cp�C�RC�HC�C
=CQ�Cz�C�\C�RC�C  C�CffC�C��C�
C
=C{C=qCz�C�\C�C�C {C (�C \)C �C ��C �C ��C!{C!(�C!\)C!�\C!��C!C"
=C"(�C"=qC"z�C"��C"�C"�HC#{C#�C#Q�C#�C#�\C#��C#��C$  C$�C$ffC$p�C$�\C$�
C$�HC%�C%Q�C%\)C%�C%C%��C&  C&33C&G�C&ffC&�C&�RC&�C'�C'33C'ffC'��C'��C'��C(
=C(�C(G�C(�C(�\C(C(��C)  C)�C)\)C)p�C)�\C)�
C)�C*  C*G�C*\)C*z�C*C*��C+  C+=qC+Q�C+z�C+�RC+��C+�C,=qC,Q�C,p�C,C,�C,��C-=qC-\)C-z�C-C-�
C.{C.Q�C.ffC.�C.��C.�C/33C/ffC/z�C/C/�HC0  C0G�C0ffC0�C0�
C0�HC1{C1\)C1ffC1�C1�HC1�C2=qC2ffC2p�C2C2�HC3
=C3Q�C3ffC3�C3�
C3�C4=qC4G�C4��C4�C4�
C5�C533C5z�C5�C5C6{C6�C6\)C6��C6�RC7  C7{C7G�C7�\C7�C7��C8
=C8=qC8�\C8��C8�C9  C9=qC9z�C9�\C9�
C9�C:�C:p�C:z�C:C:�C;  C;Q�C;ffC;�C;��C;��C<G�C<\)C<��C<�RC<�C=(�C=G�C=�\C=�C=�C>
=C>G�C>p�C>��C>�
C>�C?=qC?\)C?��C?�C?�C@(�C@=qC@�C@��C@��CA{CA(�CAp�CA�CACB  CB{CB\)CBp�CB�CB�CC  CCQ�CCffCC�CC��CC��CDG�CDQ�CD�CD�CE  CE�CEffCEz�CECE�HCF�CF\)CFp�CFCF�HCG(�CG=qCGz�CG�CG��CH(�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111114111111111111111111411411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111                                                                                                                                                            ?�=q@�\@@  @}p�@�  @��R@�p�A   A��A   A,��AAG�A`��A\)A�\)A��A�  A���A�  A߮A�Q�B (�B�
B  BQ�B (�B((�B0(�B8  B@  BH(�BP(�BW�
B_�Bg�
Bp  Bx  B�
B�{B�(�B�  B�  B�{B�  B�(�B�(�B�{B�{B�  B��B��B�  B�{B��B��B��
B�  B�=qB�=qB�{B�{B��B��
B��
B��B�  B�  B�  B��
C   C
=C��C��C
=C
  C��C
=C
=C  C{C
=C  C
=C
=C��C   C"  C#��C&  C(
=C*  C,
=C.
=C0  C1��C3��C6
=C8
=C:
=C;��C>
=C@  CA��CC�CE�CH  CJ
=CK��CN  CP
=CR{CT{CV{CX  CY��C\  C^  C`
=Cb{Cd  Cf  Ch
=Cj  Cl  Cn{Cp
=Cr
=Ct  Cv  Cx
=Cz
=C{��C}��C��C���C���C���C���C���C�  C���C���C���C���C���C��C���C���C�  C�
=C���C���C�C�  C�  C�  C���C�  C�  C���C�  C�  C���C���C�  C���C���C���C���C���C�C�
=C�  C�C�
=C�C�C���C��C���C�  C�C�C�C�
=C�C�  C�C�C�  C�C�  C�  C�C�  C�  C���C���C�C�C���C�  C���C���C�  C���C���C�  C�C�C�  C���C���C�  C���C�  C�C�  C�  C�C�C�
=C�  C�C�  C�C�  C���C�  C�  C�
=C�\C�  C�  C�  C�  C�  C�  C�C�  C���C�  C�
=C�C�C�
=C�
=C���C���C�  C�  C���C�C�  C�  C�C�C�C���C���C�  D   D � DD��D�qD}qD  D� D��D}qD�qD� D�D� D�D��D�qD}qD	  D	z�D	�RD
}qD�D��D��D}qD  D� D  D�D�D��D�D��DD��D�Dz�D��Dz�D�RDz�D  D}qD�qD� D  Dz�D  D�D  D�=D  DxRD�qD�DD�D�D� D�D�D  D}qD�RD z�D!  D!}qD"  D"��D"�qD#xRD#��D$��D%  D%� D%�qD&� D'  D'� D(  D(� D(��D)�D*  D*}qD+  D+� D,  D,��D-�D-��D.  D.��D/�D/��D0D0� D0�RD1}qD1�qD2� D2��D3}qD4D4� D5D5��D6  D6}qD6�qD7��D7�qD8}qD9�D9�D:�D:� D:�qD;}qD;��D<z�D=  D=��D>�D>}qD>�qD?��D?�qD@z�DA�DA�DA�qDB}qDC�DC}qDD  DDz�DD��DE�DF  DF��DF�qDG}qDG��DHxRDI  DI�DI�qDJxRDJ��DK� DL  DL}qDM  DM��DN�DN��DO  DO� DP�DP� DQ�DQ�DR  DR}qDS  DS}qDT�DT�DU�DU��DU�qDVz�DW  DW� DW�qDX}qDY  DY��DZ  DZ� DZ�RD[z�D\�D\}qD\��D]��D^  D^z�D^�qD_}qD`�D`��Da�Da��Db�Db� Db�RDcz�Dd�Dd� Dd��De� Df�Df� Df�qDg��Dh  Dh��Di�Di� Dj�Dj� Dj�qDk� DlDl��Dl��Dmz�Dm�qDn}qDn�qDo}qDo�qDp}qDq�Dq� Dr�Dr�Ds  Ds}qDt  Dt}qDt��Duz�Dv  Dv}qDw  Dw�DxDx��Dy  Dyz�Dz  Dz�D{D{�D|�D|}qD|�qD}��D~D~��D�D�D�qD�>�D�� D���D��qD�@ D�~�D���D�HD�>�D�� D���D�HD�@ D�~�D�� D�HD�@ D�~�D��HD�  D�@ D�~�D��)D���D�AHD���D��HD�  D�AHD�~�D��qD���D�=qD�� D�� D���D�>�D�� D��HD�  D�@ D��HD��HD���D�AHD�� D���D�  D�=qD��HD���D�  D�=qD���D���D�  D�@ D���D���D�  D�@ D�~�D���D��)D�=qD�~�D��HD�  D�>�D�~�D�� D��D�AHD�� D�� D��qD�@ D�~�D�� D��qD�AHD���D�� D�HD�AHD�� D�� D�  D�AHD�� D�� D��D�@ D�� D�� D�  D�AHD���D�� D�HD�@ D�~�D���D��qD�>�D�~�D�� D���D�=qD�~�D�� D���D�>�D�~�D���D���D�AHD�� D���D�  D�@ D�� D��HD��D�B�D��HD���D�  D�>�D�� D��HD�  D�@ D�� D��HD�HD�@ D�~�D���D��qD�@ D��HD�� D���D�>�D�}qD�� D�  D�>�D��HD�� D�HD�AHD�� D�� D�  D�>�D�~�D���D���D�B�D�� D���D���D�>�D�� D�D�HD�>�D�� D��HD���D�@ D��HD��HD���D�=qD�� D��HD�HD�@ D�� D�� D�HD�AHD�� D��qD��qD�AHD��HD��HD���D�@ D�� D���D�HD�AHD���D�� D�  D�@ D��HD�� D�HD�@ D��HD�� D���D�>�D�� D���D�HD�>�D�� D��HD�HD�=qD��HD��HD��D�@ D��HD���D��qD�>�D�� D��HD�HD�@ D�~�D��HD�  D�@ D��HD�� D��qD�AHD��HD��HD�HD�>�D�� D���D�  D�@ D���D�� D���D�@ D�~�D���D��D�>�D���D�� D�HD�>�D��HD�� D�HD�>�D��HD��HD�  D�AHD�D�� D���D�AHD�}qD��HD���D�=qDāHD�� D���D�@ D�~�DŽqD��D�@ Dƀ D�� D���D�AHDǀ DǽqD�  D�AHDȂ�DȾ�D�  D�>�DɁHD�� D�  D�>�Dʀ Dʾ�D�  D�>�D˂�D��HD�  D�AHD�}qD�D���D�@ D�}qD��HD�HD�>�D΂�D�D�  D�=qD�}qD�� D�  D�>�D�}qDо�D���D�@ D�~�DѾ�D���D�@ DҁHD��HD�HD�@ DӀ DӾ�D��qD�>�D�~�DԾ�D�  D�@ D�~�D��HD��D�AHD�~�D־�D���D�=qD�~�D�� D�  D�>�D؀ Dؾ�D���D�>�D�~�Dپ�D���D�AHDځHD��HD�HD�AHDہHD�� D�  D�@ D�~�D�� D�  D�@ D݀ D��HD�  D�>�D�~�D�� D���D�AHD߀ D߾�D���D�>�D�~�DྸD�  D�AHD� DᾸD�HD�@ D� D⾸D���D�>�D�~�D�� D��qD�>�D� D�� D���D�@ D� D��HD�HD�@ D� D�D�  D�@ D� D�� D�  D�@ D�HD��HD�HD�>�D� D��HD�  D�AHD�HD��HD���D�AHD�HD뾸D���D�@ D�HD��HD���D�>�D� D�� D�  D�AHD� D�� D���D�>�D� DﾸD���D�@ D�~�D��HD�HD�@ D�HD��HD�  D�AHD�HD��HD�  D�B�D� D�D��qD�@ D�~�D��HD�  D�@ D���D�� D�  D�@ D�}qD���D�HD�=qD��HD�� D�HD�C�D���D���D���D�>�D�~�D��HD�HD�>�D�p�?L��?aG�?�=q?�33?�(�?�@�@
=@+�@5@G�@Tz�@c�
@p��@}p�@��@�{@�z�@�p�@��
@���@�33@�(�@\@�=q@�33@��H@�G�@���@��@���A   A�
A�A(�A\)Az�A�A�A{A"�\A&ffA*=qA.�RA2�\A7
=A:�HA@  AC33AG
=AK�AP  AR�\AW�A[�A^�RAa�AfffAi��An{Aq�Au�Ay��A}p�A���A��\A�(�A�ffA�Q�A��\A��
A�{A�  A��\A�(�A�ffA���A��\A���A�\)A���A��A�A�  A�=qA��
A�A�
=A���A�33A��A�\)A�G�A�33A��A��A���A��
A�A�  A��A�(�A�{A�Q�A��A�(�A�{A�Q�A�=qA�z�A�ffA��A��HA��A�RA�G�A�33A�p�A�\)A�A�A�A��A���A��
A�A��B ��B{B33BQ�B��B�RB�
B��B	�B
=B  B�B{B�HB�
B�B�B�HB�
B��B�B�HB(�BG�B�\B�B��BB�HB�B ��B!p�B"�RB#�B$��B%��B&�RB'�B(��B)�B*�HB,  B-p�B.�\B/�B0��B1��B2�HB3�B4z�B5��B6�RB7�B8��B9B;
=B<z�B=��B>�\B?�B@��BA�BB�\BC�BD��BEBF�RBG�
BIG�BJffBK�BL��BMBN�RBO�BPz�BQ��BR�\BS�BT��BV{BW33BXz�BYG�BZ{B[
=B\(�B]�B^=qB_33B`Q�Ba��Bb�RBd  Be�Bf{Bg
=BhQ�Bip�BjffBk33Bl  Bm�Bn=qBo33BpQ�Bqp�Br�RBt  Bu�Bv{Bw33Bx  Bx��By�Bz�HB|  B}G�B~�\B�B�Q�B��RB�33B��B�(�B��HB�p�B�{B��\B��B���B��B�z�B��HB��B�=qB���B�\)B��
B�ffB��RB�33B�B�Q�B���B�p�B�{B���B�33B��B�  B���B�\)B��
B�ffB���B�G�B��
B�Q�B��B��B�=qB���B�
=B���B�(�B��RB��B�  B���B���B��B�  B��\B�\)B��B�z�B���B�\)B��
B���B�33B���B�{B��\B��B��B�z�B���B�\)B��
B�ffB�33B�B�Q�B���B��B��B�(�B���B��B�{B�ffB��HB�p�B�{B��RB�G�B�B�(�B���B�33B���B�Q�B��HB�p�B�  B�z�B��HB�G�B�B�=qB��HB�p�B�  B�z�B���B�G�B�B�Q�B���B��B�{B�ffB��HB�G�B��
B�ffB�
=B��B�=qB��\B�
=B��B�=qB��HB�\)B��
B�=qBģ�B�33B��
BƏ\B�
=BǙ�B�(�Bȏ\B���B�p�B�{B��HB�\)B��B�=qB��HB�\)B�  BθRB�\)BϮB�(�BиRB�p�B�  Bҏ\B��HB�p�B��B�z�B�G�B��
B�(�BָRB�33B��
B�z�B��B�\)B��B�z�B���Bۙ�B�Q�B���B�33B�B�ffB��B�B�{B��B�G�B�  B��B�
=B�B��B�RB�G�B�B�=qB���B�B�{B��B���B�B�Q�B��HB�p�B�B�ffB��B�B�{B�RB�33B�{B�RB��B�B�Q�B�33B��
B�=qB��HB��B�=qB���B�p�B�=qB��HB�\)B�  B���B��B��B��\B�p�B�(�B��\B�33C {C ffC ��C �CffCC��CQ�CC(�C\)C�C33C�\CC�C�\C�HC33C�C��C=qC�\C  C\)C�\C�C	\)C	�RC	�C
=qC
C{CG�C��C�CffC�C
=Cz�C�
C{Cp�C�HC(�Cp�C�HCG�C�C�HCQ�C�\C�HCQ�C�\C��C=qC�\C�C��CG�Cz�C��C�
C{C�C=qC�C�C�C�
C  C{C(�CffC�C�\C��C�HC
=C{C33CffC�C�\C�RC�HC{C
=C33Cp�C�\C��C�RC  C�C(�CQ�C�C��C�RC��C�C33CG�C��CCC��C33C33C\)C��CC�
C  C33CQ�Cp�C�RC�HC�C
=CQ�Cz�C�\C�RC�C  C�CffC�C��C�
C
=C{C=qCz�C�\C�C�C {C (�C \)C �C ��C �C ��C!{C!(�C!\)C!�\C!��C!C"
=C"(�C"=qC"z�C"��C"�C"�HC#{C#�C#Q�C#�C#�\C#��C#��C$  C$�C$ffC$p�C$�\C$�
C$�HC%�C%Q�C%\)C%�C%C%��C&  C&33C&G�C&ffC&�C&�RC&�C'�C'33C'ffC'��C'��C'��C(
=C(�C(G�C(�C(�\C(C(��C)  C)�C)\)C)p�C)�\C)�
C)�C*  C*G�C*\)C*z�C*C*��C+  C+=qC+Q�C+z�C+�RC+��C+�C,=qC,Q�C,p�C,C,�C,��C-=qC-\)C-z�C-C-�
C.{C.Q�C.ffC.�C.��C.�C/33C/ffC/z�C/C/�HC0  C0G�C0ffC0�C0�
C0�HC1{C1\)C1ffC1�C1�HC1�C2=qC2ffC2p�C2C2�HC3
=C3Q�C3ffC3�C3�
C3�C4=qC4G�C4��C4�C4�
C5�C533C5z�C5�C5C6{C6�C6\)C6��C6�RC7  C7{C7G�C7�\C7�C7��C8
=C8=qC8�\C8��C8�C9  C9=qC9z�C9�\C9�
C9�C:�C:p�C:z�C:C:�C;  C;Q�C;ffC;�C;��C;��C<G�C<\)C<��C<�RC<�C=(�C=G�C=�\C=�C=�C>
=C>G�C>p�C>��C>�
C>�C?=qC?\)C?��C?�C?�C@(�C@=qC@�C@��C@��CA{CA(�CAp�CA�CACB  CB{CB\)CBp�CB�CB�CC  CCQ�CCffCC�CC��CC��CDG�CDQ�CD�CD�CE  CE�CEffCEz�CECE�HCF�CF\)CFp�CFCF�HCG(�CG=qCGz�CG�CG��CH(�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111114111111111111111111411411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111                                                                                                                                                            G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ffA�^5A�bNA�^5A�^5A�ffA�bNA�Q�A�I�A�/A��A��A���AҼjAҸRAҧ�A҃A�r�A�l�A�hsA�hsA�ffA�ffA�`BA�ZA�ZA�VA�S�A�O�A�G�A�?}A�9XA�1'A�
=A��A��;A���A��A�
=A�^5AБhAХ�AУ�AН�AЛ�AЉ7A�t�A�M�A�JAϑhA�;dA���A΅A�z�A�33A��Aȇ+AǑhA��A��Aĥ�A�ȴA��A���A�%A���A��uA��A�l�A���A�7LA�A�A��\A�K�A�?}A��mA�`BA�  A�x�A���A�C�A�z�A���A��jA��`A�\)A�VA�E�A��mA�x�A��A�n�A��A��!A�dZA�dZA��9A��#A��wA��yA���A�33A��+A��FA�$�A�;dA���A~bAzĜAw
=Ar�Ar5?Aq�Ao�Ak��Aj5?AhbAc�AZȴAW�hAR^5AP��AO;dANAMXALȴAH��AC�;A@bNA<E�A8~�A5x�A3�A0�/A/��A-K�A,��A,A�A)�#A(1A'`BA&�A%�TA%oA$(�A#�PA"�A��A��A�AZA��Ap�A��A33A��A�hA��AZAE�A��A%A�A�FA��A �A�-A��A��A�AG�A
�!A
1A	��A	�A��A	�A�uA^5AE�A+A��A�;A;dA��AbNAC�A �!A 1'@��P@��@�bN@��@��m@�+@��+@�M�@�=q@���@�X@�ƨ@�C�@���@�-@��h@��u@�t�@�7@�?}@�&�@�%@��@�@���@���@�%@웦@띲@��y@��@�z�@���@�-@�&�@㝲@���@�`B@��/@���@ާ�@��@��/@܃@�r�@ܣ�@�A�@��@���@��@�Z@�r�@�/@�p�@ܬ@�9X@ܛ�@ݺ^@�{@�
=@ߕ�@�S�@��T@�@ݺ^@ݑh@ܴ9@��m@ڏ\@�ff@�=q@�^5@�7L@�  @׾w@��@�E�@���@� �@ӶF@ҸR@�{@��@щ7@Гu@�Z@��;@�1@�b@�t�@�C�@�C�@Ο�@�v�@�V@��@�^5@�=q@���@�/@�I�@���@˝�@�K�@�@�~�@�x�@�V@ȃ@�A�@�A�@�A�@�1@Ǯ@�;d@Ƈ+@��T@�hs@Ĵ9@���@�"�@���@�@�~�@�5?@�@���@�O�@��/@���@�Z@�Q�@�9X@�  @��m@��P@�"�@�@���@���@���@��R@�~�@���@�O�@�/@��@�G�@��`@�bN@�Z@��m@�;d@�o@��R@�n�@�M�@�{@���@��D@�1@���@��@��@���@��@�-@��@��#@�X@�V@���@��;@�K�@���@�M�@�x�@��9@� �@�  @��F@�C�@��H@���@�-@��@��@�x�@��@�x�@�hs@�O�@�7L@�%@�Ĝ@���@���@��D@�1'@���@��
@���@���@���@���@��P@�K�@�@�
=@�@��@�v�@�=q@��T@�`B@�/@��@�Z@���@��P@��!@�ff@�V@�5?@�J@���@��@�`B@�?}@��@��@���@��@���@�I�@��m@�K�@��y@�~�@���@�&�@�%@�r�@�I�@� �@��@�ƨ@��F@��@��@���@�ff@�=q@�$�@�@�O�@�%@��@�Q�@��F@�dZ@�\)@�@���@��+@�M�@�{@��T@��^@��7@�7L@��u@�(�@���@�E�@���@�hs@�r�@��m@��@��@�|�@�C�@���@���@�ff@��@�@��@�X@�?}@�7L@�V@��@�I�@��m@��P@�+@��@���@�ff@��@�@���@��7@�x�@���@�Z@�Q�@�A�@��@��@�l�@�+@���@��H@���@��!@���@��\@�M�@��@���@��7@�hs@�/@�%@��/@��/@���@��@�j@�9X@�(�@���@��y@��R@��\@�v�@�^5@�E�@�-@�{@���@�@�{@�$�@���@��@���@�G�@���@��9@���@��D@��@�z�@�j@�bN@�A�@�1'@�(�@��@��@�33@��H@���@���@�ff@�^5@�E�@�J@�J@���@��h@��@�x�@�&�@�%@�Ĝ@�bN@�I�@�9X@��@K�@~ȴ@}��@|�@|1@{33@zJ@y�^@y��@y�@x�9@x �@x  @w�;@w��@v�@vv�@v{@u�h@tz�@s��@s33@r��@r�\@rM�@q�^@q%@p��@pb@o;d@n�+@n5?@n{@m�@m��@m/@m�@l�/@lz�@lI�@l1@k��@j�@j~�@jn�@jn�@j�@i�@i�^@i�7@i7L@h�9@hQ�@h1'@h1'@hb@g�w@gK�@f��@fȴ@f�+@fE�@e�T@e��@d��@d�j@d��@dj@c�m@cC�@c@c33@co@b�@bn�@bJ@a�@a�7@aX@a�@`��@`�u@`r�@`Q�@_K�@^v�@]�@]�-@]�@\�@\j@\j@\�@[�m@[��@[dZ@[o@Z��@Z-@Yx�@X��@XbN@X �@W�@W�@Vv�@U�@U�@T�@T�@Tz�@TI�@T(�@T(�@Sƨ@SC�@S@R��@R�!@R��@Rn�@R-@Q��@QG�@P��@P�9@PA�@O�@O�;@O�;@O�P@N�y@N�+@N@M@M`B@M�@L��@L��@K��@J��@J=q@I�#@I�7@I&�@H��@H�`@H�`@H��@H�u@G�@G�@Fv�@E�@Ep�@EO�@E�@DZ@C�m@Cƨ@C�@CC�@Co@B�H@B-@A�#@A�^@A��@A&�@@1'@?K�@>�R@>�+@>v�@>E�@>{@=�@=��@=O�@=/@<��@<�j@<j@;��@;�F@;��@;t�@:�@:��@:�\@:~�@:~�@:n�@:n�@:=q@9��@9��@9X@8��@8Ĝ@8r�@8Q�@8b@7�w@7K�@7�@6ff@5��@5�h@5�@5?}@4�@41@3�@2�@2^5@2=q@1��@1�^@1��@1�7@1&�@0��@0bN@01'@0  @/��@/��@/|�@/;d@.�y@.{@-�@-�@-V@,�@,j@+��@+��@+33@*�@*��@*=q@)��@)hs@)7L@)7L@)&�@)�@(�`@(�`@(bN@(b@'�@'��@'\)@'�@'
=@&��@&�y@&�y@&��@&��@&V@&@%�T@%�-@%�@%�@%O�@$�@#�F@#dZ@#"�@#@#o@"�@"�@"�H@"�H@"~�@!X@!%@ �9@ �@ A�@ b@�w@��@l�@\)@+@+@;d@
=@
=@
=@��@ȴ@��@V@�@�@�@�@p�@p�@`B@O�@V@��@��@Z@��@�@dZ@S�@S�@S�@C�@33@33@33@~�@J@J@�@�@�@�@�@�^@�7@X@7L@&�@�`@�u@b@��@�P@�P@�P@��@��@�P@�P@�P@�P@l�@�@�@V@{@@�@`B@?}@�@�@�@�D@j@I�@1@��@�m@�F@�F@t�@33@@��@��@n�@=q@-@��@x�@hs@G�@7L@7L@�@��@��@�u@ �@�;@�@��@|�@\)@\)@\)@K�@K�@;d@+@;d@+@+@+@+@+@�@�@ff@�@��@��@�h@�h@��@�-@�-@�-@��@�h@�h@p�@`B@��@z�@j@Z@I�@Z@Z@Z@Z@Z@Z@9X@(�A�dZA�hsA�dZA�bNA�XA�\)A�\)A�^5A�^5A�bNA�`BA�bNA�dZA�dZA�dZA�Q�A�S�A�^5A�bNA�`BA�ffA�dZA�ffA�jA�jA�jA�bNA�bNA�VA�bNA�\)A�M�A�O�A�K�A�G�A�E�A�K�A�C�A�;dA�1'A�&�A�$�A��A��A�{A� �A��A��`A���A���A��
A��TA��
A��#A��A�A�A�ȴA�ĜA�ĜA�ĜAҾwAҾwAҼjAҼjAҼjAҾwAҼjA�ĜA���A�ĜAҸRAҸRAҶFAҲ-Aҩ�AҲ-AҴ9AҴ9AҰ!AҮAҝ�AғuAҋDA҉7A҅A҇+A҉7A҅AҁA�z�A�z�A�v�A�v�A�v�A�v�A�v�A�t�A�p�A�p�A�p�A�p�A�p�A�p�A�n�A�n�A�l�A�l�A�jA�jA�jA�jA�jA�jA�jA�jA�jA�jA�jA�jA�jA�jA�hsA�hsA�hsA�hsA�hsA�hsA�hsA�hsA�ffA�ffA�dZA�dZA�dZA�dZA�bNA�dZA�`BA�^5A�`BA�^5A�^5A�\)A�\)A�^5A�^5A�\)A�^5A�\)A�\)A�ZA�XA�XA�XA�XA�XA�ZA�ZA�ZA�ZA�ZA�VA�XA�XA�S�A�VA�VA�VA�S�A�Q�A�Q�A�Q�A�S�A�S�A�VA�S�A�S�A�S�A�S�A�S�A�O�A�O�A�O�A�M�A�I�A�G�A�A�A�?}A�A�A�A�A�C�A�A�A�A�A�A�A�?}A�=qA�9XA�7LA�7LA�9XA�9XA�;dA�;dA�;dA�7LA�5?A�1'A�/A�-A�/A�/A�/A�/A�+A�$�A� �A��A���AѲ-AѓuA�S�A�C�A�/A��TAд9AЃA�7LA���A��A��mA��;A��/A���A���A���A���A���A���A���A���A���A��
A��#A��HA��TA��A���A�  A�A�1A�
=A�1A�1A�1A�A�A�
=A��A�$�A�=qA�VA�hsA�r�A�|�AЃAЅAЋDAЏ\AГuAЕ�AЕ�AЗ�AЙ�AС�AХ�AЧ�AЩ�AЩ�AЧ�AХ�AЧ�AЩ�AЩ�AХ�AУ�AС�AП�AП�AС�AУ�AС�AЛ�AЛ�AН�AЛ�AЛ�AП�AС�AС�AН�AЗ�AГuAЏ\AЍPAЋDAЉ7AЅAЃAЃAЅAЃA�|�A�t�A�n�A�n�A�jA�ffA�dZA�XA�O�A�I�A�A�A�=qA�=qA�;dA�7LA�+A��A�  A��`A�ĜAϮAϬAϡ�Aϗ�AϋDAρA�v�A�p�A�jA�`BA�S�A�K�A�/A�"�A�1A��A��yA��TA��
A���A�ƨA�ĜA�ĜA�AθRAάAΛ�A΍PA�x�A�ffA�\)A�C�A��#A͝�ÁA�`BA�K�A�E�A�33A��ȂhA�x�A�5?A��A˼jA˸RA˰!A˕�A˅A�O�A��A���Aʙ�A�`BA�ƨA���AȋDA�M�A�$�A��A�A��A�ƨAǲ-Aǟ�Aǡ�AǋDA�ffA�E�A�9XA�33A�{A���A��`A�AƗ�A�G�A��A���A��;A��
A���A���A�ƨA�x�A�;dA�{AĶFA�1'A��yA�^5A�=qA�oA��#A���A�~�A�ZA�  A�ȴA��A�S�A��HA��uA�t�A�1'A���A�S�A�C�A�A�A�-A�9XA��A���A�bNA�5?A��A��A���A�S�A��A���A���A�=qA���A��RA�JA��jA���A��uA��\A�|�A�r�A�`BA�VA�/A��A��-A�bNA�"�A��jA�S�A�E�A�1'A� �A�oA���A�Q�A��A���A�Q�A��^A��PA�hsA�C�A�1A��
A��FA��7A�K�A��A�1A��#A��uA��DA��PA��\A��PA��A�r�A�jA�\)A�K�A�1'A��TA���A�x�A�K�A�bA�ĜA��!A��7A��A���A��DA�/A��
A���A��A�ffA�$�A��A���A���A�O�A�%A�=qA��A�ĜA���A�l�A�I�A�K�A�K�A�A�A�A�A�A�A�7LA���A��9A��A���A���A��A��A�r�A�hsA�hsA�dZA�VA�O�A�=qA�/A�A�bA�A��yA��TA��;A��
A���A��-A��A���A��A�|�A�n�A�A�A�5?A�1'A�1'A�-A�-A��A���A�hsA�VA��;A��-A�v�A��A�5?A���A�33A�z�A�&�A���A��A��A��A��`A��TA��;A��#A��wA���A��hA�x�A�hsA�G�A�"�A��A�VA���A��A��`A��;A��
A���A�ƨA��^A���A���A��A��A�~�A�~�A�~�A�|�A�z�A�z�A�r�A�;dA�(�A�%A���A�ƨA��FA���A���A��uA��A�bNA�C�A�5?A�
=A���A���A��A���A���A��RA�x�A�ffA�`BA�A�A�=qA�"�A���A���A��A��
A��
A�A��A��DA�l�A�;dA��A��TA���A��FA���A�bNA��A��RA���A���A��uA��DA��+A��A�~�A�|�A�x�A�hsA�hsA�VA�C�A�G�A�7LA�+A��A��A���A���A��/A��!A���A�hsA�A��A�XA�5?A�oA���A�+A�v�A���A��HA���A���A�dZA�JA���A�|�A�ZA�K�A��
A�z�A�=qA�JA���A��;A���A���A��RA��9A��RA��9A��9A��FA��A��A���A���A���A��hA�~�A�|�A�|�A�x�A�v�A�l�A�ZA�A�A�&�A�1A��mA���A��FA���A��A�n�A�K�A�33A��A��A��#A��9A�I�A�VA��`A���A�~�A�bNA�bNA�`BA�`BA�XA�C�A�(�A��A�{A�
=A�%A��A��-A�dZA�=qA�-A�oA���A��A��;A���A��jA���A��hA�hsA�oA��A���A���A��wA���A�|�A�oA�n�A��PA�JA���A�Q�A�?}A�1'A�+A�$�A�"�A��A��A���A��A�jA�\)A�\)A�I�A�G�A�?}A�1'A��A�{A�JA�  A��A��/A���A�ĜA��-A��A��PA�K�A� �A�1A�%A�A���A���A���A��A��A��\A�hsA�S�A�7LA�+A�oA��A��HA��9A�l�A�bA�ƨA�bNA�A�+A��yA�ĜA���A�|�A�O�A�"�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                            A�ffA�^5A�bNA�^5A�^5A�ffA�bNA�Q�A�I�A�/A��A��A���AҼjAҸRAҧ�A҃A�r�A�l�A�hsA�hsA�ffA�ffA�`BA�ZA�ZA�VA�S�A�O�A�G�A�?}A�9XA�1'A�
=A��A��;A���A��A�
=A�^5AБhAХ�AУ�AН�AЛ�AЉ7A�t�A�M�A�JAϑhA�;dA���A΅A�z�A�33A��Aȇ+AǑhA��A��Aĥ�A�ȴA��A���A�%A���A��uA��A�l�A���A�7LA�A�A��\A�K�A�?}A��mA�`BA�  A�x�A���A�C�A�z�A���A��jA��`A�\)A�VA�E�A��mA�x�A��A�n�A��A��!A�dZA�dZA��9A��#A��wA��yA���A�33A��+A��FA�$�A�;dA���A~bAzĜAw
=Ar�Ar5?Aq�Ao�Ak��Aj5?AhbAc�AZȴAW�hAR^5AP��AO;dANAMXALȴAH��AC�;A@bNA<E�A8~�A5x�A3�A0�/A/��A-K�A,��A,A�A)�#A(1A'`BA&�A%�TA%oA$(�A#�PA"�A��A��A�AZA��Ap�A��A33A��A�hA��AZAE�A��A%A�A�FA��A �A�-A��A��A�AG�A
�!A
1A	��A	�A��A	�A�uA^5AE�A+A��A�;A;dA��AbNAC�A �!A 1'@��P@��@�bN@��@��m@�+@��+@�M�@�=q@���@�X@�ƨ@�C�@���@�-@��h@��u@�t�@�7@�?}@�&�@�%@��@�@���@���@�%@웦@띲@��y@��@�z�@���@�-@�&�@㝲@���@�`B@��/@���@ާ�@��@��/@܃@�r�@ܣ�@�A�@��@���@��@�Z@�r�@�/@�p�@ܬ@�9X@ܛ�@ݺ^@�{@�
=@ߕ�@�S�@��T@�@ݺ^@ݑh@ܴ9@��m@ڏ\@�ff@�=q@�^5@�7L@�  @׾w@��@�E�@���@� �@ӶF@ҸR@�{@��@щ7@Гu@�Z@��;@�1@�b@�t�@�C�@�C�@Ο�@�v�@�V@��@�^5@�=q@���@�/@�I�@���@˝�@�K�@�@�~�@�x�@�V@ȃ@�A�@�A�@�A�@�1@Ǯ@�;d@Ƈ+@��T@�hs@Ĵ9@���@�"�@���@�@�~�@�5?@�@���@�O�@��/@���@�Z@�Q�@�9X@�  @��m@��P@�"�@�@���@���@���@��R@�~�@���@�O�@�/@��@�G�@��`@�bN@�Z@��m@�;d@�o@��R@�n�@�M�@�{@���@��D@�1@���@��@��@���@��@�-@��@��#@�X@�V@���@��;@�K�@���@�M�@�x�@��9@� �@�  @��F@�C�@��H@���@�-@��@��@�x�@��@�x�@�hs@�O�@�7L@�%@�Ĝ@���@���@��D@�1'@���@��
@���@���@���@���@��P@�K�@�@�
=@�@��@�v�@�=q@��T@�`B@�/@��@�Z@���@��P@��!@�ff@�V@�5?@�J@���@��@�`B@�?}@��@��@���@��@���@�I�@��m@�K�@��y@�~�@���@�&�@�%@�r�@�I�@� �@��@�ƨ@��F@��@��@���@�ff@�=q@�$�@�@�O�@�%@��@�Q�@��F@�dZ@�\)@�@���@��+@�M�@�{@��T@��^@��7@�7L@��u@�(�@���@�E�@���@�hs@�r�@��m@��@��@�|�@�C�@���@���@�ff@��@�@��@�X@�?}@�7L@�V@��@�I�@��m@��P@�+@��@���@�ff@��@�@���@��7@�x�@���@�Z@�Q�@�A�@��@��@�l�@�+@���@��H@���@��!@���@��\@�M�@��@���@��7@�hs@�/@�%@��/@��/@���@��@�j@�9X@�(�@���@��y@��R@��\@�v�@�^5@�E�@�-@�{@���@�@�{@�$�@���@��@���@�G�@���@��9@���@��D@��@�z�@�j@�bN@�A�@�1'@�(�@��@��@�33@��H@���@���@�ff@�^5@�E�@�J@�J@���@��h@��@�x�@�&�@�%@�Ĝ@�bN@�I�@�9X@��@K�@~ȴ@}��@|�@|1@{33@zJ@y�^@y��@y�@x�9@x �@x  @w�;@w��@v�@vv�@v{@u�h@tz�@s��@s33@r��@r�\@rM�@q�^@q%@p��@pb@o;d@n�+@n5?@n{@m�@m��@m/@m�@l�/@lz�@lI�@l1@k��@j�@j~�@jn�@jn�@j�@i�@i�^@i�7@i7L@h�9@hQ�@h1'@h1'@hb@g�w@gK�@f��@fȴ@f�+@fE�@e�T@e��@d��@d�j@d��@dj@c�m@cC�@c@c33@co@b�@bn�@bJ@a�@a�7@aX@a�@`��@`�u@`r�@`Q�@_K�@^v�@]�@]�-@]�@\�@\j@\j@\�@[�m@[��@[dZ@[o@Z��@Z-@Yx�@X��@XbN@X �@W�@W�@Vv�@U�@U�@T�@T�@Tz�@TI�@T(�@T(�@Sƨ@SC�@S@R��@R�!@R��@Rn�@R-@Q��@QG�@P��@P�9@PA�@O�@O�;@O�;@O�P@N�y@N�+@N@M@M`B@M�@L��@L��@K��@J��@J=q@I�#@I�7@I&�@H��@H�`@H�`@H��@H�u@G�@G�@Fv�@E�@Ep�@EO�@E�@DZ@C�m@Cƨ@C�@CC�@Co@B�H@B-@A�#@A�^@A��@A&�@@1'@?K�@>�R@>�+@>v�@>E�@>{@=�@=��@=O�@=/@<��@<�j@<j@;��@;�F@;��@;t�@:�@:��@:�\@:~�@:~�@:n�@:n�@:=q@9��@9��@9X@8��@8Ĝ@8r�@8Q�@8b@7�w@7K�@7�@6ff@5��@5�h@5�@5?}@4�@41@3�@2�@2^5@2=q@1��@1�^@1��@1�7@1&�@0��@0bN@01'@0  @/��@/��@/|�@/;d@.�y@.{@-�@-�@-V@,�@,j@+��@+��@+33@*�@*��@*=q@)��@)hs@)7L@)7L@)&�@)�@(�`@(�`@(bN@(b@'�@'��@'\)@'�@'
=@&��@&�y@&�y@&��@&��@&V@&@%�T@%�-@%�@%�@%O�@$�@#�F@#dZ@#"�@#@#o@"�@"�@"�H@"�H@"~�@!X@!%@ �9@ �@ A�@ b@�w@��@l�@\)@+@+@;d@
=@
=@
=@��@ȴ@��@V@�@�@�@�@p�@p�@`B@O�@V@��@��@Z@��@�@dZ@S�@S�@S�@C�@33@33@33@~�@J@J@�@�@�@�@�@�^@�7@X@7L@&�@�`@�u@b@��@�P@�P@�P@��@��@�P@�P@�P@�P@l�@�@�@V@{@@�@`B@?}@�@�@�@�D@j@I�@1@��@�m@�F@�F@t�@33@@��@��@n�@=q@-@��@x�@hs@G�@7L@7L@�@��@��@�u@ �@�;@�@��@|�@\)@\)@\)@K�@K�@;d@+@;d@+@+@+@+@+@�@�@ff@�@��@��@�h@�h@��@�-@�-@�-@��@�h@�h@p�@`B@��@z�@j@Z@I�@Z@Z@Z@Z@Z@Z@9X@(�A�dZA�hsA�dZA�bNA�XA�\)A�\)A�^5A�^5A�bNA�`BA�bNA�dZA�dZA�dZA�Q�A�S�A�^5A�bNA�`BA�ffA�dZA�ffA�jA�jA�jA�bNA�bNA�VA�bNA�\)A�M�A�O�A�K�A�G�A�E�A�K�A�C�A�;dA�1'A�&�A�$�A��A��A�{A� �A��A��`A���A���A��
A��TA��
A��#A��A�A�A�ȴA�ĜA�ĜA�ĜAҾwAҾwAҼjAҼjAҼjAҾwAҼjA�ĜA���A�ĜAҸRAҸRAҶFAҲ-Aҩ�AҲ-AҴ9AҴ9AҰ!AҮAҝ�AғuAҋDA҉7A҅A҇+A҉7A҅AҁA�z�A�z�A�v�A�v�A�v�A�v�A�v�A�t�A�p�A�p�A�p�A�p�A�p�A�p�A�n�A�n�A�l�A�l�A�jA�jA�jA�jA�jA�jA�jA�jA�jA�jA�jA�jA�jA�jA�hsA�hsA�hsA�hsA�hsA�hsA�hsA�hsA�ffA�ffA�dZA�dZA�dZA�dZA�bNA�dZA�`BA�^5A�`BA�^5A�^5A�\)A�\)A�^5A�^5A�\)A�^5A�\)A�\)A�ZA�XA�XA�XA�XA�XA�ZA�ZA�ZA�ZA�ZA�VA�XA�XA�S�A�VA�VA�VA�S�A�Q�A�Q�A�Q�A�S�A�S�A�VA�S�A�S�A�S�A�S�A�S�A�O�A�O�A�O�A�M�A�I�A�G�A�A�A�?}A�A�A�A�A�C�A�A�A�A�A�A�A�?}A�=qA�9XA�7LA�7LA�9XA�9XA�;dA�;dA�;dA�7LA�5?A�1'A�/A�-A�/A�/A�/A�/A�+A�$�A� �A��A���AѲ-AѓuA�S�A�C�A�/A��TAд9AЃA�7LA���A��A��mA��;A��/A���A���A���A���A���A���A���A���A���A��
A��#A��HA��TA��A���A�  A�A�1A�
=A�1A�1A�1A�A�A�
=A��A�$�A�=qA�VA�hsA�r�A�|�AЃAЅAЋDAЏ\AГuAЕ�AЕ�AЗ�AЙ�AС�AХ�AЧ�AЩ�AЩ�AЧ�AХ�AЧ�AЩ�AЩ�AХ�AУ�AС�AП�AП�AС�AУ�AС�AЛ�AЛ�AН�AЛ�AЛ�AП�AС�AС�AН�AЗ�AГuAЏ\AЍPAЋDAЉ7AЅAЃAЃAЅAЃA�|�A�t�A�n�A�n�A�jA�ffA�dZA�XA�O�A�I�A�A�A�=qA�=qA�;dA�7LA�+A��A�  A��`A�ĜAϮAϬAϡ�Aϗ�AϋDAρA�v�A�p�A�jA�`BA�S�A�K�A�/A�"�A�1A��A��yA��TA��
A���A�ƨA�ĜA�ĜA�AθRAάAΛ�A΍PA�x�A�ffA�\)A�C�A��#A͝�ÁA�`BA�K�A�E�A�33A��ȂhA�x�A�5?A��A˼jA˸RA˰!A˕�A˅A�O�A��A���Aʙ�A�`BA�ƨA���AȋDA�M�A�$�A��A�A��A�ƨAǲ-Aǟ�Aǡ�AǋDA�ffA�E�A�9XA�33A�{A���A��`A�AƗ�A�G�A��A���A��;A��
A���A���A�ƨA�x�A�;dA�{AĶFA�1'A��yA�^5A�=qA�oA��#A���A�~�A�ZA�  A�ȴA��A�S�A��HA��uA�t�A�1'A���A�S�A�C�A�A�A�-A�9XA��A���A�bNA�5?A��A��A���A�S�A��A���A���A�=qA���A��RA�JA��jA���A��uA��\A�|�A�r�A�`BA�VA�/A��A��-A�bNA�"�A��jA�S�A�E�A�1'A� �A�oA���A�Q�A��A���A�Q�A��^A��PA�hsA�C�A�1A��
A��FA��7A�K�A��A�1A��#A��uA��DA��PA��\A��PA��A�r�A�jA�\)A�K�A�1'A��TA���A�x�A�K�A�bA�ĜA��!A��7A��A���A��DA�/A��
A���A��A�ffA�$�A��A���A���A�O�A�%A�=qA��A�ĜA���A�l�A�I�A�K�A�K�A�A�A�A�A�A�A�7LA���A��9A��A���A���A��A��A�r�A�hsA�hsA�dZA�VA�O�A�=qA�/A�A�bA�A��yA��TA��;A��
A���A��-A��A���A��A�|�A�n�A�A�A�5?A�1'A�1'A�-A�-A��A���A�hsA�VA��;A��-A�v�A��A�5?A���A�33A�z�A�&�A���A��A��A��A��`A��TA��;A��#A��wA���A��hA�x�A�hsA�G�A�"�A��A�VA���A��A��`A��;A��
A���A�ƨA��^A���A���A��A��A�~�A�~�A�~�A�|�A�z�A�z�A�r�A�;dA�(�A�%A���A�ƨA��FA���A���A��uA��A�bNA�C�A�5?A�
=A���A���A��A���A���A��RA�x�A�ffA�`BA�A�A�=qA�"�A���A���A��A��
A��
A�A��A��DA�l�A�;dA��A��TA���A��FA���A�bNA��A��RA���A���A��uA��DA��+A��A�~�A�|�A�x�A�hsA�hsA�VA�C�A�G�A�7LA�+A��A��A���A���A��/A��!A���A�hsA�A��A�XA�5?A�oA���A�+A�v�A���A��HA���A���A�dZA�JA���A�|�A�ZA�K�A��
A�z�A�=qA�JA���A��;A���A���A��RA��9A��RA��9A��9A��FA��A��A���A���A���A��hA�~�A�|�A�|�A�x�A�v�A�l�A�ZA�A�A�&�A�1A��mA���A��FA���A��A�n�A�K�A�33A��A��A��#A��9A�I�A�VA��`A���A�~�A�bNA�bNA�`BA�`BA�XA�C�A�(�A��A�{A�
=A�%A��A��-A�dZA�=qA�-A�oA���A��A��;A���A��jA���A��hA�hsA�oA��A���A���A��wA���A�|�A�oA�n�A��PA�JA���A�Q�A�?}A�1'A�+A�$�A�"�A��A��A���A��A�jA�\)A�\)A�I�A�G�A�?}A�1'A��A�{A�JA�  A��A��/A���A�ĜA��-A��A��PA�K�A� �A�1A�%A�A���A���A���A��A��A��\A�hsA�S�A�7LA�+A�oA��A��HA��9A�l�A�bA�ƨA�bNA�A�+A��yA�ĜA���A�|�A�O�A�"�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                            G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
~�B
}�B
~(B
~�B
}�B
~(B
~�B
}VB
}"B
|�B
zDB
zxB
w�B
v�B
v�B
v`B
t�B
t�B
tTB
tTB
t�B
t�B
t�B
t�B
s�B
s�B
r�B
r�B
rGB
qAB
pB
o B
m�B
kB
R�B
9$B
7�B
E9B
OBB
e�B
v`B
|�B
�B
�B
�SB
��B
�uB
}VB
v�B
e�B
]dB
j�B
��BGEB��BӏB�BqB%�B+�BH�BpBm]BkB^5BdZBR�BW�BS�BVmBN�BOBBK)BI�BH�BE9B:*B;dB+�B&�B�B:B��B�B�vB��B�B��B��B��B� BZQB7B)_B&�B�B�B
�B
�B
ɆB
�~B
��B
~�B
s�B
[WB
1�B
%FB
%B
�B	��B	��B	�aB	�HB	��B	��B	��B	�_B	��B	_B	M�B	5�B	-CB	'RB	 �B	�B	B	�B�+B��B�mB�B֡B� B�B�KB�jB��B��B�B� B��B�WB��B�cB�B�)B�MB��B�B�%B��B��B��B�vB�B��B��B�B�B�WB�/B�iB�;B�B�B��B��B��B��B�lB�	B�xB�B��B	 4B	{B	
=B	�B	B	 'B	'�B	'RB	$tB	$�B	!�B	&B	)�B	*eB	33B	5�B	6�B	6B	C�B	OvB	Q�B	Q�B	S�B	WsB	]�B	jB	lWB	iB	i�B	n�B	rB	qvB	s�B	qvB	poB	rB	r�B	r�B	q�B	l�B	l�B	o5B	qvB	o�B	l�B	k�B	e�B	e�B	b�B	dZB	b�B	gmB	j�B	j�B	l"B	n�B	n�B	m]B	oiB	u�B	xB	z�B	y�B	y	B	{B	�GB	��B	��B	�B	�OB	��B	��B	��B	�B	�tB	�<B	�NB	ѷB	�NB	҉B	�gB	��B	�;B	�dB	��B	��B	�jB	�pB	چB	��B	�WB	��B	��B	�yB	�B	�?B	֡B	�?B	�B	��B	��B	�jB	�;B	�HB	��B	�B	�B	��B	�`B	�fB	�
B	�B	��B	�]B	�/B	�B	��B	�5B	�cB	�)B	�B	�yB	��B	�
B	�B	�2B	��B	�B	��B	�B	�yB	��B	�sB	�B	��B	�fB	��B	�B	�B	�yB	�B	�KB	�QB	�B	�QB	��B	�B	�B	��B	�B	�B	��B	�iB	�;B	�oB	�oB	�B	�B	�MB	�B	�B	�B	�MB	�ZB	�B	�TB	�+B	�ZB	��B	�ZB	��B	��B	��B	�`B	��B	��B	��B	��B	�ZB	�%B	�8B	��B	�B	�B	�B	�B	��B	��B	��B	��B	�B	��B	��B	�>B	�>B	��B	��B	��B	�B	��B	��B	�"B	�"B	�"B	�"B	��B	�(B	��B	��B	�cB	��B	��B	��B
B
;B
B
AB
�B
B
SB
_B
�B
�B
B
"B
�B
�B
�B
�B
\B
VB
(B
�B
�B
4B
B
�B
�B
B
�B
FB
FB
{B
{B
FB
uB
uB
@B
�B
�B
FB
{B
{B
B
B
B
FB
�B
{B
{B
{B
�B
B
�B
�B
�B
�B
�B
�B
$B
�B
�B
�B
�B
kB
kB
B
kB
7B
�B
	B
qB
qB
�B
�B
B
�B
�B
1B
YB
+B
�B
7B
�B
B
B
7B
�B
�B
�B
B
B
B
VB
 'B
 'B
 'B
 �B
!-B
!�B
"4B
"�B
"�B
#:B
"�B
#�B
#�B
$B
$B
#�B
#�B
$�B
$�B
$�B
%B
&B
&�B
&�B
'�B
'�B
'B
'RB
'�B
'RB
'�B
'�B
'�B
(�B
(�B
(�B
)*B
)�B
)�B
)*B
)*B
)�B
)�B
)�B
*0B
+�B
-B
,�B
,�B
-CB
,�B
-�B
-CB
-wB
.B
.B
-�B
/�B
0�B
0!B
1[B
1�B
1[B
1[B
1[B
1[B
1�B
1'B
1�B
1'B
1[B
1�B
0�B
1�B
1�B
1�B
1�B
1�B
2�B
2�B
2aB
2�B
3hB
2�B
3�B
3�B
33B
3hB
49B
49B
49B
5�B
5B
5?B
5?B
6FB
5�B
7�B
7B
8�B
8RB
8�B
8�B
8�B
9�B
8�B
:^B
:^B
:*B
9�B
:�B
:�B
:�B
:�B
<jB
;�B
<6B
;�B
<jB
;�B
<�B
;�B
<B
<�B
=qB
=<B
<�B
<�B
=B
=B
<�B
<jB
<�B
=qB
=<B
<�B
=�B
>wB
=�B
>BB
>BB
>�B
>�B
>wB
?B
?�B
@OB
@OB
@B
@B
?�B
?}B
?�B
?�B
?�B
@B
@�B
@�B
A�B
A�B
A�B
AUB
AUB
A�B
B�B
C�B
E�B
GEB
GzB
I�B
I�B
J#B
J�B
K)B
K^B
K^B
K�B
K�B
K)B
L�B
M6B
MB
M6B
M�B
MB
LdB
L0B
L�B
L�B
M6B
NB
NB
N�B
OvB
PB
OBB
OB
N�B
OBB
O�B
P�B
Q�B
Q�B
Q�B
R B
S&B
S[B
S&B
R�B
S�B
S�B
S�B
S�B
T,B
TaB
S�B
S�B
S�B
T,B
S�B
T,B
UgB
U�B
UgB
T�B
U2B
U2B
U2B
VmB
V9B
VmB
U�B
U�B
VB
W�B
XB
XEB
YB
YKB
X�B
Y�B
ZB
Y�B
ZB
Z�B
Z�B
Z�B
[WB
Z�B
[#B
[#B
[�B
\�B
[�B
[�B
\)B
[�B
[�B
[�B
]/B
\�B
]/B
\�B
]�B
^�B
_�B
_�B
_�B
`BB
`B
`vB
`BB
`�B
`�B
aB
`�B
a�B
a|B
c�B
d&B
c�B
dZB
e`B
e�B
e`B
f2B
e�B
e�B
e�B
f2B
e�B
f2B
e�B
f�B
ffB
gB
f�B
gmB
gB
g�B
g�B
hsB
h�B
h�B
h�B
h�B
i�B
i�B
j�B
j�B
kQB
kB
kQB
k�B
k�B
kQB
k�B
l�B
lWB
l�B
lWB
m)B
l�B
l�B
m)B
m)B
ncB
ncB
n�B
n�B
n�B
o�B
oiB
p;B
poB
p;B
poB
qAB
qAB
rB
q�B
q�B
q�B
qvB
q�B
qB
rGB
q�B
rGB
rB
r�B
s�B
sMB
s�B
sMB
sMB
s�B
sB
s�B
s�B
sMB
s�B
s�B
s�B
s�B
tTB
v+B
v+B
v�B
v�B
v+B
v�B
v`B
v�B
v+B
v�B
x8B
w�B
xB
x�B
x�B
y	B
yrB
y�B
y�B
y	B
y�B
yrB
y>B
y�B
zB
y�B
y>B
yrB
y�B
zDB
zxB
z�B
z�B
z�B
{B
{JB
{JB
{JB
{�B
{�B
{JB
{B
z�B
{JB
{�B
|PB
|�B
|�B
|�B
|�B
|�B
|B
|�B
{�B
{�B
{�B
|PB
|PB
{�B
{�B
|B
|�B
}"B
}�B
}�B
~(B
}�B
~(B
~�B
cB
�B
cB
~�B
~�B
�B
�B
cB
.B
cB
�iB
��B
�B
�;B
��B
�uB
�uB
�B
�AB
��B
��B
�uB
�uB
��B
��B
�GB
�GB
��B
��B
��B
�B
�B
�MB
��B
��B
�B
��B
�YB
�SB
�%B
��B
��B
��B
�%B
��B
��B
��B
��B
��B
��B
�1B
��B
�1B
�1B
��B
��B
�1B
��B
��B
��B
�1B
��B
�fB
��B
��B
�fB
�fB
��B
�B
��B
��B
��B
��B
��B
�	B
�lB
��B
��B
��B
�lB
��B
�lB
�=B
��B
��B
�=B
�=B
�	B
�=B
�rB
�	B
�	B
�rB
�rB
�rB
.B
��B
}�B
� B
�4B
{JB
�4B
~]B
� B
~(B
~�B
}VB
~�B
~�B
.B
�oB
�B
|�B
{JB
~�B
{B
~�B
|�B
|�B
}VB
}�B
�B
z�B
�B
|�B
}�B
}�B
{B
|�B
~�B
|PB
xB
�uB
|�B
{JB
zxB
}�B
z�B
zB
{�B
|B
�{B
�oB
y�B
xlB
v`B
v�B
wfB
zB
w�B
}�B
wfB
v�B
w�B
u%B
u�B
u�B
v`B
v`B
u�B
u�B
u�B
v`B
v�B
v+B
v`B
wfB
uZB
u�B
u�B
v`B
rB
uZB
v�B
u�B
v`B
u�B
u%B
y�B
y	B
u�B
s�B
s�B
t�B
t�B
s�B
sMB
s�B
s�B
s�B
s�B
s�B
s�B
tTB
s�B
s�B
s�B
sMB
sB
s�B
sMB
s�B
sMB
sMB
s�B
s�B
sMB
sMB
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
s�B
s�B
tB
s�B
s�B
s�B
tB
t�B
u%B
uZB
u�B
uZB
u�B
uZB
v+B
u%B
tTB
u�B
uZB
tTB
s�B
sMB
s�B
r�B
sMB
sMB
s�B
tTB
tB
t�B
t�B
t�B
u%B
s�B
tB
tB
tTB
s�B
r|B
sMB
rGB
q�B
q�B
q�B
rB
r�B
sB
s�B
s�B
r�B
s�B
r�B
r�B
qvB
qB
qvB
p�B
qAB
qAB
q�B
rB
sMB
r|B
rB
qAB
pB
o5B
p;B
o5B
oiB
n�B
oiB
poB
qAB
p;B
p;B
o�B
o�B
ncB
ncB
m�B
n/B
m]B
n�B
o B
n�B
m�B
m�B
l�B
l"B
k�B
j�B
i�B
i�B
l�B
i�B
b�B
Z�B
VB
T�B
Y�B
K�B
IRB
PHB
>�B
9�B
8�B
8B
7LB
7B
6B
7B
7�B
7�B
7LB
7B
7�B
7�B
8�B
:*B
=<B
?�B
FB
J�B
K)B
K)B
LdB
MjB
NB
OB
OBB
PHB
QNB
NpB
S�B
VmB
[�B
d�B
dZB
oiB
m�B
t�B
tTB
u%B
u%B
u�B
u�B
v�B
w�B
wfB
}VB
}�B
|�B
{�B
|�B
~�B
.B
~�B
}�B
}�B
~�B
�4B
�B
�B
�{B
�uB
��B
�uB
�oB
�uB
��B
�%B
��B
�SB
��B
��B
�B
��B
�%B
�%B
��B
��B
��B
��B
�SB
�MB
��B
�B
�GB
�B
��B
�uB
��B
�B
}�B
�B
~(B
cB
|B
y>B
y	B
y�B
y	B
zDB
z�B
v+B
qAB
r�B
h
B
h�B
iB
h�B
f2B
c�B
a�B
_pB
`B
`B
^5B
^jB
^5B
[WB
\�B
XEB
Y�B
[�B
aHB
k�B
p�B
x8B
x8B
zxB
�{B
��B
�XB
�FB
�BB
ҽB
چB
��B�B>BBG�BQNBZB]/BdZBzxB�4B��B��B�0B�B�mB�BɺBȀB�}B�?B�B�)B�5B�]B�BYB�B	�BDB�B�B�BeB�B�B�B#:B!�B!�B �B%B#�B#�B)_B*0B2�B-CB-CB,�B*eB*eB(�B*eB1�B3hB0!BIBP�Ba|Bq�Bk�BsMBp;BoiBpBm]By	BkQBj�BsMBjBk�Be�BncBf�Bi�B[�BZ�B^�B�7Bu�Ba�B_�B[�B[WBXBc�B[�BXyBQBQBh�B��Bv+B`BZ�BS&BP�BPHBQBQ�BS�BQ�BXBW�B\�BZQBXyBiBVBO�BPHBM�BM6BV9Ba�BW�BUgBV�BU�BP�BL0BN�BPHBMBL0BS[BMjBRTBK^BS&BMjBJ�BI�BHBHKBI�BJ#BG�BGEBHKBJ#BQNBH�BG�BI�BJ�BGBAUBD�BCaBN<BA�BGB?}B;�B:^B7B?}B7B6�B:*B:�B>wBI�B.�B/B1[B0!B(�B&�B%�B&�B&�B$�B&�B6B&�B!-B"4B �B 'B �B!-B �BIB�B!B�BBCB�B�B�B{B1B�B�B�B=BSBSBBB�B�B(B(BBDB�B�BbB.B�B�B �B��B	�B �B�2BMB��B�`B�/B��B��B�KBںB��B�EB�B�)B�BخB�B��B֡B��B�}BӏB�TBΥB�BB�<B�6B�jBΥB�B�XB�0B�BB�B�RBȴB�zBǮBȴBǮBȴB�)BÖB��B��B�[B��B��B�UB��B��B�B��B��B�B��B�zB�*B��B�3B��B�B��B��B�qB�0B��B��B��B�$B��B��B��B��B��B��B��B��B��B��B��B�$B��B�B�eB�(B��B��B�"B��B�DB��B�xB��B��B�lB�B��B�AB�fB��B�B�uB�B�B�B��Bu�Bt�B��B~�Bh
BgBbNBo5BpBu�BTaBK^BJ�BMBK�BMBC�BF�B:�B5?BUgB@B0�B.�B/B.B,qB*�B)�B*0B(�B*0B)_B(�B)�B)*B*�B)�B(XB)�B(XB&�B'RB&�B%zB%zB'�B%FB$@B#�B"hB�B�B�B�B	B�BSBMBoB(B�B�B�B�B�B
��B
��B
�fB
�B
��B
��B
�DB
�ZB
��B
��B
�B
�B
�MB
�2B
�|B
�B
�B
�B
�|B
�TB
��B
�B
ߤB
�B
��B
�dB
�]B
��B
�jB
�)B
�dB
��B
�B
��B
ΥB
��B
��B
��B
��B
�'B
�B
�B
�CB
�xB
��B
�B
�SB
�YB
��B
�.B
��B
��B
��B
��B
��B
�fB
��B
��B
�fB
�_B
�uB
�GB
�B
��B
}�B
��B
��B
}�B
s�B
u�B
u�B
t�B
r�B
sMB
v�B
w�B
}�B
l�B
m)B
oiB
gB
g�B
_�B
`vB
^B
`�B
XyB
P}B
W
B
Q�B
C�B
8�B
1�B
3�B
.�B
5�B
0�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                            B
v�B
v B
v4B
v�B
u�B
v4B
v�B
ubB
u.B
t�B
rPB
r�B
o�B
o	B
o	B
nlB
l�B
l�B
l`B
l`B
l�B
l�B
l�B
l�B
k�B
k�B
j�B
j�B
jSB
iMB
hB
gB
e�B
c(B
J�B
10B
/�B
=EB
GNB
^
B
nlB
t�B
w�B
{B
}_B
|�B
z�B
ubB
o	B
^
B
UpB
b�B
��B?QB��B˛B��B}B�B#�B@�BhBeiBc(BVAB\fBJ�BO�BLBNyBF�BGNBC5BA�B@�B=EB26B3pB#�B�B�B
FB��B�#BǂB��B�B��B��B��BxBR]B/#B!kB�B�B
��B
�B
�B
��B
��B
��B
wB
k�B
ScB
)�B
RB
B
�B	��B	��B	�mB	�TB	��B	��B	��B	�kB	��B	WB	E�B	-�B	%OB	^B	B	�B	$B	�B�7B��B�yB�BέB�,B�B�WB�vB��B��B�B�B��B�cB��B�oB�B�5B�YB��B�B�1B�B�B�B�B�B�B��B�B�(B�cB�;B�uB�GB�B�B�B��B��B�	B�xB�B�B�B� B�@B��B	IB	�B	B	3B	�B	^B	�B	�B	B	$B	!�B	"qB	+?B	-�B	.�B	.B	;�B	G�B	I�B	I�B	LB	OB	U�B	b"B	dcB	aB	a�B	f�B	jB	i�B	k�B	i�B	h{B	jB	j�B	j�B	i�B	e B	d�B	gAB	i�B	g�B	d�B	c�B	^
B	]�B	Z�B	\fB	Z�B	_yB	b�B	b�B	d.B	f�B	f�B	eiB	guB	nB	pB	r�B	q�B	qB	s�B	{SB	�B	��B	�'B	�[B	��B	��B	��B	�B	��B	�HB	�ZB	��B	�ZB	ʕB	�sB	��B	�GB	�pB	�B	��B	�vB	�|B	ҒB	��B	�cB	�B	�B	ЅB	�#B	�KB	έB	�KB	�)B	�B	��B	�vB	�GB	�TB	��B	��B	ާB	�
B	�lB	�rB	�B	�(B	��B	�iB	�;B	�B	��B	�AB	�oB	�5B	��B	�B	��B	�B	ާB	�>B	��B	ާB	��B	�B	�B	��B	�B	�B	��B	�rB	��B	�B	߭B	�B	�B	�WB	�]B	�B	�]B	��B	��B	��B	� B	�B	�B	�B	�uB	�GB	�{B	�{B	�B	�B	�YB	�B	�B	�B	�YB	�fB	�B	�`B	�7B	�fB	��B	�fB	�B	�B	�B	�lB	�B	��B	��B	��B	�fB	�1B	�DB	��B	�B	�"B	�(B	�"B	�B	��B	��B	��B	�(B	��B	��B	�JB	�JB	�B	�B	��B	�(B	��B	��B	�.B	�.B	�.B	�.B	��B	�4B	��B	��B	�oB	��B	��B	��B	�B	�GB	�B	�MB	��B	�+B	�_B	�kB
 �B
�B
'B
.B
�B
�B
�B
�B
hB
bB
4B
�B
�B
	@B
B
�B
�B
B
�B
RB
RB
�B
�B
RB
�B
�B
LB
�B
�B
RB
�B
�B
B
$B
B
RB
�B
�B
�B
�B
�B
$B
�B
�B
�B
�B
�B
�B
0B
�B
�B
�B
B
wB
wB
B
wB
CB
�B
B
}B
}B
�B
�B
B
�B
�B
=B
eB
7B
B
CB
�B
B
B
CB
�B
�B
�B
!B
!B
'B
bB
3B
3B
3B
�B
9B
�B
@B
�B
�B
FB
�B
�B
�B
B
B
�B
�B
�B
�B
�B
B
$B
�B
�B
�B
�B
*B
^B
�B
^B
�B
�B
�B
 �B
 �B
 �B
!6B
!�B
!�B
!6B
!6B
!�B
"B
"B
"<B
#�B
%B
$�B
$�B
%OB
$�B
%�B
%OB
%�B
& B
& B
%�B
'�B
(�B
(-B
)gB
*B
)gB
)gB
)gB
)gB
)�B
)3B
)�B
)3B
)gB
)�B
(�B
)�B
)�B
)�B
)�B
*B
*�B
+B
*mB
*�B
+tB
*�B
+�B
+�B
+?B
+tB
,EB
,EB
,EB
-�B
-B
-KB
-KB
.RB
-�B
/�B
/#B
0�B
0^B
0�B
0�B
0�B
2B
0�B
2jB
2jB
26B
2B
2�B
2�B
2�B
2�B
4vB
3�B
4BB
3�B
4vB
3�B
4�B
3�B
4B
4�B
5}B
5HB
4�B
4�B
5B
5B
4�B
4vB
4�B
5}B
5HB
4�B
5�B
6�B
5�B
6NB
6NB
6�B
6�B
6�B
7 B
7�B
8[B
8[B
8&B
8&B
7�B
7�B
7�B
7�B
7�B
8&B
8�B
8�B
9�B
9�B
9�B
9aB
9aB
9�B
:�B
;�B
=�B
?QB
?�B
A�B
A�B
B/B
B�B
C5B
CjB
CjB
DB
C�B
C5B
D�B
EBB
EB
EBB
E�B
EB
DpB
D<B
D�B
D�B
EBB
FB
FB
F�B
G�B
H B
GNB
GB
F�B
GNB
G�B
H�B
I�B
I�B
I�B
J,B
K2B
KgB
K2B
J�B
K�B
LB
K�B
LB
L8B
LmB
LB
LB
K�B
L8B
LB
L8B
MsB
M�B
MsB
M
B
M>B
M>B
M>B
NyB
NEB
NyB
M�B
M�B
NB
O�B
PB
PQB
Q#B
QWB
P�B
Q�B
R)B
Q�B
R)B
R�B
R�B
R�B
ScB
R�B
S/B
S/B
S�B
UB
TB
S�B
T5B
S�B
TB
S�B
U;B
T�B
U;B
T�B
U�B
V�B
W�B
W�B
W�B
XNB
XB
X�B
XNB
X�B
X�B
YB
X�B
Y�B
Y�B
[�B
\2B
[�B
\fB
]lB
^
B
]lB
^>B
]�B
]�B
]�B
^>B
]�B
^>B
^
B
^�B
^rB
_B
^�B
_yB
_B
_�B
_�B
`B
`�B
`�B
`�B
`�B
a�B
a�B
b�B
b�B
c]B
c(B
c]B
c�B
c�B
c]B
c�B
d�B
dcB
d�B
dcB
e5B
d�B
d�B
e5B
e5B
foB
foB
f�B
f�B
f�B
g�B
guB
hGB
h{B
hGB
h{B
iMB
iMB
jB
i�B
i�B
i�B
i�B
i�B
iB
jSB
i�B
jSB
jB
j�B
k�B
kYB
k�B
kYB
kYB
k�B
k%B
k�B
k�B
kYB
k�B
k�B
k�B
k�B
l`B
n7B
n7B
n�B
n�B
n7B
n�B
nlB
n�B
n7B
n�B
pDB
o�B
pB
p�B
p�B
qB
q~B
q�B
q�B
qB
q�B
q~B
qJB
q�B
rB
q�B
qJB
q~B
q�B
rPB
r�B
r�B
r�B
r�B
s"B
sVB
sVB
sVB
s�B
s�B
sVB
s�B
r�B
sVB
s�B
t\B
t�B
t�B
t�B
t�B
t�B
t(B
t�B
s�B
s�B
s�B
t\B
t\B
s�B
s�B
t(B
t�B
u.B
u�B
v B
v4B
u�B
v4B
wB
woB
w�B
woB
wB
v�B
w�B
w�B
woB
w:B
woB
xuB
x�B
yB
yGB
y�B
z�B
z�B
zB
zMB
z�B
z�B
z�B
z�B
z�B
{�B
{SB
{SB
{�B
{�B
{�B
|%B
|%B
|YB
|�B
|�B
}+B
}�B
~eB
}_B
~1B
~�B
}�B
}�B
~1B
}�B
}�B
~�B
B
B
�B
�=B
�B
�=B
�=B
�	B
��B
�=B
�	B
��B
�	B
�=B
��B
�rB
��B
�	B
�rB
�rB
��B
�B
��B
��B
��B
��B
��B
�B
�xB
��B
��B
��B
�xB
��B
�xB
�IB
��B
��B
�IB
�IB
�B
�IB
�~B
�B
�B
�~B
�~B
�~B
w:B
x�B
u�B
xB
x@B
sVB
x@B
viB
xB
v4B
wB
ubB
v�B
v�B
w:B
y{B
w�B
t�B
sVB
v�B
s"B
v�B
t�B
t�B
ubB
u�B
w�B
r�B
{B
t�B
u�B
u�B
s"B
t�B
v�B
t\B
pB
z�B
t�B
sVB
r�B
u�B
r�B
rB
s�B
t(B
{�B
y{B
q�B
pxB
nlB
n�B
orB
rB
o�B
u�B
orB
o	B
o�B
m1B
nB
nB
nlB
nlB
nB
m�B
nB
nlB
n�B
n7B
nlB
orB
mfB
nB
nB
nlB
jB
mfB
n�B
m�B
nlB
nB
m1B
q�B
qB
m�B
k�B
k�B
l�B
l�B
k�B
kYB
k�B
k�B
k�B
k�B
k�B
k�B
l`B
k�B
k�B
k�B
kYB
k%B
k�B
kYB
k�B
kYB
kYB
k�B
k�B
kYB
kYB
kYB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l+B
k�B
k�B
k�B
l+B
l�B
m1B
mfB
m�B
mfB
nB
mfB
n7B
m1B
l`B
m�B
mfB
l`B
k�B
kYB
k�B
j�B
kYB
kYB
k�B
l`B
l+B
l�B
l�B
l�B
m1B
k�B
l+B
l+B
l`B
k�B
j�B
kYB
jSB
i�B
i�B
i�B
jB
j�B
k%B
k�B
k�B
j�B
k�B
j�B
j�B
i�B
iB
i�B
h�B
iMB
iMB
i�B
jB
kYB
j�B
jB
iMB
hB
gAB
hGB
gAB
guB
f�B
guB
h{B
iMB
hGB
hGB
g�B
g�B
foB
foB
e�B
f;B
eiB
f�B
gB
f�B
e�B
e�B
d�B
d.B
c�B
b�B
a�B
a�B
e B
a�B
Z�B
R�B
NB
L�B
Q�B
C�B
A^B
HTB
6�B
1�B
0�B
0)B
/XB
/#B
.B
/#B
/�B
/�B
/XB
/#B
/�B
/�B
0�B
26B
5HB
7�B
>B
B�B
C5B
C5B
DpB
EvB
FB
GB
GNB
HTB
IZB
F|B
LB
NyB
S�B
]B
\fB
guB
fB
l�B
l`B
m1B
m1B
nB
m�B
o	B
o�B
orB
ubB
u�B
t�B
s�B
t�B
v�B
w:B
v�B
v B
v B
v�B
x@B
yB
zB
{�B
z�B
y�B
z�B
y{B
z�B
|�B
~1B
}�B
}_B
|�B
|�B
}+B
}�B
~1B
~1B
|�B
{�B
{�B
|�B
}_B
|YB
{�B
{B
{SB
zB
{�B
z�B
x�B
w�B
u�B
w�B
v4B
woB
t(B
qJB
qB
q�B
qB
rPB
r�B
n7B
iMB
j�B
`B
`�B
aB
`�B
^>B
[�B
Y�B
W|B
XB
XB
VAB
VvB
VAB
ScB
T�B
PQB
Q�B
TB
YTB
c�B
h�B
pDB
pDB
r�B
{�B
��B
�dB
�RB
�NB
��B
ҒB
��B�B6NB?�BIZBR)BU;B\fBr�B�@B��B��B�<B�B�yB�B��B��BȉB�KB�B�5B�AB�iB��B�eB��B�BPB�B
�BBqBB�B�BFBB�B�BB�B�B!kB"<B*�B%OB%OB$�B"qB"qB �B"qB)�B+tB(-BA)BH�BY�Bi�Bc�BkYBhGBguBhBeiBqBc]Bb�BkYBb"Bc�B^
BfoB^�Ba�BTBR�BV�B�CBnBY�BW�BS�BScBPB[�BTBP�BI&BI&B`�B��Bn7BXBR�BK2BH�BHTBI&BI�BK�BI�BPBO�BUBR]BP�BaBNBG�BHTBE�BEBBNEBY�BO�BMsBN�BM�BH�BD<BF�BHTBEBD<BKgBEvBJ`BCjBK2BEvBB�BA�B@#B@WBA�BB/B?�B?QB@WBB/BIZB@�B?�BA�BB�B?B9aB<�B;mBFHB9�B?B7�B3�B2jB/#B7�B/#B.�B26B2�B6�BA�B&�B''B)gB(-B �B�B�B�B�B�B�B.B�B9B@BB3B�B9BBUB�B-B�BBOB�B�BB�B=B�B�B�BIB_B_B*BB�B�B4B4B'BPB �B�BnB:B��B��B��B��B�B��B�>B�YB��B�lB�;B��B��B�WB��B��B�QB�B�5B�#BкB�B��BέB��BȉB˛B�`BƱB�NB�HB�BB�vBƱB�B�dB�<B�NB�)B�^B��B��B��B��B��B��B�5B��B�B��B�gB��B��B�aB��B��B�&B��B��B�B��B��B�6B��B�?B��B�B�B��B�}B�<B�B�B��B�0B��B��B��B��B��B��B��B��B��B��B��B�0B��B�!B�qB�4B��B��B�.B��B�PB��B��B�B��B�xB�B��BzMB�rB~�ByBz�ByB|%ByB}�Bm�Bl�B}�Bv�B`B_BZZBgABhBnBLmBCjBCBEBC�BEB;�B>�B2�B-KBMsB8&B(�B&�B''B& B$}B"�B!�B"<B �B"<B!kB �B!�B!6B"�B!�B dB"B dB�B^B�B�B�B�BRBLB�BtB�B�B�B�BB�B_BYB
{B4B
�B�B
��B 	B
��B
�B
�B
�rB
�B
�B
��B
�PB
�fB
�B
��B
�B
�%B
�YB
�>B
�B
�B
�B
߭B
وB
�`B
��B
ضB
װB
�B
�B
�pB
�iB
��B
�vB
�5B
�pB
��B
�)B
��B
ƱB
�
B
��B
��B
��B
�3B
�'B
�!B
�OB
��B
��B
�'B
�_B
�eB
��B
�:B
��B
��B
��B
��B
��B
�rB
��B
~�B
�rB
kB
z�B
{SB
w�B
x�B
v B
z�B
{�B
v B
k�B
nB
nB
l�B
j�B
kYB
o	B
o�B
u�B
d�B
e5B
guB
_B
_�B
W�B
X�B
VB
X�B
P�B
H�B
OB
I�B
<
B
0�B
*B
+�B
&�B
-�B
(�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                            G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0077649                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0077649                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 23 11 2022 179 -0.0077649 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 23 11 2022 179 -0.0077649 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230426223237                            20230426223237AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023042622323720230426223237  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622323720230426223237QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F83E           783E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622323720230426223237QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               