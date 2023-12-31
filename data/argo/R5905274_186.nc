CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-04-26T22:32:29Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20230426223229  20230426223229  5905274 5905274 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO7315                            7315                            2B  2B  AA  SOLO_II                         SOLO_II                         8643                            8643                            SBE602 11Jan18                  SBE602 11Jan18                  853 853 @���i�@���i�11  @��8�0@��8�0@0B$�LD|@0B$�LD|�dI��1e��dI��1e�11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  ?u?��H@E�@�  @�  @\@�  A   A��A   A+�AAG�A`��A�  A�  A�\)A�\)A�  A�  A�  A�Q�B   B  B(�B�
B�
B'�
B0(�B8  B@  BH(�BPQ�BW�
B`  BhQ�Bp(�Bx  B�
B��
B�B��
B��B�{B�{B�{B�(�B�{B��B�  B�  B�{B�{B��B��B�{B�{B�{B�  B�B��
B��B��
B��
B��
B�  B�{B�  B�  B�{C 
=C{C
=C  C  C	�C��C
=C{C
=C  C{C{C{C{C
=C 
=C"
=C$  C&  C(
=C*
=C,  C-��C/��C1��C4  C6
=C8  C:  C<
=C>
=C@  CB  CD{CF
=CH  CI��CL  CN
=CP  CR  CT  CV  CX  CZ  C[��C]�C`  Cb
=Cd
=Cf  Cg��Cj
=Cl
=Cn  Cp  Cr  Ct
=Cv{Cx
=Cy��C{��C~{C�
=C�C�  C���C�C���C���C�C�C�  C�  C�  C�C�C�  C���C���C���C�  C�C�C�C�  C�  C�C�
=C�C�C�\C�C�C�  C�  C�C�C�C�  C�  C�C�C�C�C�C�  C�  C�C�  C�  C�  C�  C�C�C�
=C�  C�C�
=C�C���C���C���C�C�
=C�
=C�
=C���C���C���C���C���C���C�
=C�
=C�C�
=C�
=C�  C�  C�
=C�
=C�  C���C���C���C���C���C���C���C���C�C�
=C�  C���C�  C�
=C�C�  C���C�C�\C�\C�\C�\C�
=C���C���C�  C���C���C�C�  C�C�C�  C�C���C�  C�  C�C�C��C���C�C�  C�  C�  C�  C�C�C���D }qD �qD� D  Dz�D�qD}qD�qD� D  D��D�D� D�qD��D  D�D	D	� D
  D
� D
�qD�DD��D�D}qD�qD� D  D� D  D� D�qD}qD  D� D�D��D�D� D�D}qD�qD}qD  D}qD  D� D�qD}qD�D��D  D��D�D��D  D}qD  D��D  D}qD   D � D!D!��D!��D"}qD#�D#��D$  D$��D%�D%z�D%�RD&}qD'�D'� D(  D(}qD(��D)� D*�D*��D+  D+� D,  D,� D-  D-��D-�qD.}qD.��D/}qD0  D0� D0�qD1� D2�D2� D3�D3��D3�qD4}qD5  D5}qD5��D6� D7  D7z�D7��D8z�D8��D9}qD9�qD:�D;�D;��D<D<�D=�D=� D>�D>�D?�D?��D@D@� D@��DA}qDA�qDBxRDB��DC�DD�DDxRDD�RDEz�DF  DFz�DG  DG��DH�DH� DH�qDI}qDI�qDJ}qDJ�qDK� DK�qDL� DM  DMz�DM�qDN}qDO  DO}qDO�qDP��DP�qDQ}qDR  DR��DS  DS}qDS��DT� DU  DU��DV�DV}qDV��DW}qDX  DX��DY  DY}qDY�qDZ� D[�D[��D\  D\� D]  D]�D^  D^}qD_  D_�D_�qD`}qD`�qDa� Db�Db� Dc�Dc� Dd�Dd��De  De}qDf  Df� Df�qDgz�Dh  Dh��Di  Di� Dj�Dj��Dk�Dk��Dl  Dl� Dm  Dm��Dn  Dn�DoDo� Do�qDpz�Dp�qDq� Dr  Dr}qDr��Ds��Dt�Dtz�Dt�qDu� Du��Dv� Dw  Dw��Dw��Dx}qDx�qDy}qDz  Dz}qDz�qD{}qD{��D|}qD|��D}}qD}��D~}qDD�D�qD�<)D�~�D�D�HD�AHD�|)D���D�HD�@ D�~�D��HD���D�>�D�� D��HD���D�=qD�~�D�� D�HD�B�D�� D��HD��D�@ D�}qD�� D��D�AHD�}qD��qD��D�@ D�}qD�� D�HD�@ D�}qD�� D��D�AHD�}qD�� D��qD�<)D�� D�D�HD�AHD�}qD�� D��D�>�D�}qD�� D��D�@ D�}qD�� D��qD�AHD���D��qD���D�B�D�~�D�� D�HD�=qD��HD��HD�  D�AHD�}qD�� D���D�@ D�}qD�� D��qD�@ D�~�D�� D�  D�=qD�� D�� D��qD�@ D�� D�� D���D�B�D�� D��)D�HD�@ D�� D�D�  D�AHD�� D�D���D�>�D�~�D���D��D�=qD�~�D�� D���D�AHD�� D�� D��D�=qD�� D�� D���D�AHD�� D�D�  D�@ D�� D��D�  D�@ D���D�D���D�AHD���D�� D��)D�AHD��HD�� D�HD�=qD�~�D��HD�  D�>�D�~�D�D�HD�>�D���D��HD�HD�B�D�~�D���D�  D�@ D�~�D��qD�HD�@ D�~�D��HD���D�=qD��HD��HD�  D�AHD��HD�� D�  D�@ D�~�D���D��D�=qD��HD���D���D�AHD�� D��HD�  D�@ D�� D�D���D�@ D���D�� D�HD�B�D�~�D�� D��qD�@ D�� D�D�HD�@ D�~�D���D�HD�AHD��HD��HD���D�>�D�}qD��HD�  D�@ D�~�D���D�HD�=qD�~�D���D���D�>�D�z�D�� D�HD�>�D��HD�� D�HD�=qD��HD��qD�  D�@ D���D��qD���D�<)D��HD��HD�HD�>�D�~�D��HD��D�B�D���D���D���D�>�D�~�D���D���D�AHD��HD��HD�HD�B�D�� D���D�  D�@ D�~�D�D�  D�AHD D��HD�  D�@ DÁHD�� D���D�@ DāHDľ�D�HD�@ Dŀ Dż)D�  D�AHD�}qD�� D�HD�@ Dǀ D�� D�  D�@ DȀ D�� D���D�@ D�~�DɽqD�  D�@ Dʀ Dʾ�D�  D�B�DˁHD�� D�  D�>�D�~�D��HD�  D�@ D�}qD��HD��D�AHD΁HDνqD��qD�@ Dπ D�D��qD�AHDЀ D�� D�  D�AHDр DѾ�D�  D�>�D�~�D�� D���D�AHD�~�D��HD���D�B�DԂ�D��HD�HD�=qDՀ Dվ�D�HD�>�Dր DֽqD�  D�AHDׁHD��HD���D�>�D�~�Dؾ�D�  D�@ D�~�Dپ�D���D�>�DځHD��HD�  D�@ DہHD�� D�  D�@ D܁HD�D�HD�AHD݀ Dݾ�D���D�AHDހ D�D�HD�AHD�~�D߾�D�HD�=qD�� D�� D���D�>�D� D�� D�  D�AHD�HD⾸D���D�@ D� D��HD��D�AHD� D�D�  D�AHD�HD��HD�HD�AHD� D��HD���D�AHD� D羸D�  D�>�D� D�� D�  D�AHD�HD�� D�HD�>�D� D�� D��qD�>�D� D�qD���D�>�D�~�D��HD�  D�AHD�HD���D���D�B�D�HD�� D�  D�>�D�HD�� D���D�B�D�~�D�D�HD�@ D�~�D�D�HD�@ D� D�D�HD�AHD� D�D�HD�@ D� D�� D���D�>�D�~�D�� D���D�=qD�}qD���D���D�@ D�~�D�� D�  D�>�D�� D�� D���D�>�D�� D�D��D�0�?�?L��?u?��
?\?�(�@   @\)@�R@0��@:�H@L��@\(�@p��@�G�@�=q@�33@�(�@��
@�{@�
=@�G�@˅@�@޸R@���@��@���A�AffA
�HA\)A�
A
=A(�A ��A%�A)��A.{A2�\A7�A<(�AAG�AG
=AJ�HAO\)AS�
AX��A]p�Aa�AfffAk�Amp�Ar�\Aw
=A{�A\)A�=qA�p�A��A�=qA�z�A�
=A���A��HA�z�A�
=A�G�A��A�A�Q�A�=qA�p�A�  A�=qA���A�
=A���A�(�A�{A�  A��\A���A�\)A��A���A�\)A��A���AθRA�G�AӅA��A�\)Aٙ�A��
A�{A�Q�A�33A�p�A�Q�A��HA�A�  A�\A���A�\)A��A��A�A��B�B�B33B(�Bp�B�HB  B	G�B
=qB33B  B�B=qB\)BQ�B�B
=B(�BG�BffB�B(�Bp�BffB\)B��B{B33B Q�B!G�B"{B#\)B$Q�B%p�B&�RB(  B)�B*=qB+\)B,(�B-�B.{B/
=B0(�B1G�B2�RB3�
B4��B5B6�HB7�B8Q�B9G�B:ffB;\)B<��B=�B>�HB@  B@��BA��BB�\BC�BDz�BE�BG
=BH  BHz�BIp�BJffBK\)BL��BMBN�HBO�BPQ�BQp�BR{BS�BTz�BU��BV=qBV�HBX  BX��BY�B[33B\Q�B]�B^=qB_
=B_�B`��BaBb�RBd  BeG�Bf{Bg\)BhQ�Bi�Bi�Bj�HBl  Bl��Bn=qBo�Bp��BqBr�RBs�
Btz�Bup�Bv�\Bw�Bx��By�B{
=B|(�B}�B}�B~�RB�B�ffB�
=B���B�{B���B���B�\)B�  B�ffB�
=B��B�  B��\B�
=B�\)B�B�Q�B���B�p�B�  B�z�B���B�\)B��
B���B��B�B�(�B���B�33B��B���B�G�B��
B�Q�B�
=B���B�ffB��B��
B�=qB��HB��B�=qB�
=B�B�ffB��HB��B�(�B���B�B�ffB��B���B�=qB��HB��B�z�B�33B��
B�Q�B���B��B��\B�33B��B�z�B�
=B���B�z�B�G�B��B�z�B�
=B��B���B�G�B��
B�ffB�
=B��
B��\B�G�B�B�Q�B���B���B�z�B�
=B��B�{B���B�p�B�=qB���B�G�B��B��RB�\)B�{B��\B�33B��
B���B�\)B�B�ffB�33B��B�Q�B���B��
B�z�B���BǅB�(�B���Bə�B�  Bʏ\B�p�B�  B̏\B�
=B͙�B�ffB�
=BυB�{BУ�B�p�B�{B�z�B�
=B��Bԏ\B��HBՙ�B�=qB���BׅB�  Bأ�B�p�B�{Bڏ\B��B�  Bܣ�B��BݮBޏ\B��Bߙ�B�Q�B�
=B�B�=qB�
=B��
B�=qB�
=B��
B�Q�B�
=B�  B��B�33B�{B��HB�\)B�  B���B�B�=qB��HB��
B��B�
=B��
B�RB�\)B�  B��RB���B�=qB��HB�B��\B�
=B��B��RB�p�B�  B��RB���B�=qB��HB�C G�C �C �
CQ�C��C�
C=qC�C��C�HC=qCffC�C��C  C
=C33Cp�Cz�C��C�C{C�CG�CffC�C�
C�HC�CG�CQ�C�CC�HC  C33CffCz�C��C�HC{C�CG�C�C��CC	  C	33C	=qC	ffC	�RC	�
C	�C
(�C
\)C
p�C
��C
�HC
=C{C=qC�C��C�RC
=C33CG�C�C�RC��C��C33CffCp�C��C�C{C(�Cp�C��C�C�C(�C=qCffC�C�
C��C33C\)Cz�C�RC�C��C(�Cp�Cz�C��C�C{C(�Cp�C��C��C�C�C�CQ�C��C�RC�
C�C=qC\)C��C�RC�HC(�CQ�C\)C�C�
C�C33C\)Cp�C�RC�HC��C33CffCp�C�RC�
C��C=qCffCz�C�C�HC��C�CffC�\C�C��C�C(�Cp�C��C�C��C(�C33Cp�C�C�RC��C(�C33Cz�C�C�RC��C(�C=qCp�C�C�RC�C(�C=qCffC�C�RC�C(�C=qCffC��C�
C�C (�C =qC ffC �C �RC!  C!(�C!=qC!ffC!�C!�C!�C"(�C"33C"\)C"��C"�RC"�C#(�C#33C#\)C#��C#�RC#�HC$(�C$33C$ffC$�C$C$��C%=qC%G�C%�\C%�RC%C&
=C&(�C&Q�C&��C&�C&�
C'(�C'=qC'p�C'�RC'��C({C(G�C(\)C(��C(C(�C)=qC)Q�C)��C)C)�
C*(�C*G�C*�\C*C*�
C+33C+=qC+�C+C+�
C,(�C,G�C,p�C,C,�
C-(�C-=qC-z�C-C-�
C.(�C.\)C.p�C.C.�C/{C/ffC/�C/��C0
=C0(�C0p�C0�\C0�RC1{C1�C1p�C1�\C1�
C2
=C2(�C2z�C2�\C2�
C3�C333C3z�C3��C3�
C4{C433C4�C4��C4�
C5�C5=qC5�C5��C5�C6�C6G�C6��C6�C7  C7�C7Q�C7��C7�RC8  C8�C8\)C8�C8�RC9
=C9(�C9\)C9�RC9��C:{C:33C:�C:�RC:��C;{C;=qC;�\C;�C;�HC<33C<=qC<�\C<�C<�C=�C==qC=��C=��C=��C>
=C>\)C>p�C>�C>��C?  C?Q�C?p�C?�C?��C@  C@Q�C@\)C@�C@CA{CA(�CAp�CA��CACB{CB(�CBz�CB�\CB�
CB��CC=qCCffCC�\CC�
CC�HCD33CDQ�CD��CD�CD��CE{CEQ�CE�CE��CE��CF  CFQ�CFp�CF�RCF��CG�CG=qCG�CG��CG�CH  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                          ?u?��H@E�@�  @�  @\@�  A   A��A   A+�AAG�A`��A�  A�  A�\)A�\)A�  A�  A�  A�Q�B   B  B(�B�
B�
B'�
B0(�B8  B@  BH(�BPQ�BW�
B`  BhQ�Bp(�Bx  B�
B��
B�B��
B��B�{B�{B�{B�(�B�{B��B�  B�  B�{B�{B��B��B�{B�{B�{B�  B�B��
B��B��
B��
B��
B�  B�{B�  B�  B�{C 
=C{C
=C  C  C	�C��C
=C{C
=C  C{C{C{C{C
=C 
=C"
=C$  C&  C(
=C*
=C,  C-��C/��C1��C4  C6
=C8  C:  C<
=C>
=C@  CB  CD{CF
=CH  CI��CL  CN
=CP  CR  CT  CV  CX  CZ  C[��C]�C`  Cb
=Cd
=Cf  Cg��Cj
=Cl
=Cn  Cp  Cr  Ct
=Cv{Cx
=Cy��C{��C~{C�
=C�C�  C���C�C���C���C�C�C�  C�  C�  C�C�C�  C���C���C���C�  C�C�C�C�  C�  C�C�
=C�C�C�\C�C�C�  C�  C�C�C�C�  C�  C�C�C�C�C�C�  C�  C�C�  C�  C�  C�  C�C�C�
=C�  C�C�
=C�C���C���C���C�C�
=C�
=C�
=C���C���C���C���C���C���C�
=C�
=C�C�
=C�
=C�  C�  C�
=C�
=C�  C���C���C���C���C���C���C���C���C�C�
=C�  C���C�  C�
=C�C�  C���C�C�\C�\C�\C�\C�
=C���C���C�  C���C���C�C�  C�C�C�  C�C���C�  C�  C�C�C��C���C�C�  C�  C�  C�  C�C�C���D }qD �qD� D  Dz�D�qD}qD�qD� D  D��D�D� D�qD��D  D�D	D	� D
  D
� D
�qD�DD��D�D}qD�qD� D  D� D  D� D�qD}qD  D� D�D��D�D� D�D}qD�qD}qD  D}qD  D� D�qD}qD�D��D  D��D�D��D  D}qD  D��D  D}qD   D � D!D!��D!��D"}qD#�D#��D$  D$��D%�D%z�D%�RD&}qD'�D'� D(  D(}qD(��D)� D*�D*��D+  D+� D,  D,� D-  D-��D-�qD.}qD.��D/}qD0  D0� D0�qD1� D2�D2� D3�D3��D3�qD4}qD5  D5}qD5��D6� D7  D7z�D7��D8z�D8��D9}qD9�qD:�D;�D;��D<D<�D=�D=� D>�D>�D?�D?��D@D@� D@��DA}qDA�qDBxRDB��DC�DD�DDxRDD�RDEz�DF  DFz�DG  DG��DH�DH� DH�qDI}qDI�qDJ}qDJ�qDK� DK�qDL� DM  DMz�DM�qDN}qDO  DO}qDO�qDP��DP�qDQ}qDR  DR��DS  DS}qDS��DT� DU  DU��DV�DV}qDV��DW}qDX  DX��DY  DY}qDY�qDZ� D[�D[��D\  D\� D]  D]�D^  D^}qD_  D_�D_�qD`}qD`�qDa� Db�Db� Dc�Dc� Dd�Dd��De  De}qDf  Df� Df�qDgz�Dh  Dh��Di  Di� Dj�Dj��Dk�Dk��Dl  Dl� Dm  Dm��Dn  Dn�DoDo� Do�qDpz�Dp�qDq� Dr  Dr}qDr��Ds��Dt�Dtz�Dt�qDu� Du��Dv� Dw  Dw��Dw��Dx}qDx�qDy}qDz  Dz}qDz�qD{}qD{��D|}qD|��D}}qD}��D~}qDD�D�qD�<)D�~�D�D�HD�AHD�|)D���D�HD�@ D�~�D��HD���D�>�D�� D��HD���D�=qD�~�D�� D�HD�B�D�� D��HD��D�@ D�}qD�� D��D�AHD�}qD��qD��D�@ D�}qD�� D�HD�@ D�}qD�� D��D�AHD�}qD�� D��qD�<)D�� D�D�HD�AHD�}qD�� D��D�>�D�}qD�� D��D�@ D�}qD�� D��qD�AHD���D��qD���D�B�D�~�D�� D�HD�=qD��HD��HD�  D�AHD�}qD�� D���D�@ D�}qD�� D��qD�@ D�~�D�� D�  D�=qD�� D�� D��qD�@ D�� D�� D���D�B�D�� D��)D�HD�@ D�� D�D�  D�AHD�� D�D���D�>�D�~�D���D��D�=qD�~�D�� D���D�AHD�� D�� D��D�=qD�� D�� D���D�AHD�� D�D�  D�@ D�� D��D�  D�@ D���D�D���D�AHD���D�� D��)D�AHD��HD�� D�HD�=qD�~�D��HD�  D�>�D�~�D�D�HD�>�D���D��HD�HD�B�D�~�D���D�  D�@ D�~�D��qD�HD�@ D�~�D��HD���D�=qD��HD��HD�  D�AHD��HD�� D�  D�@ D�~�D���D��D�=qD��HD���D���D�AHD�� D��HD�  D�@ D�� D�D���D�@ D���D�� D�HD�B�D�~�D�� D��qD�@ D�� D�D�HD�@ D�~�D���D�HD�AHD��HD��HD���D�>�D�}qD��HD�  D�@ D�~�D���D�HD�=qD�~�D���D���D�>�D�z�D�� D�HD�>�D��HD�� D�HD�=qD��HD��qD�  D�@ D���D��qD���D�<)D��HD��HD�HD�>�D�~�D��HD��D�B�D���D���D���D�>�D�~�D���D���D�AHD��HD��HD�HD�B�D�� D���D�  D�@ D�~�D�D�  D�AHD D��HD�  D�@ DÁHD�� D���D�@ DāHDľ�D�HD�@ Dŀ Dż)D�  D�AHD�}qD�� D�HD�@ Dǀ D�� D�  D�@ DȀ D�� D���D�@ D�~�DɽqD�  D�@ Dʀ Dʾ�D�  D�B�DˁHD�� D�  D�>�D�~�D��HD�  D�@ D�}qD��HD��D�AHD΁HDνqD��qD�@ Dπ D�D��qD�AHDЀ D�� D�  D�AHDр DѾ�D�  D�>�D�~�D�� D���D�AHD�~�D��HD���D�B�DԂ�D��HD�HD�=qDՀ Dվ�D�HD�>�Dր DֽqD�  D�AHDׁHD��HD���D�>�D�~�Dؾ�D�  D�@ D�~�Dپ�D���D�>�DځHD��HD�  D�@ DہHD�� D�  D�@ D܁HD�D�HD�AHD݀ Dݾ�D���D�AHDހ D�D�HD�AHD�~�D߾�D�HD�=qD�� D�� D���D�>�D� D�� D�  D�AHD�HD⾸D���D�@ D� D��HD��D�AHD� D�D�  D�AHD�HD��HD�HD�AHD� D��HD���D�AHD� D羸D�  D�>�D� D�� D�  D�AHD�HD�� D�HD�>�D� D�� D��qD�>�D� D�qD���D�>�D�~�D��HD�  D�AHD�HD���D���D�B�D�HD�� D�  D�>�D�HD�� D���D�B�D�~�D�D�HD�@ D�~�D�D�HD�@ D� D�D�HD�AHD� D�D�HD�@ D� D�� D���D�>�D�~�D�� D���D�=qD�}qD���D���D�@ D�~�D�� D�  D�>�D�� D�� D���D�>�D�� D�D��D�0�?�?L��?u?��
?\?�(�@   @\)@�R@0��@:�H@L��@\(�@p��@�G�@�=q@�33@�(�@��
@�{@�
=@�G�@˅@�@޸R@���@��@���A�AffA
�HA\)A�
A
=A(�A ��A%�A)��A.{A2�\A7�A<(�AAG�AG
=AJ�HAO\)AS�
AX��A]p�Aa�AfffAk�Amp�Ar�\Aw
=A{�A\)A�=qA�p�A��A�=qA�z�A�
=A���A��HA�z�A�
=A�G�A��A�A�Q�A�=qA�p�A�  A�=qA���A�
=A���A�(�A�{A�  A��\A���A�\)A��A���A�\)A��A���AθRA�G�AӅA��A�\)Aٙ�A��
A�{A�Q�A�33A�p�A�Q�A��HA�A�  A�\A���A�\)A��A��A�A��B�B�B33B(�Bp�B�HB  B	G�B
=qB33B  B�B=qB\)BQ�B�B
=B(�BG�BffB�B(�Bp�BffB\)B��B{B33B Q�B!G�B"{B#\)B$Q�B%p�B&�RB(  B)�B*=qB+\)B,(�B-�B.{B/
=B0(�B1G�B2�RB3�
B4��B5B6�HB7�B8Q�B9G�B:ffB;\)B<��B=�B>�HB@  B@��BA��BB�\BC�BDz�BE�BG
=BH  BHz�BIp�BJffBK\)BL��BMBN�HBO�BPQ�BQp�BR{BS�BTz�BU��BV=qBV�HBX  BX��BY�B[33B\Q�B]�B^=qB_
=B_�B`��BaBb�RBd  BeG�Bf{Bg\)BhQ�Bi�Bi�Bj�HBl  Bl��Bn=qBo�Bp��BqBr�RBs�
Btz�Bup�Bv�\Bw�Bx��By�B{
=B|(�B}�B}�B~�RB�B�ffB�
=B���B�{B���B���B�\)B�  B�ffB�
=B��B�  B��\B�
=B�\)B�B�Q�B���B�p�B�  B�z�B���B�\)B��
B���B��B�B�(�B���B�33B��B���B�G�B��
B�Q�B�
=B���B�ffB��B��
B�=qB��HB��B�=qB�
=B�B�ffB��HB��B�(�B���B�B�ffB��B���B�=qB��HB��B�z�B�33B��
B�Q�B���B��B��\B�33B��B�z�B�
=B���B�z�B�G�B��B�z�B�
=B��B���B�G�B��
B�ffB�
=B��
B��\B�G�B�B�Q�B���B���B�z�B�
=B��B�{B���B�p�B�=qB���B�G�B��B��RB�\)B�{B��\B�33B��
B���B�\)B�B�ffB�33B��B�Q�B���B��
B�z�B���BǅB�(�B���Bə�B�  Bʏ\B�p�B�  B̏\B�
=B͙�B�ffB�
=BυB�{BУ�B�p�B�{B�z�B�
=B��Bԏ\B��HBՙ�B�=qB���BׅB�  Bأ�B�p�B�{Bڏ\B��B�  Bܣ�B��BݮBޏ\B��Bߙ�B�Q�B�
=B�B�=qB�
=B��
B�=qB�
=B��
B�Q�B�
=B�  B��B�33B�{B��HB�\)B�  B���B�B�=qB��HB��
B��B�
=B��
B�RB�\)B�  B��RB���B�=qB��HB�B��\B�
=B��B��RB�p�B�  B��RB���B�=qB��HB�C G�C �C �
CQ�C��C�
C=qC�C��C�HC=qCffC�C��C  C
=C33Cp�Cz�C��C�C{C�CG�CffC�C�
C�HC�CG�CQ�C�CC�HC  C33CffCz�C��C�HC{C�CG�C�C��CC	  C	33C	=qC	ffC	�RC	�
C	�C
(�C
\)C
p�C
��C
�HC
=C{C=qC�C��C�RC
=C33CG�C�C�RC��C��C33CffCp�C��C�C{C(�Cp�C��C�C�C(�C=qCffC�C�
C��C33C\)Cz�C�RC�C��C(�Cp�Cz�C��C�C{C(�Cp�C��C��C�C�C�CQ�C��C�RC�
C�C=qC\)C��C�RC�HC(�CQ�C\)C�C�
C�C33C\)Cp�C�RC�HC��C33CffCp�C�RC�
C��C=qCffCz�C�C�HC��C�CffC�\C�C��C�C(�Cp�C��C�C��C(�C33Cp�C�C�RC��C(�C33Cz�C�C�RC��C(�C=qCp�C�C�RC�C(�C=qCffC�C�RC�C(�C=qCffC��C�
C�C (�C =qC ffC �C �RC!  C!(�C!=qC!ffC!�C!�C!�C"(�C"33C"\)C"��C"�RC"�C#(�C#33C#\)C#��C#�RC#�HC$(�C$33C$ffC$�C$C$��C%=qC%G�C%�\C%�RC%C&
=C&(�C&Q�C&��C&�C&�
C'(�C'=qC'p�C'�RC'��C({C(G�C(\)C(��C(C(�C)=qC)Q�C)��C)C)�
C*(�C*G�C*�\C*C*�
C+33C+=qC+�C+C+�
C,(�C,G�C,p�C,C,�
C-(�C-=qC-z�C-C-�
C.(�C.\)C.p�C.C.�C/{C/ffC/�C/��C0
=C0(�C0p�C0�\C0�RC1{C1�C1p�C1�\C1�
C2
=C2(�C2z�C2�\C2�
C3�C333C3z�C3��C3�
C4{C433C4�C4��C4�
C5�C5=qC5�C5��C5�C6�C6G�C6��C6�C7  C7�C7Q�C7��C7�RC8  C8�C8\)C8�C8�RC9
=C9(�C9\)C9�RC9��C:{C:33C:�C:�RC:��C;{C;=qC;�\C;�C;�HC<33C<=qC<�\C<�C<�C=�C==qC=��C=��C=��C>
=C>\)C>p�C>�C>��C?  C?Q�C?p�C?�C?��C@  C@Q�C@\)C@�C@CA{CA(�CAp�CA��CACB{CB(�CBz�CB�\CB�
CB��CC=qCCffCC�\CC�
CC�HCD33CDQ�CD��CD�CD��CE{CEQ�CE�CE��CE��CF  CFQ�CFp�CF�RCF��CG�CG=qCG�CG��CG�CH  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                          G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�
=A�VA�VA�oA�oA�oA�oA�{A�{A�{A�{A�{A�{A�1A�A���A�%A�oA��A�{A�bA�bA�bA�oA��A��A��A��A��A��A��A��A��A��A��A� �A�A��TA��
A͙�A�XA�=qA͑hA���A�ĜA͏\A��/Ả7A˙�Aʛ�A�5?AɋDA�JA�O�A¸RA�A���A��;A�p�A���A�  A���A�$�A�dZA��\A�z�A���A�VA�\)A��A�33A�;dA��\A�(�A��A�O�A�x�A�;dA���A�K�A�VA���A�A�A�A�{A�1A�O�A��!A�K�A��yA�ȴA�ĜA��A�E�A���A���A��A��A}|�Az$�AvbAq�FAk��Ah�Ac&�A]��A\��AYl�AU�ANI�AH^5AD��A>�\A<��A;��A9�A9?}A7�
A5XA4Q�A2��A0n�A/��A/S�A-�#A+�A*�!A*z�A*I�A*~�A+�A-C�A-��A-��A,��A+\)A+l�A+�^A+ƨA+\)A+�A*n�A)�hA(ĜA($�A'��A'�A'dZA&��A$ȴA$5?A#�TA#�
A#�PA"��A"ZA!�;A!�FA!�#A"�A$bNA$��A#��A#�A"��A"M�A"5?A!�A �uA/A�A��AĜA��AffA^5Av�AbA��AdZA7LA�mA%A�`A�\A1'A�A�9Av�A=qA1A�#A�^AK�A�`A�\A�A33A�\A�A�A�^Av�A�wAt�A`BA?}A�A�A�`AQ�A7LA^5AC�A
�A
ffA	��A	oA��A��AG�AO�A�A��A��A��A~�A9XA{A  A�A�mA��A�AG�AoA�A�+A�#A�^A��AC�AQ�AK�A �A v�@���@�33@�@���@�~�@��@�Q�@���@��R@�M�@��-@�G�@��`@��D@���@��P@�
=@��y@��H@���@�$�@��@�p�@�Ĝ@�9X@��
@�ȴ@���@�@��@�@�j@�@��y@�\@���@�X@�bN@�w@�w@�  @�dZ@ꟾ@�=q@���@�u@�+@�ȴ@��#@� �@�S�@�o@���@�$�@�-@�h@�`B@�/@���@��
@ߍP@ߕ�@߅@�t�@�\)@�;d@�o@�V@ݩ�@��@���@�Ĝ@ܴ9@ܬ@ܬ@܋D@��
@���@��#@�G�@���@���@�r�@�l�@�M�@ա�@�(�@Ӆ@�t�@�K�@�ȴ@�5?@�?}@���@�Q�@���@�"�@���@��#@���@̼j@�A�@�b@��@˅@ʰ!@�@�X@�j@ǶF@�`B@�&�@��/@�r�@�(�@��;@þw@Õ�@�K�@���@�V@�@�O�@�V@���@� �@���@�"�@��H@���@��R@��\@�$�@���@��-@�x�@���@�1'@��@�;d@��y@�ȴ@���@��7@�&�@��@��/@��9@�\)@��y@���@��@�?}@�/@�/@��@��`@�Ĝ@�j@�A�@��@���@��;@���@��F@�t�@�-@�x�@��/@��@���@���@�|�@�dZ@�;d@�@��@�~�@��@�/@��D@�I�@��w@���@��P@�S�@�C�@��@���@���@�^5@�J@���@��u@���@���@�K�@�"�@���@�v�@�$�@�X@�V@���@�1@��@���@�@���@�X@�/@��j@��@�j@�Q�@�b@��m@��F@��P@�\)@�@��@���@���@���@�{@���@���@�A�@��@��@��@�l�@�S�@�33@��@�$�@��@�7L@���@��@�b@��@��m@��m@�ƨ@���@�"�@��H@���@�n�@��#@�hs@�%@��`@���@���@�r�@�I�@�1'@��@��@��
@��w@�33@�ff@��^@��@�/@���@���@�bN@��
@�"�@��R@�~�@�v�@�V@�{@��@��-@��7@�x�@�`B@��@�r�@�t�@�+@�@���@�^5@�5?@�5?@�-@�@��^@��@��@�9X@�1'@��@���@�S�@�
=@���@���@�~�@�V@��h@�/@��`@���@��@�I�@�t�@�
=@��@���@��\@�E�@��@�{@�@��@��^@��@�G�@��@���@��D@�j@�1'@�;@l�@~��@~E�@~{@}�T@}p�@|�/@|�@|��@|�D@|�D@|z�@|z�@|Z@|I�@|(�@|�@|1@{��@{�m@{�
@{33@z�\@zM�@zJ@yX@wl�@u�@u�-@up�@uV@t�/@t�@tz�@st�@s@r��@r��@r-@q��@q��@qhs@qX@qX@qX@qX@q&�@q%@p��@pĜ@p�u@p�u@pbN@pA�@o|�@o+@o�@nff@m@m/@l9X@kC�@j��@j~�@j�@i�@i�@i�@i�#@i�#@i��@i�^@i%@g|�@f�+@f@e@eV@c��@c33@bM�@b=q@b-@bJ@ax�@`Ĝ@`r�@`r�@`A�@` �@_�@_�P@_;d@]�-@[ƨ@Zn�@ZJ@ZJ@Y�@Y�7@Y7L@X�9@Xr�@XQ�@XQ�@XA�@X1'@X �@Xb@W�@V�R@V{@U�@T�@T�j@T��@Tz�@Tj@SS�@R-@P��@O�w@Ol�@O;d@O+@N��@Nv�@Lz�@K@J�H@J��@J��@J�\@JJ@I�7@I&�@H��@G�@GK�@Fȴ@Fv�@FV@F@E��@E`B@D�@D�D@D�@Ct�@B�!@@�9@?��@?\)@?
=@>�R@>��@>v�@>E�@>{@=�@=�T@=�-@=O�@<�/@<�D@<j@<I�@<(�@;�m@;��@;S�@:��@9�#@9x�@9&�@8�`@8�@8Q�@8 �@7�@7\)@6�y@6�R@6��@6v�@6E�@6{@5��@5�-@5��@5`B@4Z@3ƨ@3S�@2�\@2�@1��@1�#@1�^@1x�@0��@0bN@0A�@/|�@.�y@.�@.�@.��@.5?@.$�@.$�@.$�@.{@.{@-@-@-�-@-��@-�@-?}@-/@,�@,��@,�@,Z@+��@+dZ@*��@*-@)�@)�@)��@)��@)hs@)7L@)%@(�u@(Q�@'�@'��@'�w@'��@'��@'��@&��@&�y@&��@&�@&�+@&ff@&V@&$�@%�T@%p�@%O�@%V@$�j@$I�@#�m@#�F@#t�@#S�@#S�@#33@#33@#@"�@"�@"�@"�!@"n�@"M�@"=q@"-@"J@!X@ �9@ �@ bN@ b@�;@�@�w@��@�P@�P@|�@|�@K�@�@ȴ@$�@�-@�@/@��@9X@�F@C�@��@~�@n�@n�@^5@^5@M�@J@J@��@��@�#@�^@hs@7L@&�@%@�9@Q�@ �@�@|�@|�@|�@�P@l�@;d@�@��@�+@V@V@5?@5?@{@�@��@��@�-@?}@�@��@�@�@j@I�@�m@33@�H@��@�!@�!@��@�\@~�@n�@M�@=q@=q@�@�@��@��@r�@bN@Q�@A�@�@��@�P@��@��@�P@l�@K�@K�@�@��@�y@�R@�+@ff@E�@5?@@�T@@p�@�@�@�@��@��@�j@�D@Z@(�@��@ƨ@��@t�@dZ@33@@
�@
�H@
��@
~�@
M�@
-@
�@
�@
J@	�#@	��@	�7@	G�@	&�@	�@	�@��@�`@bN@  @��@�P@l�@l�@l�A�A�%A�VA�JA�JA�VA�JA�
=A�
=A�JA�oA�bA�bA�oA�oA�{A�{A�{A�{A�{A�{A�{A�oA�oA�oA�oA�oA�oA�bA�bA�oA�oA�{A�{A��A��A��A��A��A��A��A��A�{A�oA�oA�oA�oA�oA�oA�{A�oA�oA�{A�
=A�VA�JA�
=A�A�A�A�A���A�  A�  A�  A�  A�  A�  A�  A�A�  A���A���A���A���A���A���A�JA�JA�JA�bA�bA�{A�VA��A��A��A��A��A��A��A��A��A��A��A��A��A�{A�oA�{A�oA�bA�VA�VA�VA�VA�JA�JA�VA�VA�bA�bA�oA�{A�oA�oA�bA�VA�VA�bA�bA�bA�oA�oA�{A�{A��A��A��A�{A�{A�{A��A��A��A��A��A�{A�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A� �A� �A��A��A��A��A��A��A��A��A��A��A��A��A� �A��A��A��A��A��A��A��A� �A��A��A��A��A��A��A� �A� �A� �A� �A��A��A��A��A��A��A��A� �A� �A� �A� �A� �A��A��A��A��A�JA�A�%A�1A�A�A��A��yA��mA��yA��TA��TA��TA��;A��
A��
A��
A��A��A��
A���A��
A��
A���A��
A���A͟�A�t�A�t�A�p�A�hsA�bNA�bNA�dZA�dZA�^5A�S�A�G�A�C�A�A�A�E�A�G�A�G�A�E�A�7LA�{A�
=A�E�AͅA�ƨA��A��A��`A��;A���A���A�A�ĜA�Aͺ^AͲ-A�ƨA�ȴA��
A��/Aͺ^Aʹ9A�t�A�ffA�ZA�5?A�oA��A̼jA̡�A̝�A̓uA̋DẢ7Ȧ+A̅A�v�A�?}A�  A˼jA�^5A�bA�ĜA�z�A�|�Aʏ\Aʲ-A���Aʴ9Aʩ�Aʇ+A�hsA�{AɼjA���A��#A�ȴAɺ^Aɟ�A�=qA�1'A� �A��#AȾwA�S�A�ȴA�O�A�bNA���A���A�n�Aò-A�~�AÁAÁA�O�A´9A�jA�A�A��A��-A�M�A��yA���A�\)A�&�A�ȴA�/A�  A��A��RA���A���A��\A��+A�;dA��A�t�A��#A�7LA��yA���A�bNA�(�A���A��/A���A�VA���A�dZA���A�O�A�/A��A�JA���A��A��TA��A���A�t�A�"�A�Q�A��\A���A�K�A���A�+A���A�XA�-A�C�A���A�1A��
A�ƨA��wA��-A���A���A�|�A�E�A�S�A�M�A�O�A��RA��wA�t�A�+A���A�ȴA���A�?}A�$�A�bA��7A��yA�A��-A�^5A��TA�C�A��mA�O�A��A�A�A��A��A�|�A�t�A�hsA�ffA�bNA�XA� �A��;A���A��\A��DA��A�bNA�G�A�/A��A�JA�JA�  A��A���A�A��9A���A���A���A���A��\A��+A��A�|�A�hsA�O�A�7LA�%A��A��!A��hA�hsA�=qA�A�A�~�A�bNA�I�A��A��A���A���A�jA�;dA��A���A���A��hA�M�A��A��A��9A��hA�z�A�hsA�XA�I�A�;dA�+A��A�JA���A��A�ȴA��FA��A���A���A���A���A��7A�hsA�(�A��A�5?A��yA��!A�I�A��A�
=A���A��A���A��A��A��A��A��TA��/A��/A��
A�ĜA�ĜA��^A��9A��FA��-A��A���A�9XA��PA�I�A��A�1A��A���A��FA���A��+A�v�A�dZA�VA�C�A�(�A�A��A��TA��/A���A���A�ȴA���A��^A��^A��-A��A��A���A���A���A���A��PA�p�A�33A���A���A�9XA��A��A���A���A�ffA�/A���A���A��-A���A��A�x�A�ffA�K�A�(�A�A��`A���A��RA���A��A�l�A�\)A�I�A�33A�VA���A��TA��9A��DA�l�A�G�A�&�A��A�{A�VA�%A�  A�A���A��mA��`A��A�ȴA��FA���A��+A�x�A�jA�`BA�\)A�I�A�bA��HA���A�\)A�33A��A�VA�1A��A���A���A�t�A�5?A��
A�^5A��
A��A��A�`BA�O�A�G�A�A�A�5?A�VA��mA���A���A��hA�bNA��A��A���A��^A���A��hA�v�A�C�A�{A�A��A��`A���A��^A��9A���A��hA�x�A�dZA�7LA� �A���A���A���A��PA�p�A�S�A�A�A�
=A���A��hA�1'A��HA��uA�VA�&�A�ƨA�|�A�bNA�S�A�O�A�G�A�C�A�C�A�9XA�1'A�-A�$�A�"�A��A�bA�1A�A���A��;A���A���A�ȴA�ȴA�ƨA��RA���A���A���A���A�|�A�hsA�I�A�(�A��A��^A��\A�I�A��/A�XA��A���A�S�A���A��RA�ffA�
=A�ȴA�jA��A\)A~��A~A�A~JA}ƨA}��A}`BA}C�A}
=A|�A|��A|-A{A{S�Az�jAz�AyAy�-Ayt�Ax��Ax�Aw�TAw?}Av�Av��AvI�Av �AuAux�Au�Atr�As��AsAr��Ar�\Arz�Aq��AqC�Ap��Ap5?Ao`BAn��An1'Am?}Al�\Ak�mAkp�Aj��Aj�+AjM�Aj1'AjbAi�Ai��Ai�^Ai�Ai?}Ah�Ah��Ah5?Ag�mAg��Ag�AfffAeO�Ad1'Ac��Ab��Ab{Aa\)A`��A_�TA_&�A^ȴA^A�A^A]��A]�7G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                          A�
=A�VA�VA�oA�oA�oA�oA�{A�{A�{A�{A�{A�{A�1A�A���A�%A�oA��A�{A�bA�bA�bA�oA��A��A��A��A��A��A��A��A��A��A��A� �A�A��TA��
A͙�A�XA�=qA͑hA���A�ĜA͏\A��/Ả7A˙�Aʛ�A�5?AɋDA�JA�O�A¸RA�A���A��;A�p�A���A�  A���A�$�A�dZA��\A�z�A���A�VA�\)A��A�33A�;dA��\A�(�A��A�O�A�x�A�;dA���A�K�A�VA���A�A�A�A�{A�1A�O�A��!A�K�A��yA�ȴA�ĜA��A�E�A���A���A��A��A}|�Az$�AvbAq�FAk��Ah�Ac&�A]��A\��AYl�AU�ANI�AH^5AD��A>�\A<��A;��A9�A9?}A7�
A5XA4Q�A2��A0n�A/��A/S�A-�#A+�A*�!A*z�A*I�A*~�A+�A-C�A-��A-��A,��A+\)A+l�A+�^A+ƨA+\)A+�A*n�A)�hA(ĜA($�A'��A'�A'dZA&��A$ȴA$5?A#�TA#�
A#�PA"��A"ZA!�;A!�FA!�#A"�A$bNA$��A#��A#�A"��A"M�A"5?A!�A �uA/A�A��AĜA��AffA^5Av�AbA��AdZA7LA�mA%A�`A�\A1'A�A�9Av�A=qA1A�#A�^AK�A�`A�\A�A33A�\A�A�A�^Av�A�wAt�A`BA?}A�A�A�`AQ�A7LA^5AC�A
�A
ffA	��A	oA��A��AG�AO�A�A��A��A��A~�A9XA{A  A�A�mA��A�AG�AoA�A�+A�#A�^A��AC�AQ�AK�A �A v�@���@�33@�@���@�~�@��@�Q�@���@��R@�M�@��-@�G�@��`@��D@���@��P@�
=@��y@��H@���@�$�@��@�p�@�Ĝ@�9X@��
@�ȴ@���@�@��@�@�j@�@��y@�\@���@�X@�bN@�w@�w@�  @�dZ@ꟾ@�=q@���@�u@�+@�ȴ@��#@� �@�S�@�o@���@�$�@�-@�h@�`B@�/@���@��
@ߍP@ߕ�@߅@�t�@�\)@�;d@�o@�V@ݩ�@��@���@�Ĝ@ܴ9@ܬ@ܬ@܋D@��
@���@��#@�G�@���@���@�r�@�l�@�M�@ա�@�(�@Ӆ@�t�@�K�@�ȴ@�5?@�?}@���@�Q�@���@�"�@���@��#@���@̼j@�A�@�b@��@˅@ʰ!@�@�X@�j@ǶF@�`B@�&�@��/@�r�@�(�@��;@þw@Õ�@�K�@���@�V@�@�O�@�V@���@� �@���@�"�@��H@���@��R@��\@�$�@���@��-@�x�@���@�1'@��@�;d@��y@�ȴ@���@��7@�&�@��@��/@��9@�\)@��y@���@��@�?}@�/@�/@��@��`@�Ĝ@�j@�A�@��@���@��;@���@��F@�t�@�-@�x�@��/@��@���@���@�|�@�dZ@�;d@�@��@�~�@��@�/@��D@�I�@��w@���@��P@�S�@�C�@��@���@���@�^5@�J@���@��u@���@���@�K�@�"�@���@�v�@�$�@�X@�V@���@�1@��@���@�@���@�X@�/@��j@��@�j@�Q�@�b@��m@��F@��P@�\)@�@��@���@���@���@�{@���@���@�A�@��@��@��@�l�@�S�@�33@��@�$�@��@�7L@���@��@�b@��@��m@��m@�ƨ@���@�"�@��H@���@�n�@��#@�hs@�%@��`@���@���@�r�@�I�@�1'@��@��@��
@��w@�33@�ff@��^@��@�/@���@���@�bN@��
@�"�@��R@�~�@�v�@�V@�{@��@��-@��7@�x�@�`B@��@�r�@�t�@�+@�@���@�^5@�5?@�5?@�-@�@��^@��@��@�9X@�1'@��@���@�S�@�
=@���@���@�~�@�V@��h@�/@��`@���@��@�I�@�t�@�
=@��@���@��\@�E�@��@�{@�@��@��^@��@�G�@��@���@��D@�j@�1'@�;@l�@~��@~E�@~{@}�T@}p�@|�/@|�@|��@|�D@|�D@|z�@|z�@|Z@|I�@|(�@|�@|1@{��@{�m@{�
@{33@z�\@zM�@zJ@yX@wl�@u�@u�-@up�@uV@t�/@t�@tz�@st�@s@r��@r��@r-@q��@q��@qhs@qX@qX@qX@qX@q&�@q%@p��@pĜ@p�u@p�u@pbN@pA�@o|�@o+@o�@nff@m@m/@l9X@kC�@j��@j~�@j�@i�@i�@i�@i�#@i�#@i��@i�^@i%@g|�@f�+@f@e@eV@c��@c33@bM�@b=q@b-@bJ@ax�@`Ĝ@`r�@`r�@`A�@` �@_�@_�P@_;d@]�-@[ƨ@Zn�@ZJ@ZJ@Y�@Y�7@Y7L@X�9@Xr�@XQ�@XQ�@XA�@X1'@X �@Xb@W�@V�R@V{@U�@T�@T�j@T��@Tz�@Tj@SS�@R-@P��@O�w@Ol�@O;d@O+@N��@Nv�@Lz�@K@J�H@J��@J��@J�\@JJ@I�7@I&�@H��@G�@GK�@Fȴ@Fv�@FV@F@E��@E`B@D�@D�D@D�@Ct�@B�!@@�9@?��@?\)@?
=@>�R@>��@>v�@>E�@>{@=�@=�T@=�-@=O�@<�/@<�D@<j@<I�@<(�@;�m@;��@;S�@:��@9�#@9x�@9&�@8�`@8�@8Q�@8 �@7�@7\)@6�y@6�R@6��@6v�@6E�@6{@5��@5�-@5��@5`B@4Z@3ƨ@3S�@2�\@2�@1��@1�#@1�^@1x�@0��@0bN@0A�@/|�@.�y@.�@.�@.��@.5?@.$�@.$�@.$�@.{@.{@-@-@-�-@-��@-�@-?}@-/@,�@,��@,�@,Z@+��@+dZ@*��@*-@)�@)�@)��@)��@)hs@)7L@)%@(�u@(Q�@'�@'��@'�w@'��@'��@'��@&��@&�y@&��@&�@&�+@&ff@&V@&$�@%�T@%p�@%O�@%V@$�j@$I�@#�m@#�F@#t�@#S�@#S�@#33@#33@#@"�@"�@"�@"�!@"n�@"M�@"=q@"-@"J@!X@ �9@ �@ bN@ b@�;@�@�w@��@�P@�P@|�@|�@K�@�@ȴ@$�@�-@�@/@��@9X@�F@C�@��@~�@n�@n�@^5@^5@M�@J@J@��@��@�#@�^@hs@7L@&�@%@�9@Q�@ �@�@|�@|�@|�@�P@l�@;d@�@��@�+@V@V@5?@5?@{@�@��@��@�-@?}@�@��@�@�@j@I�@�m@33@�H@��@�!@�!@��@�\@~�@n�@M�@=q@=q@�@�@��@��@r�@bN@Q�@A�@�@��@�P@��@��@�P@l�@K�@K�@�@��@�y@�R@�+@ff@E�@5?@@�T@@p�@�@�@�@��@��@�j@�D@Z@(�@��@ƨ@��@t�@dZ@33@@
�@
�H@
��@
~�@
M�@
-@
�@
�@
J@	�#@	��@	�7@	G�@	&�@	�@	�@��@�`@bN@  @��@�P@l�@l�@l�A�A�%A�VA�JA�JA�VA�JA�
=A�
=A�JA�oA�bA�bA�oA�oA�{A�{A�{A�{A�{A�{A�{A�oA�oA�oA�oA�oA�oA�bA�bA�oA�oA�{A�{A��A��A��A��A��A��A��A��A�{A�oA�oA�oA�oA�oA�oA�{A�oA�oA�{A�
=A�VA�JA�
=A�A�A�A�A���A�  A�  A�  A�  A�  A�  A�  A�A�  A���A���A���A���A���A���A�JA�JA�JA�bA�bA�{A�VA��A��A��A��A��A��A��A��A��A��A��A��A��A�{A�oA�{A�oA�bA�VA�VA�VA�VA�JA�JA�VA�VA�bA�bA�oA�{A�oA�oA�bA�VA�VA�bA�bA�bA�oA�oA�{A�{A��A��A��A�{A�{A�{A��A��A��A��A��A�{A�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A� �A� �A��A��A��A��A��A��A��A��A��A��A��A��A� �A��A��A��A��A��A��A��A� �A��A��A��A��A��A��A� �A� �A� �A� �A��A��A��A��A��A��A��A� �A� �A� �A� �A� �A��A��A��A��A�JA�A�%A�1A�A�A��A��yA��mA��yA��TA��TA��TA��;A��
A��
A��
A��A��A��
A���A��
A��
A���A��
A���A͟�A�t�A�t�A�p�A�hsA�bNA�bNA�dZA�dZA�^5A�S�A�G�A�C�A�A�A�E�A�G�A�G�A�E�A�7LA�{A�
=A�E�AͅA�ƨA��A��A��`A��;A���A���A�A�ĜA�Aͺ^AͲ-A�ƨA�ȴA��
A��/Aͺ^Aʹ9A�t�A�ffA�ZA�5?A�oA��A̼jA̡�A̝�A̓uA̋DẢ7Ȧ+A̅A�v�A�?}A�  A˼jA�^5A�bA�ĜA�z�A�|�Aʏ\Aʲ-A���Aʴ9Aʩ�Aʇ+A�hsA�{AɼjA���A��#A�ȴAɺ^Aɟ�A�=qA�1'A� �A��#AȾwA�S�A�ȴA�O�A�bNA���A���A�n�Aò-A�~�AÁAÁA�O�A´9A�jA�A�A��A��-A�M�A��yA���A�\)A�&�A�ȴA�/A�  A��A��RA���A���A��\A��+A�;dA��A�t�A��#A�7LA��yA���A�bNA�(�A���A��/A���A�VA���A�dZA���A�O�A�/A��A�JA���A��A��TA��A���A�t�A�"�A�Q�A��\A���A�K�A���A�+A���A�XA�-A�C�A���A�1A��
A�ƨA��wA��-A���A���A�|�A�E�A�S�A�M�A�O�A��RA��wA�t�A�+A���A�ȴA���A�?}A�$�A�bA��7A��yA�A��-A�^5A��TA�C�A��mA�O�A��A�A�A��A��A�|�A�t�A�hsA�ffA�bNA�XA� �A��;A���A��\A��DA��A�bNA�G�A�/A��A�JA�JA�  A��A���A�A��9A���A���A���A���A��\A��+A��A�|�A�hsA�O�A�7LA�%A��A��!A��hA�hsA�=qA�A�A�~�A�bNA�I�A��A��A���A���A�jA�;dA��A���A���A��hA�M�A��A��A��9A��hA�z�A�hsA�XA�I�A�;dA�+A��A�JA���A��A�ȴA��FA��A���A���A���A���A��7A�hsA�(�A��A�5?A��yA��!A�I�A��A�
=A���A��A���A��A��A��A��A��TA��/A��/A��
A�ĜA�ĜA��^A��9A��FA��-A��A���A�9XA��PA�I�A��A�1A��A���A��FA���A��+A�v�A�dZA�VA�C�A�(�A�A��A��TA��/A���A���A�ȴA���A��^A��^A��-A��A��A���A���A���A���A��PA�p�A�33A���A���A�9XA��A��A���A���A�ffA�/A���A���A��-A���A��A�x�A�ffA�K�A�(�A�A��`A���A��RA���A��A�l�A�\)A�I�A�33A�VA���A��TA��9A��DA�l�A�G�A�&�A��A�{A�VA�%A�  A�A���A��mA��`A��A�ȴA��FA���A��+A�x�A�jA�`BA�\)A�I�A�bA��HA���A�\)A�33A��A�VA�1A��A���A���A�t�A�5?A��
A�^5A��
A��A��A�`BA�O�A�G�A�A�A�5?A�VA��mA���A���A��hA�bNA��A��A���A��^A���A��hA�v�A�C�A�{A�A��A��`A���A��^A��9A���A��hA�x�A�dZA�7LA� �A���A���A���A��PA�p�A�S�A�A�A�
=A���A��hA�1'A��HA��uA�VA�&�A�ƨA�|�A�bNA�S�A�O�A�G�A�C�A�C�A�9XA�1'A�-A�$�A�"�A��A�bA�1A�A���A��;A���A���A�ȴA�ȴA�ƨA��RA���A���A���A���A�|�A�hsA�I�A�(�A��A��^A��\A�I�A��/A�XA��A���A�S�A���A��RA�ffA�
=A�ȴA�jA��A\)A~��A~A�A~JA}ƨA}��A}`BA}C�A}
=A|�A|��A|-A{A{S�Az�jAz�AyAy�-Ayt�Ax��Ax�Aw�TAw?}Av�Av��AvI�Av �AuAux�Au�Atr�As��AsAr��Ar�\Arz�Aq��AqC�Ap��Ap5?Ao`BAn��An1'Am?}Al�\Ak�mAkp�Aj��Aj�+AjM�Aj1'AjbAi�Ai��Ai�^Ai�Ai?}Ah�Ah��Ah5?Ag�mAg��Ag�AfffAeO�Ad1'Ac��Ab��Ab{Aa\)A`��A_�TA_&�A^ȴA^A�A^A]��A]�7G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                          G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	��B	�_B	��B	��B	�+B	�_B	�+B	�+B	�_B	�_B	�_B	�+B	�+B	�_B	�+B	�_B	��B	�_B	�_B	�+B	�YB	�YB	��B	�YB	��B	��B	��B	��B	��B	�+B	��B	��B	��B	��B	��B	�YB	��B	�YB	�7B	�B	��B
jB?B��B�?BбB�jB�WB��B� B�BC�By�Bo�BpBz�Bv�BsB�(B�hBo�Bv�BqABncBaBz�B��B�rB|�BFtB2�B'�BIB�B�B�B��B��B�hB� BzBo�B[#B?HB,�BB�B
�	B
ںB
�B
��B
�B
�JB
oiB
i�B
bNB
R B
,�B
SB	�B	�B	�}B	�B	��B	n�B	L�B	A�B	6zB	#:B	.B�)B��B�pB��B��B��B�6B�}BȴB�zB��B�B�dB��B�6B�BӏB�mBچB�fB	B	:�B	h>B	p�B	u%B	u�B	�B	�SB	��B	ϫB	�B	�cB	�B	��B	��B	�B	� B	�fB	�rB	��B	��B	�(B
  B	�.B
  B	�(B
;B
�B
�B
#�B
M�B
X�B
YKB
Y�B
XEB
VmB
V9B
V�B
W�B
T�B
P}B
RTB
TaB
U�B
U�B
XEB
a�B
b�B
`B
_pB
\�B
_B
XB
WsB
V�B
U2B
R�B
P�B
N�B
NB
L�B
K^B
J�B
IRB
F�B
EB
C�B
>wB
?B
9�B
:�B
4�B
2-B
-CB
,B
+6B
+6B
*0B
)�B
(XB
)_B
'�B
&LB
$@B
"hB
 'B
 'B
�B
�B
�B
�B
#�B
 �B
�B
 �B
 �B
!�B
!�B
 �B
 �B
 \B
 'B
 �B
 �B
 �B
"�B
$@B
$B
#�B
#nB
#B
!�B
!�B
�B
�B
�B
MB
�B
hB
�B
�B
.B
 B
VB
�B
�B
�B
B
PB
�B
VB
�B
�B
"B
"B
VB
"B
�B
�B
PB
�B
JB
(B
�B
"B
�B
VB
"B
�B
"B
PB
�B
�B
�B
B
MB
B
~B
B
xB
�B
 \B
OB
�B
�B
_B
�B
�B
SB
�B
�B
�B
�B
�B
�B
SB
FB
�B
{B
FB
FB
�B
�B
eB
_B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
:B
B
�B
�B
4B
�B
�B
"B

�B

rB
	�B
B
	7B

rB
�B
1B
�B
%B
�B
�B
{B
B
{B
B
�B
uB
B
�B
 4B
uB
�B
�B
B
�B
�B
B
MB
�B
�B
{B
B
{B
B
AB
�B
uB
�B
�B
�B
uB
uB
uB
�B
B
B
B
B
�B
B
�B
B
�B
�B
�B
�B
�B
�B
�B
SB
�B
B
�B
%B
B
�B
�B
�B
�B
MB
�B
�B
{B
{B
�B
�B
B
�B
;B	��B
 4B
 iB
B
B
B
uB
B
GB
GB
GB
GB
�B
�B
oB
�B
 �B
 �B
 �B
 iB
 iB
 �B
 �B
 �B
 �B
B
�B
B
uB
�B
uB
�B
B
uB
{B
{B
{B
�B
SB
�B
�B
�B
�B
1B
	lB
	lB
	�B

	B

�B

�B
xB
xB
B
PB
�B
"B
�B
�B
�B
�B
hB
:B
�B
B
@B
@B
@B
@B
B
B
�B
�B
�B
�B
_B
_B
_B
�B
_B
_B
�B
�B
B
�B
qB
�B
xB
CB
CB
�B
�B
IB
B
IB
~B
IB
IB
B
IB
�B
~B
OB
OB
!B
 'B
!�B
"4B
#nB
#�B
#�B
#�B
$tB
$tB
$tB
$@B
$tB
$B
#�B
%FB
&�B
&�B
&�B
'�B
'�B
($B
(XB
'�B
(XB
(�B
(�B
*�B
*eB
*0B
*�B
*�B
*�B
+�B
+kB
+6B
+�B
+�B
-�B
-�B
.B
.}B
.}B
.�B
0�B
0�B
0�B
0�B
1�B
2aB
2-B
1�B
1�B
2aB
2�B
2�B
2�B
3�B
4B
4�B
4�B
5B
5B
6�B
6FB
6�B
7B
6�B
7�B
8RB
8RB
8RB
8RB
8B
8RB
7�B
8RB
8B
8�B
8RB
8�B
8RB
8�B
8B
9XB
9�B
9XB
9XB
:�B
<6B
=qB
<�B
=�B
=<B
=�B
=�B
=qB
>�B
>�B
>wB
?B
?HB
@B
?}B
@�B
@OB
?�B
@OB
@B
@�B
@B
@�B
@�B
@�B
@�B
@�B
@OB
A�B
A B
@�B
A�B
A�B
B[B
B�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D3B
C�B
D�B
GB
FtB
GB
F�B
GzB
H�B
IB
I�B
I�B
I�B
I�B
J#B
K)B
J�B
J�B
J�B
J�B
J�B
K)B
J�B
L0B
N�B
O�B
OvB
OBB
OvB
PB
PB
P�B
P�B
P�B
P�B
P�B
P}B
PHB
P�B
P�B
Q�B
R B
RTB
R�B
S[B
R�B
R�B
R B
S�B
T�B
U�B
V�B
V9B
V�B
VB
VmB
VmB
X�B
XyB
XEB
XEB
XyB
XB
X�B
YB
YB
XyB
Y�B
Y�B
ZQB
ZQB
ZB
Z�B
Z�B
Z�B
[WB
Z�B
[�B
[#B
\]B
^B
^5B
^B
^5B
^�B
^jB
^�B
^�B
^�B
_B
^�B
_B
_pB
_pB
_�B
_�B
`B
_�B
`vB
`B
`�B
aB
bB
a�B
b�B
bNB
c B
cTB
cTB
c B
c�B
dZB
dZB
dZB
d�B
d�B
d�B
d�B
d�B
d�B
e,B
e�B
f2B
f2B
g8B
gmB
g8B
g8B
g8B
g�B
g�B
h�B
h
B
i�B
iyB
jB
iyB
i�B
jB
j�B
j�B
jB
jB
jB
kB
j�B
j�B
j�B
j�B
kQB
j�B
kQB
k�B
k�B
k�B
l"B
l�B
l�B
ncB
n�B
m�B
n�B
n�B
n/B
ncB
n�B
o�B
o B
o�B
o�B
oiB
p;B
o�B
oiB
p�B
p�B
pB
p�B
p�B
qAB
p�B
p�B
q�B
qvB
q�B
q�B
rB
sB
r�B
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
t�B
tTB
t�B
t�B
t�B
tTB
u�B
v+B
u�B
v�B
v�B
v�B
v`B
w2B
v�B
wfB
v�B
w2B
v�B
v�B
w�B
w�B
x�B
yrB
x�B
yrB
y�B
y�B
zxB
{JB
{�B
{�B
{�B
{B
{JB
{B
{�B
|B
{B
|B
{�B
{�B
|B
|�B
|�B
|�B
|�B
|�B
}VB
}�B
~�B
~(B
~(B
~]B
}�B
~�B
~�B
~�B
cB
�B
.B
~�B
�B
�iB
�B
�B
�iB
� B
� B
�B
��B
��B
�B
�iB
��B
�B
�;B
��B
��B
�uB
�GB
��B
�B
�B
�B
�{B
�B
�{B
�B
�GB
��B
�GB
��B
�SB
��B
�SB
��B
��B
��B
�%B
��B
��B
��B
��B
�YB
��B
��B
�YB
�%B
��B
��B
��B
�_B
��B
�+B
��B
��B
��B
�fB
��B
�fB
�B
��B
�fB
�7B
��B
�B
��B
�	B
��B
�lB
��B
��B
��B
�	B
�=B
�B
�B
��B
�xB
��B
��B
�B
�xB
��B
�JB
�~B
�~B
��B
��B
��B
�~B
��B
��B
�PB
��B
�"B
�"B
�"B	��B	��B	�_B	��B	��B	�+B	�fB	�1B	�+B	�%B	�YB	��B	��B	��B	�YB	��B	�%B	�%B	�YB	�%B	�%B	��B	�+B	�_B	�_B	��B	��B	�1B	�1B	�1B	�fB	�1B	��B	��B	�YB	��B	�YB	�YB	��B	��B	��B	��B	�_B	��B	��B	��B	��B	�1B	�1B	��B	��B	��B	�+B	��B	�%B	�%B	��B	�_B	�YB	�fB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�YB	�_B	�_B	��B	�+B	��B	��B	��B	��B	��B	��B	�SB	��B	��B	��B	��B	�+B	�+B	��B	�fB	��B	�fB	��B	��B	��B	�YB	�YB	��B	�B	��B	�YB	�%B	��B	��B	�_B	��B	�_B	��B	��B	��B	��B	��B	��B	�SB	��B	��B	��B	�+B	�+B	�+B	��B	��B	�%B	��B	�SB	��B	�%B	��B	��B	��B	��B	��B	�YB	��B	��B	��B	��B	�+B	��B	��B	�_B	�YB	�%B	�%B	�%B	��B	�+B	��B	��B	��B	�_B	�YB	��B	��B	�%B	�YB	��B	�_B	��B	��B	�_B	�_B	��B	�YB	�%B	�YB	��B	��B	��B	��B	��B	�+B	��B	�YB	��B	��B	�B	��B	��B	�YB	��B	�%B	��B	��B	�_B	�_B	�+B	��B	�YB	��B	�+B	��B	��B	��B	��B	��B	��B	�YB	�+B	�_B	��B	�+B	��B	�%B	��B	��B	�+B	��B	��B	�_B	�+B	�+B	��B	��B	��B	��B	��B	��B	�+B	��B	��B	��B	��B	�.B	��B	�(B	��B	�.B	�SB	�SB	��B	��B	��B	�YB	�B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�1B	�7B	�1B	��B	�kB	��B	��B	�B	�*B	�qB	�<B	��B	�6B	��B	��B
�B
1[B
DgB
H�B
]�B
e,B
h�B
zB
� B
ĜB
�tBC�B^B��B�B�[B�OB��B��B�qB�=B�!B��B��B��B��B�qBÖB��B�B��B��BбB�aB�B�B�`B��BںB�)B��B�)B��B�BٴB�B��B�KB�5BרB��B�TB�yB�WB��BuBhBuB�B#nB(�B&�B#�B+�B:�B>wBC�BN<BOvBZBh
BjB{B��B�B�Bn/BffB{JB~�BcTB`�B`BlWB{Bu�Br�BrGB��Bz�B}�Bx�Bt�Bw�B��B�BncBlWBu�B�BkQBjBj�Bm]Be,B{JB� B�_B��B��B��B��B�fB�MB��B�_B�1B�B�vB{�BtBrBoiBqvBp�BkQBk�Bn/BncBy�B��B� Br|BcBh>Bp;BsBg8Be�B�xBe`BhsBc�B\]B\�B_�B`�B_pBe�BhsBh>BhsBc�BrGB��B�4B�B��B��B�kB�:B� B��B�FB�{B�Bu�B�FB�B~(Bu�BcTBe`BZ�BB�B;0B3�B2aB2�B2-B/�B1�B9�B3�B/�B+�B)�B(�B.}B+6B(�B&LB%B#:B#nB#nB&�B!�B�B �B�B~BB�B�B	B�B=BkB�B�B�B�B4B B�B\B�BB{B��B�B��B�xB��B��B��B��B�B�5B�;B�5B�B��B�TB�B�5B�#B�yB�sB�B�B��B��B�aBҽB�&B�}B͟B˒B�0B��B�zBȀBɺB��B�BB��B�*B��B��B�6B��B�XB�LB��B��B�LB�LB��B��B�B�nB��B��B��B��B�CB�IB��B��B��B��B�FB�PB��B��B��B��B�B��B��B��B}�B}�B��BzBxBv�Bt�Bu%Bt�BsBsMBr�BqBq�BqvBo Bo�Bo Bl�BjBl"BoiBkBe�BqAB`BBZQBWsBS�BT,BTaBPHBN<BK�BHKBD�BCaB?HB@�BAUB>�B<B8�B5�B5�B7�B1'B0UB/B,qB+�B/�B*�B$tB+�B(�B$B%�B#�B�BCB~BB�B�BCB�B�B$B_B{B@B�BoBB�BBB�BhB�B_B
��B
�DB
��B
�8B
�+B
�fB
�rB
�B
�MB
�B
��B
��B
��B
��B
�B
��B
�gB
�9B
�[B
�QB
�&B
�[B
�B
�dB
��B
�B
�tB
�B
�OB
��B
��B
�6B
�HB
��B
�B
��B
��B
�IB
��B
��B
��B
��B
�B
��B
�@B
�nB
�zB
�hB
��B
�_B
��B
�SB
�@B
��B
�B
��B
��B
�B
��B
�GB
�B
��B
wfB
r�B
qAB
poB
o B
o5B
m]B
n�B
n/B
l�B
l�B
kB
kQB
k�B
iB
i�B
iyB
k�B
g�B
e,B
e`B
d�B
c�B
gB
aHB
bB
aHB
_pB
aB
^jB
]�B
\�B
Z�B
S�B
R B
S[B
ZQB
OvB
GzB
?HB
>�B
=qB
5�B
2�B
-CB
'�B
)*B
#�B
�B
�B
�B
	lB
+B
�B
�B	��B
�B	��B	�(B
 �B	�B	�xB	��B	��B	�)B	�B	�B	�cB	�B	�cB	�BB	�/B	ںB	��B	՛B	��B	�,B	��B	�yB	�NB	�^B	�}B	�HB	�B	��B	�jB	��B	��B	�LB	�B	��B	��B	��B	�hB	�qB	�SB	�B	��B	��B	�	B	��B	�SB	�B	��B	��B	�{B	��B	�iB	y�B	y�B	y	B	{�B	�%B	y>B	g�B	k�B	kB	g�B	c�B	d�B	Z�B	R�B	YB	P}B	OvB	HKG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                          B	~�B	kB	B	B	7B	kB	7B	7B	kB	kB	kB	7B	7B	kB	7B	kB	B	kB	kB	7B	~eB	~eB	~�B	~eB	B	~�B	B	~�B	B	7B	B	B	~�B	~�B	~�B	~eB	��B	�eB	�CB	�B	��B
b�B7 B��B�KBȽB�vB�cB��B�B�B<
Bq�Bg�BhBr�Bo	Bk%B�4B�tBg�Bn�BiMBfoBYBr�B��B�~Bt�B>�B+B�BUB�B�+B�)B��B��B�tB�BrBg�BS/B7TB$�BB�B
�B
��B
�B
��B
�'B
�VB
guB
a�B
ZZB
J,B
$�B	�_B	�B	ыB	��B	�B	y�B	f�B	D�B	9�B	.�B	FB	:B�5B��B�|B��B��B��B�BB��B��B��B��B�B�pB�B�BB�B˛B�yBҒB�rB	B	2�B	`JB	h�B	m1B	m�B	zB	�_B	��B	ǷB	ڎB	�oB	�B	��B	��B	�B	�B	�rB	�~B	�B	��B	�4B	�B	�:B	�B	�4B	�GB
�B
�B
�B
E�B
P�B
QWB
Q�B
PQB
NyB
NEB
N�B
O�B
L�B
H�B
J`B
LmB
M�B
M�B
PQB
Y�B
Z�B
XB
W|B
UB
WB
PB
OB
N�B
M>B
J�B
H�B
F�B
FB
D�B
CjB
B�B
A^B
>�B
=B
<
B
6�B
7 B
2B
2�B
,�B
*9B
%OB
$B
#BB
#BB
"<B
!�B
 dB
!kB
�B
XB
LB
tB
3B
3B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
B
�B
hB
3B
�B
�B
�B
�B
LB
B
�B
zB
B
�B
B
�B
�B
�B
YB

�B
	tB
�B
�B
:B
	B
bB
�B
�B
�B
'B
\B
�B
bB
�B
�B
.B
.B
bB
.B
�B
�B
\B
�B
VB
4B
�B
.B
�B
bB
.B
�B
.B
\B
�B
�B
�B

B
YB
B
�B
'B
�B
�B
hB
[B
�B
�B
kB
�B
�B
_B
�B
�B
�B
�B
B
B
_B
RB
�B
�B
RB
RB
�B
�B
qB
kB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

FB

B
�B
�B
	@B
�B
�B
.B
�B
~B
�B
B
CB
~B
 �B
 =B	��B	�1B	��B	��B	��B	�B	��B	�B	��B	��B	�B	��B	�@B	��B	��B	��B	�%B	��B	��B	�%B	�YB	��B	��B	��B	�B	��B	�B	�MB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	�+B	��B	�+B	��B	��B	��B	�B	��B	��B	��B	�_B	��B	�+B	��B	�1B	�+B	��B	��B	��B	��B	�YB	��B	��B	��B	��B	��B	��B	�B	��B	�GB	��B	�@B	�uB	�B	�B	�B	��B	�B	�SB	�SB	�SB	�SB	��B	��B	�{B	��B	��B	��B	��B	�uB	�uB	��B	��B	��B	��B	�B	��B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�_B	��B	��B	��B
 	B
 =B
xB
xB
�B
B
�B
�B
�B
�B
!B
\B
�B
.B
�B
�B
B
	�B
	tB

FB

�B
B
LB
LB
LB
LB
B
$B
�B
�B
�B
�B
kB
kB
kB
B
kB
kB
�B
B
B
�B
}B
�B
�B
OB
OB
�B
�B
UB
!B
UB
�B
UB
UB
'B
UB
�B
�B
[B
[B
-B
3B
B
@B
zB
�B
�B
�B
�B
�B
�B
LB
�B
B
�B
RB
�B
�B
�B
�B
�B
 0B
 dB
�B
 dB
!B
 �B
"�B
"qB
"<B
"�B
"�B
"�B
#�B
#wB
#BB
#�B
#�B
%�B
%�B
& B
&�B
&�B
&�B
(�B
(�B
(�B
(�B
)�B
*mB
*9B
)�B
*B
*mB
*�B
+B
*�B
+�B
,B
,�B
,�B
-B
-B
.�B
.RB
.�B
/#B
.�B
/�B
0^B
0^B
0^B
0^B
0)B
0^B
/�B
0^B
0)B
0�B
0^B
0�B
0^B
0�B
0)B
1dB
1�B
1dB
1dB
2�B
4BB
5}B
4�B
5�B
5HB
5�B
5�B
5}B
6�B
6�B
6�B
7 B
7TB
8&B
7�B
8�B
8[B
7�B
8[B
8&B
8�B
8&B
8�B
8�B
8�B
8�B
8�B
8[B
9�B
9,B
8�B
9�B
9�B
:gB
:�B
<
B
;�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
<?B
<
B
<�B
?B
>�B
?B
>�B
?�B
@�B
A)B
A�B
A�B
A�B
A�B
B/B
C5B
CB
B�B
CB
B�B
CB
C5B
CB
D<B
F�B
G�B
G�B
GNB
G�B
H B
H B
H�B
H�B
H�B
H�B
H�B
H�B
HTB
H�B
H�B
I�B
J,B
J`B
J�B
KgB
J�B
J�B
J,B
LB
M
B
M�B
N�B
NEB
N�B
NB
NyB
NyB
P�B
P�B
PQB
PQB
P�B
PB
P�B
Q#B
Q#B
P�B
Q�B
Q�B
R]B
R]B
R)B
R�B
R�B
R�B
ScB
R�B
S�B
S/B
TiB
VB
VAB
VB
VAB
V�B
VvB
V�B
V�B
V�B
WB
V�B
WB
W|B
W|B
W�B
W�B
XB
W�B
X�B
XB
X�B
YB
Z%B
Y�B
Z�B
ZZB
[,B
[`B
[`B
[,B
[�B
\fB
\fB
\fB
\�B
\�B
\�B
]B
]B
]B
]8B
]�B
^>B
^>B
_DB
_yB
_DB
_DB
_DB
_�B
_�B
`�B
`B
a�B
a�B
b"B
a�B
a�B
b�B
b�B
b�B
b"B
b�B
b"B
c(B
b�B
b�B
b�B
b�B
c]B
b�B
c]B
c�B
c�B
c�B
d.B
d�B
e B
foB
f�B
fB
f�B
f�B
f;B
foB
f�B
g�B
gB
g�B
g�B
guB
hGB
g�B
guB
h�B
h�B
hB
h�B
h�B
iMB
h�B
h�B
i�B
i�B
i�B
i�B
jB
k%B
j�B
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
l�B
l`B
l�B
l�B
l�B
l`B
nB
n7B
nB
n�B
n�B
o	B
nlB
o>B
n�B
orB
o	B
o>B
o	B
o	B
o�B
o�B
p�B
q~B
p�B
q~B
q�B
q�B
r�B
sVB
s�B
s�B
s�B
s�B
sVB
s"B
s�B
t(B
s�B
t(B
s�B
s�B
t(B
t�B
t�B
t�B
t�B
t�B
ubB
u�B
v�B
v4B
v4B
viB
v B
v�B
v�B
v�B
woB
w�B
w:B
wB
w�B
xuB
w�B
w�B
xuB
xB
xB
yB
x�B
x�B
yB
xuB
y�B
yB
yGB
z�B
z�B
z�B
{SB
z�B
{B
{B
{B
{�B
{B
{�B
{B
{SB
{�B
{SB
|�B
}_B
|�B
}_B
|�B
}�B
}�B
~1B
}�B
}�B
}�B
}�B
~eB
}�B
~�B
~eB
~1B
~�B
B
~�B
kB
B
7B
�B
B
�B
�rB
�B
�rB
�B
��B
�rB
�CB
��B
�B
��B
�B
��B
�xB
��B
��B
��B
�B
�IB
�B
�B
��B
��B
��B
��B
�B
��B
��B
�VB
��B
��B
��B
��B
��B
��B
��B
��B
�\B
��B
�.B
�.B
�.B	z�B	�B	kB	�B	�	B	7B	�rB	�=B	7B	~1B	~eB	~�B	~�B	~�B	~eB	}�B	~1B	~1B	~eB	~1B	~1B	B	7B	kB	kB	�B	�	B	�=B	�=B	�=B	�rB	�=B	�B	�B	~eB	~�B	~eB	~eB	~�B	~�B	~�B	~�B	kB	�B	�	B	�	B	�B	�=B	�=B	�B	�	B	�B	7B	�B	~1B	~1B	�B	kB	~eB	�rB	�B	�B	�B	�B	�B	~�B	~�B	~�B	}�B	}�B	~eB	kB	kB	�B	7B	��B	�	B	|�B	�	B	}�B	B	}_B	~�B	�B	~�B	�	B	7B	7B	�	B	�rB	��B	�rB	~�B	B	~�B	~eB	~eB	��B	}+B	}�B	~eB	~1B	B	~�B	kB	~�B	kB	B	~�B	~�B	~�B	~�B	}�B	}_B	}�B	}�B	~�B	7B	7B	7B	B	~�B	~1B	}�B	}_B	|�B	~1B	B	~�B	�B	�B	B	~eB	~�B	~�B	~�B	~�B	7B	B	B	kB	~eB	~1B	~1B	~1B	~�B	7B	�	B	�	B	�B	kB	~eB	}�B	}�B	~1B	~eB	B	kB	B	�B	kB	kB	�B	~eB	~1B	~eB	~�B	~�B	�B	�B	�B	7B	B	~eB	~�B	�B	�B	��B	}�B	~eB	}�B	~1B	B	�B	kB	kB	7B	}�B	~eB	B	7B	��B	~�B	~�B	}�B	}�B	}�B	~eB	7B	kB	�B	7B	B	~1B	}�B	}�B	7B	~�B	�B	kB	7B	7B	~�B	}�B	}�B	}�B	}�B	~�B	7B	�B	B	�B	��B	�:B	��B	�4B	��B	�:B	�_B	�_B	��B	��B	��B	�eB	�*B	�B	��B	��B	��B	��B	�B	��B	��B	��B	�=B	�CB	�=B	��B	�wB	��B	��B	�)B	�6B	�}B	�HB	��B	�BB	��B	��B
�B
)gB
<sB
@�B
U�B
]8B
`�B
rB
�,B
��B
��B<
BVB��B�B�gB�[B��B��B�}B�IB�-B��B��B��B��B�}B��B��B�&B��B��BȽB�mB�B��B�lB��B��B�5B�B�5B��BыB��B�%B��B�WB�ABϴB��B�`BЅB�cB��B�B	tB�B�BzB �B�B�B#�B2�B6�B;�BFHBG�BR)B`Bb�Bs�B��B�B|%Bf;B^rBsVBv�B[`BX�BXBdcBs�BnBj�BjSB�Br�Bu�Bp�Bl�Bo�B{�BzBfoBdcBm�B|%Bc]Bb"Bb�BeiB]8BsVBxB�kB��B��B��B��B�rB|YB�BkB�=B�'BǂBs�Bl+BjBguBi�Bh�Bc]Bc�Bf;BfoBq�B}�BxBj�BwoB`JBhGBk%B_DB]�B��B]lB`B[�BTiBT�BW�BX�BW|B]�B`B`JB`B[�BjSB��B�@B�*B��B��B�wB�FB�B��B�RB{�ByBm�B�RB�Bv4BnB[`B]lBR�B:�B3<B+�B*mB*�B*9B'�B)�B1�B+�B'�B#�B"B!B&�B#BB �BXBBFBzBzB�B�B�B�B�B�B!B�B�BB�BIBwBB�BB�B	@B	B�BhB�BB��B��B��B��B�B�B��B��B��B�B�AB�GB�AB�B��B�`B�B�AB�/BЅB�B�B�B�
B�
B�mB��B�2BȉBūBÞB�<B��B��B��B��B�B� B��B��B�6B��B��B�BB��B�dB�XB��B��B�XB�XB��B��B�B�zB��B��B�B��B�OB�UB��B��B��B��B�RB�\B��B��B��B|�B|%Bz�Bx�Bz�Bu�Bu�Bx�BrBpBo	Bl�Bm1Bl�Bk%BkYBj�BiBi�Bi�BgBg�BgBd�Bb�Bd.BguBc(B]�BiMBXNBR]BOBLBL8BLmBHTBFHBDB@WB<�B;mB7TB8�B9aB6�B4B0�B-�B-�B/�B)3B(aB''B$}B#�B'�B"�B�B#�B �BB�B�B�BOB�BB�B�BOBB�B0BkB�BLB	�B
{B!B�B!BB�B	tB�B
�kB
��B
�PB
��B
�DB
�7B
�rB
�~B
�B
�YB
�%B
�B
��B
�
B
��B
ыB
��B
�sB
�EB
�gB
�]B
�2B
�gB
�B
�pB
��B
� B
��B
�B
�[B
��B
��B
�BB
�TB
��B
�B
�B
��B
�UB
��B
��B
��B
��B
�B
��B
�LB
�zB
��B
�tB
��B
�kB
��B
�_B
�LB
��B
�B
��B
��B
�B
��B
{SB
yB
~�B
orB
j�B
iMB
h{B
gB
gAB
eiB
f�B
f;B
d�B
d�B
c(B
c]B
c�B
aB
a�B
a�B
c�B
_�B
]8B
]lB
\�B
[�B
_B
YTB
Z%B
YTB
W|B
YB
VvB
U�B
UB
R�B
LB
J,B
KgB
R]B
G�B
?�B
7TB
6�B
5}B
-�B
*�B
%OB
�B
!6B
�B
�B
�B
�B
xB	�7B	��B	��B	��B	��B	��B	�4B	��B	�"B	�B	�B	�B	�5B	�B	�B	�oB	ݡB	�oB	�NB	�;B	��B	��B	ͧB	��B	�8B	��B	ЅB	�ZB	�jB	��B	�TB	�#B	�
B	�vB	��B	��B	�XB	� B	��B	��B	��B	�tB	�}B	�_B	�B	��B	��B	�B	�B	}_B	zB	~�B	{�B	{�B	z�B	xuB	q�B	q�B	qB	s�B	~1B	qJB	_�B	c�B	c(B	_�B	[�B	]B	R�B	J�B	Q�B	H�B	G�B	@WG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                          G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0077649                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0077649                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 23 11 2022 179 -0.0077649 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 23 11 2022 179 -0.0077649 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230426223229                            20230426223229AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023042622322920230426223229  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622322920230426223229QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F83E           783E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622322920230426223229QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               