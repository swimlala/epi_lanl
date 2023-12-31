CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-04-26T22:33:02Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20230426223302  20230426223302  5905275 5905275 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO7316                            7316                            2B  2B  AA  SOLO_II                         SOLO_II                         8644                            8644                            SBE602 11Jan18                  SBE602 11Jan18                  853 853 @�:3���@�:3���11  @�:ff�`@�:ff�`@*�H@�q�@*�H@�q��c��� ��c��� �11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AB  AB  ?�  ?�@=p�@�G�@��\@��R@�p�@��RA��A ��A,(�A@��A`��A�Q�A��A�  A�Q�A�  A�  A�  A�A��B  B�
B  B�
B(  B0(�B8(�B@Q�BH(�BO�
BW�B_�
Bg�
Bo�
Bw�
B�
B�  B�(�B�{B�  B�  B�{B�(�B�{B�  B�  B�(�B��
B��B�  B��B��B�  B��B�  B�  B�{B�{B�  B�  B�  B�  B�{B�{B�  B�  B��C   C
=C
=C
=C��C
  C
=C  C��C  C
=C
=C��C��C  C
=C   C"  C$  C%��C'��C*
=C,
=C.
=C0  C2  C4  C6  C7��C:  C<
=C>  C@
=CB  CC��CF  CG�CI�CK��CM��CP  CR  CT
=CV
=CW��CY��C\  C^
=C`{Cb{Cd
=Cf  Ch  Cj  Ck��Cn
=Cp  Cr  Ct  Cv
=Cx
=Cy��C{�C}��C��C�  C�C�  C���C���C�  C���C���C�  C�C���C�  C�
=C�
=C�C���C���C�  C�  C�  C���C�  C�C�C�  C�  C�  C�C�  C���C�  C�  C�C�
=C�C�  C���C�  C�C�  C�  C�  C�  C�  C�  C�  C�  C�C�
=C�C�  C�  C�  C�  C�  C�  C�C�C���C�  C�C�  C�  C�C�
=C�
=C�  C��C���C���C�  C���C���C�C�  C���C���C�  C�C���C��C���C�C�  C���C�C�
=C�
=C�  C�  C�C���C��C���C���C�  C���C���C���C�C���C���C���C�  C�
=C�C�  C���C���C�C�  C�  C�  C���C���C���C���C�  C�
=C�
=C�C�C���C���C�  C���C�  D �D }qD �qD� D  D� D�D� D  D}qD  D� D  D� D  D}qD�qD� D	  D	� D	�qD
}qD�D� D  D�D�D� D�qD}qD  D� D��D� D�D��D  Dz�D��D}qD�qD}qD  D}qD��Dz�D��Dz�D  D��D�D}qD�D�D�D}qD  D� D�qD}qD  D� D�D��D D �D!D!� D"  D"� D#�D#��D$�D$��D$��D%z�D&  D&� D&�qD'}qD'��D(}qD)  D)��D*  D*}qD+�D+�D,�D,� D-  D-� D-�qD.z�D.�qD/��D0�D0��D1�D1� D2�D2� D2��D3� D4�D4��D5  D5}qD5�qD6}qD6�qD7�D8D8}qD8�qD9z�D9�qD:� D;  D;��D<�D<��D=�D=��D>�D>� D?  D?}qD@  D@��DA  DA}qDB  DB� DC  DC}qDD  DD��DE  DEz�DE�qDF}qDF�qDG}qDH�DH��DH�qDI}qDJ  DJ� DJ�qDK� DL  DL}qDL�qDM� DM�qDN� DO  DO� DP�DP��DP�qDQ� DR�DR� DS  DS� DT�DT}qDT�qDUz�DU�qDV��DW�DW��DX�DX� DX�qDY}qDZ  DZ}qDZ�qD[� D\  D\��D]�D]}qD^  D^� D_  D_��D_�qD`z�D`�qDa� Db  Db}qDb�qDc}qDd  Dd�De�De� Df  Df� Dg�Dg�Dh  Dh}qDh�qDi}qDi�qDj}qDj�qDk� Dl�Dl��Dl�qDm� DnDn�Do�Do}qDo�qDp� Dq�Dq��Dq�qDr� Ds  Ds��Dt�Dt��Du�Du��Dv  Dv� Dw�Dw��Dx  Dx��Dy�Dy��Dz  Dz� D{  D{� D{�qD|}qD|��D}� D~D~��D�D��D�HD�AHD�� D���D��qD�@ D��HD���D���D�>�D�~�D���D�  D�@ D�� D�� D�HD�@ D�}qD�� D�HD�>�D�� D���D�  D�@ D�� D�� D�  D�@ D��HD�� D���D�>�D�� D�� D�  D�AHD�� D��qD���D�@ D��HD��HD��D�AHD�� D�� D���D�>�D�~�D��HD�HD�AHD��HD�� D��D�B�D�~�D���D�HD�@ D�� D�� D�  D�@ D�~�D��HD�HD�@ D�� D�� D���D�AHD��HD�� D�  D�@ D��HD��HD�HD�B�D�� D��HD��D�@ D��HD�D�HD�>�D�� D�� D���D�>�D�� D�� D�HD�@ D�~�D���D���D�@ D��HD��HD��D�@ D�� D��HD�  D�>�D�� D��HD�  D�>�D�� D�D�HD�@ D�~�D��qD���D�@ D�~�D�� D�HD�AHD�� D���D���D�AHD�� D��HD�HD�>�D�}qD�� D�HD�@ D�~�D�� D�HD�@ D�~�D��qD���D�AHD�� D�� D�  D�AHD��HD�� D�  D�AHD��HD��HD��D�@ D�}qD��qD���D�>�D�� D�� D�HD�AHD�� D��HD�  D�@ D�� D��HD��D�AHD�� D���D���D�>�D�~�D��HD�HD�>�D�~�D��HD�HD�@ D�� D���D���D�>�D�� D��HD�  D�AHD�� D�� D��D�AHD��HD��HD��D�B�D���D���D��)D�@ D���D�D�  D�>�D�� D��HD��D�AHD�� D���D��qD�@ D�� D�� D�  D�>�D�� D�� D�  D�AHD�~�D���D���D�>�D�~�D�� D�HD�>�D�� D��HD�  D�>�D�~�D��HD�  D�AHD��HD��HD�  D�>�D�� D�� D�  D�>�D�~�D�� D�  D�B�D��HD�� D���D�>�D��HD�� D�  D�AHD�� D�� D�  D�@ D�~�D½qD�  D�AHDÀ D�� D�  D�@ D�~�D�� D�  D�>�D�~�DŽqD��qD�=qD�}qDƾ�D���D�@ Dǀ DǾ�D���D�@ D�~�D�� D�HD�AHDɀ D�� D�  D�@ Dʀ D��HD��D�@ D�~�D�� D�HD�B�D́HD��HD�HD�@ D�~�DͽqD���D�AHD΀ D�� D�  D�@ Dπ D�� D�HD�AHDЂ�D��HD�  D�@ DсHD��HD��D�AHDҀ D�� D�  D�@ D�~�D�� D�HD�@ DԀ D��HD���D�@ DՀ Dվ�D�  D�AHDր D־�D�  D�@ D�~�D׽qD���D�>�D�}qDؾ�D�HD�AHDفHD�� D�  D�AHD�~�Dھ�D���D�@ DہHD�� D�HD�>�D�~�D��HD�HD�@ D݁HD�� D���D�AHDހ D�� D���D�AHD߀ D߽qD���D�@ D�~�D�� D�  D�>�D�HD�� D�  D�AHD�HD⾸D�  D�AHD� D�� D�  D�>�D� D��HD�  D�@ D�~�D�qD�  D�@ D� D澸D��qD�>�D�HD��HD���D�>�D�~�D�� D�  D�@ D� D�qD���D�@ D�}qD꾸D�  D�AHD� D�qD���D�@ D�~�D�� D�HD�>�D� D��HD�  D�@ DD��HD�HD�@ D� DﾸD��qD�@ D���D�� D��qD�@ D�D�D�HD�>�D� D�� D�HD�@ D�}qD�� D���D�=qD� D�� D�  D�AHD��HD�� D�HD�AHD�� D���D�  D�@ D�� D�� D�HD�AHD��HD�� D�  D�@ D�� D�D��D�4{?�?\)?aG�?�z�?�Q�?���?��@\)@#�
@0��@G�@\(�@n{@�G�@�=q@�@�  @�=q@�z�@��R@���@�33@޸R@���@��@�(�A33A	��A�RA�\A
=A��A"�\A'
=A*�HA0  A5A:�HA@  ADz�AH��AN�RATz�AX��A]p�Ac33Ah��Al��AqG�Aw
=A|(�A���A��HA�p�A��A��A���A�
=A���A��
A��RA���A�(�A��RA���A�(�A��RA���A��A��RA�G�A��HA�A���A��A�{A�Q�A�33A�{A�Q�Aʏ\A�p�A�Q�Aҏ\A��A׮Aڏ\A���A�
=A��A�z�A�
=A�G�A��
A�RA��A��HA�A�Q�A��HA���A��B�BffB�B��B{B�B��B	��B
=BQ�Bp�B�\B�
B�B�\B�B��B=qB�B��B�B
=Bz�BB�HB (�B!p�B"�HB$(�B%G�B&�\B(  B)p�B*�RB+�
B-�B.ffB0  B1�B2=qB3�B4��B6ffB7�B8��B9�B;�B<��B=�B?
=B@z�BB{BC\)BD��BEBG33BH��BI�BK33BLQ�BMBO\)BP��BQ�BS
=BT��BV{BW�BX��BY�B[�B\��B^=qB_\)B`��Bb=qBc�Bd��Be�Bg\)Bh��Bj=qBk�Bl��Bn{Bo�Bp��Br=qBs\)Bt��Bv{Bw�Bx��Bz{B{\)B|��B~=qB�B�ffB�
=B�B�z�B��B��B�=qB���B��B�=qB���B��B�=qB���B�\)B�  B���B�\)B�  B���B��B�B�z�B��B��B�=qB��HB���B�=qB��RB�\)B�  B��RB�G�B��
B�ffB�
=B���B�Q�B��HB�\)B��
B�z�B��B��B�(�B���B�33B�B�Q�B���B�33B��B�=qB��RB�33B���B�{B��\B��B���B�{B��\B��HB�p�B��B�ffB���B��B�{B���B��B��B�  B�z�B�
=B���B�  B�z�B���B��B��B��
B�{B�Q�B�z�B��\B���B��RB��HB���B��B�33B�33B�G�B�G�B�\)B��B���B�B��B�  B�{B�(�B�Q�B�z�B��RB���B�
=B��B�G�B�p�B��B��B�{B�ffB���B��HB��B�\)B��B�B�  B�Q�B���B���B�33B�p�B��B��B�=qB��\B��HB�G�B���B��B�=qB��\B��HB�33B��B��
B�=qB���B�
=B�p�B��
B�{B�z�B���B�33B���B�{B�z�B��HB�G�B���B�  B�ffB��HB�G�B�B�=qB£�B�
=B�p�B��
B�=qBģ�B��Bř�B�{B�z�B��HB�G�BǮB�{Bȏ\B���B�p�B��B�Q�B���B��B˅B�  B�ffB���B�G�B�B�=qBΣ�B��BυB��B�Q�BиRB��Bљ�B�{Bҏ\B���B�p�B��
B�=qBԣ�B�
=BՅB�  B�z�B��HB�\)B��
B�Q�Bأ�B��Bٙ�B�  B�ffB��HB�G�BۮB�=qBܸRB�33Bݙ�B�{B�z�B��HB�\)B߮B�(�B��\B���B�p�B��B�Q�B���B�G�B�B�(�B�\B���B�\)B��
B�(�B��B�
=B�B��B�ffB��HB�G�B�B�(�B��B�
=B�B��B�Q�B�RB��B�B�  B�ffB���B�G�B�B�=qB��B�
=B�B��B�ffB�RB��B�p�B��B�Q�B���B�G�B��B�(�B��\B���B�\)B��B�(�B��\B���B�\)B��
B�Q�B��RB�33B���B�{B�z�B��HB�G�B��B�{B�z�B��HB�\)B�C �C Q�C �C �RC �C{CG�Cz�C�C�HC
=C=qCp�C��C�
C
=C=qCffC��C��C  C33CffC��C��C��C�CQ�Cz�C�C�
C  C33C\)C�C�C�
C
=C33C\)C�C�C�
C
=C33CffC�C�C�HC	
=C	33C	\)C	�\C	�RC	�HC
{C
=qC
p�C
��C
��C
��C(�CQ�C�C�C�HC
=C=qCffC��CC�C{CG�Cp�C��C��C��C�CQ�Cp�C�C�
C  C=qCffC�\CC��C�CQ�C�C�C�HC{CG�C�C�C�HC{CG�Cz�C�C�HC{CQ�C�C�RC�C�CQ�C�C�C�C�CQ�C�C�RC�C{CG�Cz�C�C�HC
=CG�Cp�C��C�
C
=C=qCp�C��C�
C{CG�Cz�C�RC��C33Cp�C�C�C33Cp�C�C�C(�C\)C��C�
C{CG�C�CC  C=qCz�CC  C=qC�CC   C =qC �C ��C!{C!G�C!�\C!��C"{C"Q�C"�\C"�
C#
=C#Q�C#�\C#�
C${C$\)C$��C$�HC%(�C%ffC%�C%��C&G�C&�C&C'  C'G�C'�C'C(
=C(G�C(�C(��C)
=C)Q�C)�\C)�
C*{C*\)C*��C*�HC+(�C+p�C+�C+��C,33C,p�C,�C,�C-33C-z�C-C.  C.Q�C.�\C.�
C/�C/ffC/��C/�HC0�C0\)C0��C0��C1=qC1�C1��C2{C2\)C2��C2�HC3�C3p�C3�RC4  C4G�C4�\C4��C5{C5\)C5��C5�HC6(�C6z�C6C7{C7\)C7��C7�HC8(�C8ffC8�C9  C9G�C9��C9�C:�C:p�C:�C:��C;33C;�C;�
C<�C<ffC<�C<�C=33C=z�C=C>{C>ffC>�C>�C?33C?z�C?��C@�C@p�C@�RC@��CA=qCA�CA��CB�CBz�CBCC
=CCQ�CC�\CC�HCD(�CD�CD��CE�CE\)CE�CE��CFG�CF��CF�CG33CGz�CG�RCH
=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                 ?�  ?�@=p�@�G�@��\@��R@�p�@��RA��A ��A,(�A@��A`��A�Q�A��A�  A�Q�A�  A�  A�  A�A��B  B�
B  B�
B(  B0(�B8(�B@Q�BH(�BO�
BW�B_�
Bg�
Bo�
Bw�
B�
B�  B�(�B�{B�  B�  B�{B�(�B�{B�  B�  B�(�B��
B��B�  B��B��B�  B��B�  B�  B�{B�{B�  B�  B�  B�  B�{B�{B�  B�  B��C   C
=C
=C
=C��C
  C
=C  C��C  C
=C
=C��C��C  C
=C   C"  C$  C%��C'��C*
=C,
=C.
=C0  C2  C4  C6  C7��C:  C<
=C>  C@
=CB  CC��CF  CG�CI�CK��CM��CP  CR  CT
=CV
=CW��CY��C\  C^
=C`{Cb{Cd
=Cf  Ch  Cj  Ck��Cn
=Cp  Cr  Ct  Cv
=Cx
=Cy��C{�C}��C��C�  C�C�  C���C���C�  C���C���C�  C�C���C�  C�
=C�
=C�C���C���C�  C�  C�  C���C�  C�C�C�  C�  C�  C�C�  C���C�  C�  C�C�
=C�C�  C���C�  C�C�  C�  C�  C�  C�  C�  C�  C�  C�C�
=C�C�  C�  C�  C�  C�  C�  C�C�C���C�  C�C�  C�  C�C�
=C�
=C�  C��C���C���C�  C���C���C�C�  C���C���C�  C�C���C��C���C�C�  C���C�C�
=C�
=C�  C�  C�C���C��C���C���C�  C���C���C���C�C���C���C���C�  C�
=C�C�  C���C���C�C�  C�  C�  C���C���C���C���C�  C�
=C�
=C�C�C���C���C�  C���C�  D �D }qD �qD� D  D� D�D� D  D}qD  D� D  D� D  D}qD�qD� D	  D	� D	�qD
}qD�D� D  D�D�D� D�qD}qD  D� D��D� D�D��D  Dz�D��D}qD�qD}qD  D}qD��Dz�D��Dz�D  D��D�D}qD�D�D�D}qD  D� D�qD}qD  D� D�D��D D �D!D!� D"  D"� D#�D#��D$�D$��D$��D%z�D&  D&� D&�qD'}qD'��D(}qD)  D)��D*  D*}qD+�D+�D,�D,� D-  D-� D-�qD.z�D.�qD/��D0�D0��D1�D1� D2�D2� D2��D3� D4�D4��D5  D5}qD5�qD6}qD6�qD7�D8D8}qD8�qD9z�D9�qD:� D;  D;��D<�D<��D=�D=��D>�D>� D?  D?}qD@  D@��DA  DA}qDB  DB� DC  DC}qDD  DD��DE  DEz�DE�qDF}qDF�qDG}qDH�DH��DH�qDI}qDJ  DJ� DJ�qDK� DL  DL}qDL�qDM� DM�qDN� DO  DO� DP�DP��DP�qDQ� DR�DR� DS  DS� DT�DT}qDT�qDUz�DU�qDV��DW�DW��DX�DX� DX�qDY}qDZ  DZ}qDZ�qD[� D\  D\��D]�D]}qD^  D^� D_  D_��D_�qD`z�D`�qDa� Db  Db}qDb�qDc}qDd  Dd�De�De� Df  Df� Dg�Dg�Dh  Dh}qDh�qDi}qDi�qDj}qDj�qDk� Dl�Dl��Dl�qDm� DnDn�Do�Do}qDo�qDp� Dq�Dq��Dq�qDr� Ds  Ds��Dt�Dt��Du�Du��Dv  Dv� Dw�Dw��Dx  Dx��Dy�Dy��Dz  Dz� D{  D{� D{�qD|}qD|��D}� D~D~��D�D��D�HD�AHD�� D���D��qD�@ D��HD���D���D�>�D�~�D���D�  D�@ D�� D�� D�HD�@ D�}qD�� D�HD�>�D�� D���D�  D�@ D�� D�� D�  D�@ D��HD�� D���D�>�D�� D�� D�  D�AHD�� D��qD���D�@ D��HD��HD��D�AHD�� D�� D���D�>�D�~�D��HD�HD�AHD��HD�� D��D�B�D�~�D���D�HD�@ D�� D�� D�  D�@ D�~�D��HD�HD�@ D�� D�� D���D�AHD��HD�� D�  D�@ D��HD��HD�HD�B�D�� D��HD��D�@ D��HD�D�HD�>�D�� D�� D���D�>�D�� D�� D�HD�@ D�~�D���D���D�@ D��HD��HD��D�@ D�� D��HD�  D�>�D�� D��HD�  D�>�D�� D�D�HD�@ D�~�D��qD���D�@ D�~�D�� D�HD�AHD�� D���D���D�AHD�� D��HD�HD�>�D�}qD�� D�HD�@ D�~�D�� D�HD�@ D�~�D��qD���D�AHD�� D�� D�  D�AHD��HD�� D�  D�AHD��HD��HD��D�@ D�}qD��qD���D�>�D�� D�� D�HD�AHD�� D��HD�  D�@ D�� D��HD��D�AHD�� D���D���D�>�D�~�D��HD�HD�>�D�~�D��HD�HD�@ D�� D���D���D�>�D�� D��HD�  D�AHD�� D�� D��D�AHD��HD��HD��D�B�D���D���D��)D�@ D���D�D�  D�>�D�� D��HD��D�AHD�� D���D��qD�@ D�� D�� D�  D�>�D�� D�� D�  D�AHD�~�D���D���D�>�D�~�D�� D�HD�>�D�� D��HD�  D�>�D�~�D��HD�  D�AHD��HD��HD�  D�>�D�� D�� D�  D�>�D�~�D�� D�  D�B�D��HD�� D���D�>�D��HD�� D�  D�AHD�� D�� D�  D�@ D�~�D½qD�  D�AHDÀ D�� D�  D�@ D�~�D�� D�  D�>�D�~�DŽqD��qD�=qD�}qDƾ�D���D�@ Dǀ DǾ�D���D�@ D�~�D�� D�HD�AHDɀ D�� D�  D�@ Dʀ D��HD��D�@ D�~�D�� D�HD�B�D́HD��HD�HD�@ D�~�DͽqD���D�AHD΀ D�� D�  D�@ Dπ D�� D�HD�AHDЂ�D��HD�  D�@ DсHD��HD��D�AHDҀ D�� D�  D�@ D�~�D�� D�HD�@ DԀ D��HD���D�@ DՀ Dվ�D�  D�AHDր D־�D�  D�@ D�~�D׽qD���D�>�D�}qDؾ�D�HD�AHDفHD�� D�  D�AHD�~�Dھ�D���D�@ DہHD�� D�HD�>�D�~�D��HD�HD�@ D݁HD�� D���D�AHDހ D�� D���D�AHD߀ D߽qD���D�@ D�~�D�� D�  D�>�D�HD�� D�  D�AHD�HD⾸D�  D�AHD� D�� D�  D�>�D� D��HD�  D�@ D�~�D�qD�  D�@ D� D澸D��qD�>�D�HD��HD���D�>�D�~�D�� D�  D�@ D� D�qD���D�@ D�}qD꾸D�  D�AHD� D�qD���D�@ D�~�D�� D�HD�>�D� D��HD�  D�@ DD��HD�HD�@ D� DﾸD��qD�@ D���D�� D��qD�@ D�D�D�HD�>�D� D�� D�HD�@ D�}qD�� D���D�=qD� D�� D�  D�AHD��HD�� D�HD�AHD�� D���D�  D�@ D�� D�� D�HD�AHD��HD�� D�  D�@ D�� D�D��D�4{?�?\)?aG�?�z�?�Q�?���?��@\)@#�
@0��@G�@\(�@n{@�G�@�=q@�@�  @�=q@�z�@��R@���@�33@޸R@���@��@�(�A33A	��A�RA�\A
=A��A"�\A'
=A*�HA0  A5A:�HA@  ADz�AH��AN�RATz�AX��A]p�Ac33Ah��Al��AqG�Aw
=A|(�A���A��HA�p�A��A��A���A�
=A���A��
A��RA���A�(�A��RA���A�(�A��RA���A��A��RA�G�A��HA�A���A��A�{A�Q�A�33A�{A�Q�Aʏ\A�p�A�Q�Aҏ\A��A׮Aڏ\A���A�
=A��A�z�A�
=A�G�A��
A�RA��A��HA�A�Q�A��HA���A��B�BffB�B��B{B�B��B	��B
=BQ�Bp�B�\B�
B�B�\B�B��B=qB�B��B�B
=Bz�BB�HB (�B!p�B"�HB$(�B%G�B&�\B(  B)p�B*�RB+�
B-�B.ffB0  B1�B2=qB3�B4��B6ffB7�B8��B9�B;�B<��B=�B?
=B@z�BB{BC\)BD��BEBG33BH��BI�BK33BLQ�BMBO\)BP��BQ�BS
=BT��BV{BW�BX��BY�B[�B\��B^=qB_\)B`��Bb=qBc�Bd��Be�Bg\)Bh��Bj=qBk�Bl��Bn{Bo�Bp��Br=qBs\)Bt��Bv{Bw�Bx��Bz{B{\)B|��B~=qB�B�ffB�
=B�B�z�B��B��B�=qB���B��B�=qB���B��B�=qB���B�\)B�  B���B�\)B�  B���B��B�B�z�B��B��B�=qB��HB���B�=qB��RB�\)B�  B��RB�G�B��
B�ffB�
=B���B�Q�B��HB�\)B��
B�z�B��B��B�(�B���B�33B�B�Q�B���B�33B��B�=qB��RB�33B���B�{B��\B��B���B�{B��\B��HB�p�B��B�ffB���B��B�{B���B��B��B�  B�z�B�
=B���B�  B�z�B���B��B��B��
B�{B�Q�B�z�B��\B���B��RB��HB���B��B�33B�33B�G�B�G�B�\)B��B���B�B��B�  B�{B�(�B�Q�B�z�B��RB���B�
=B��B�G�B�p�B��B��B�{B�ffB���B��HB��B�\)B��B�B�  B�Q�B���B���B�33B�p�B��B��B�=qB��\B��HB�G�B���B��B�=qB��\B��HB�33B��B��
B�=qB���B�
=B�p�B��
B�{B�z�B���B�33B���B�{B�z�B��HB�G�B���B�  B�ffB��HB�G�B�B�=qB£�B�
=B�p�B��
B�=qBģ�B��Bř�B�{B�z�B��HB�G�BǮB�{Bȏ\B���B�p�B��B�Q�B���B��B˅B�  B�ffB���B�G�B�B�=qBΣ�B��BυB��B�Q�BиRB��Bљ�B�{Bҏ\B���B�p�B��
B�=qBԣ�B�
=BՅB�  B�z�B��HB�\)B��
B�Q�Bأ�B��Bٙ�B�  B�ffB��HB�G�BۮB�=qBܸRB�33Bݙ�B�{B�z�B��HB�\)B߮B�(�B��\B���B�p�B��B�Q�B���B�G�B�B�(�B�\B���B�\)B��
B�(�B��B�
=B�B��B�ffB��HB�G�B�B�(�B��B�
=B�B��B�Q�B�RB��B�B�  B�ffB���B�G�B�B�=qB��B�
=B�B��B�ffB�RB��B�p�B��B�Q�B���B�G�B��B�(�B��\B���B�\)B��B�(�B��\B���B�\)B��
B�Q�B��RB�33B���B�{B�z�B��HB�G�B��B�{B�z�B��HB�\)B�C �C Q�C �C �RC �C{CG�Cz�C�C�HC
=C=qCp�C��C�
C
=C=qCffC��C��C  C33CffC��C��C��C�CQ�Cz�C�C�
C  C33C\)C�C�C�
C
=C33C\)C�C�C�
C
=C33CffC�C�C�HC	
=C	33C	\)C	�\C	�RC	�HC
{C
=qC
p�C
��C
��C
��C(�CQ�C�C�C�HC
=C=qCffC��CC�C{CG�Cp�C��C��C��C�CQ�Cp�C�C�
C  C=qCffC�\CC��C�CQ�C�C�C�HC{CG�C�C�C�HC{CG�Cz�C�C�HC{CQ�C�C�RC�C�CQ�C�C�C�C�CQ�C�C�RC�C{CG�Cz�C�C�HC
=CG�Cp�C��C�
C
=C=qCp�C��C�
C{CG�Cz�C�RC��C33Cp�C�C�C33Cp�C�C�C(�C\)C��C�
C{CG�C�CC  C=qCz�CC  C=qC�CC   C =qC �C ��C!{C!G�C!�\C!��C"{C"Q�C"�\C"�
C#
=C#Q�C#�\C#�
C${C$\)C$��C$�HC%(�C%ffC%�C%��C&G�C&�C&C'  C'G�C'�C'C(
=C(G�C(�C(��C)
=C)Q�C)�\C)�
C*{C*\)C*��C*�HC+(�C+p�C+�C+��C,33C,p�C,�C,�C-33C-z�C-C.  C.Q�C.�\C.�
C/�C/ffC/��C/�HC0�C0\)C0��C0��C1=qC1�C1��C2{C2\)C2��C2�HC3�C3p�C3�RC4  C4G�C4�\C4��C5{C5\)C5��C5�HC6(�C6z�C6C7{C7\)C7��C7�HC8(�C8ffC8�C9  C9G�C9��C9�C:�C:p�C:�C:��C;33C;�C;�
C<�C<ffC<�C<�C=33C=z�C=C>{C>ffC>�C>�C?33C?z�C?��C@�C@p�C@�RC@��CA=qCA�CA��CB�CBz�CBCC
=CCQ�CC�\CC�HCD(�CD�CD��CE�CE\)CE�CE��CFG�CF��CF�CG33CGz�CG�RCH
=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���AξwAβ-AζFAμjA���A�ƨA�ĜAκ^AζFAκ^Aκ^Aδ9Aΰ!AΩ�AΡ�AΛ�AΛ�AΝ�AΙ�AΕ�AΓuAΓuA΍PAΏ\A΍PA΋DA΅A�~�A�~�A�|�A�~�A�|�A�p�A�n�A�I�A�$�A�%AͶFA�Ả7A�?}A�x�A��A�K�A��A��A�(�A�K�A��A�XA��A��PA�ĜA�~�A�"�A��!A�(�A�I�A���A�jA�5?A���A���A���A�?}A�5?A�n�A��9A�/A��;A�dZA���A��A��#A��
A�$�A�C�A��!A��DA�;dA���A��A�1A�dZA~�Az��Av�RAs��Ao�-AlZAh��Ad��A`ĜAZ��AV�HAT��ARĜAP9XANVAI��AD��AB5?A@I�A=�A;VA9+A5oA6��A9;dA:{A81'A;K�A=oA<��A:{A:=qA9�A4jA1�A/C�A+�A*��A)\)A'��A&��A&^5A&{A&5?A%��A%&�A#�A#\)A#�A"�jA"=qA!�
A!�hA!l�A ��A 9XA�hA`BA�A%A�RAz�A��AhsA`BA��A33AG�AS�AK�AȴAM�AJA�TA��A�A�A��At�A��A��Av�A��AoA�+AƨAx�A%A��AZA$�A��AXA�AbNA(�A  A�PA�A�A��AVA��A�AK�A�An�A��A�AhsAG�A��AE�A�AƨA�FA��A��A�PAdZA33A
�/A
~�A
VA
9XA	��A	\)A	�A�A��AA�AbA��A��AO�A&�A��AȴAn�A$�AƨA�7AK�AA�A��A�+AM�A1'AA�A1'A(�AA�PA+A��AQ�A-A�wA&�A �RA �uA �\A �@��F@��@���@���@�Q�@�1'@���@�K�@���@�v�@��@��#@���@�p�@�(�@�K�@���@�E�@�@��-@�/@���@��;@�K�@���@�ff@�J@�x�@�V@��@�w@�t�@��y@�!@�{@�@��@�u@�F@���@�^@�9@���@�l�@�M�@噚@���@�A�@�@��@�@��@߾w@�;d@�+@ޟ�@��@���@�x�@܃@�C�@�
=@ڧ�@�-@�@�hs@���@ش9@�r�@� �@���@�l�@�J@�1@�+@���@ҧ�@�~�@�J@�p�@���@�Z@ϝ�@�S�@�
=@�ȴ@Ο�@��@�`B@̬@�1'@��m@ˍP@ʧ�@�=q@�p�@��@�A�@�ƨ@�;d@Ɨ�@��@�O�@���@�1@å�@ÍP@��@�v�@�5?@���@���@���@��@�G�@��@�(�@�\)@��@�J@�hs@�V@�I�@�1'@��@��w@�K�@��@�@�O�@�Ĝ@���@�(�@�C�@��R@�5?@���@���@�p�@��@���@�bN@� �@��
@��@���@���@��9@��@�9X@� �@���@��;@��w@�l�@��y@��\@��T@���@��h@�G�@�%@���@��`@��@�j@��P@�
=@�ȴ@�ff@��@��h@�hs@�`B@���@�r�@�I�@��@��m@��w@�S�@���@���@�n�@��@���@���@�G�@�Ĝ@�Q�@�(�@�  @�ƨ@�dZ@�o@���@��+@�$�@���@���@��u@�Z@�I�@�b@��
@��F@�t�@�+@��@���@���@�$�@��@���@��-@�x�@��@�%@���@��@�j@�9X@�ƨ@���@�dZ@��@��@�ff@��@��@���@��-@�x�@�G�@��@���@�9X@���@���@�l�@�;d@��H@��!@�v�@�^5@�-@��@���@�O�@��j@� �@���@�t�@�\)@�K�@�;d@�"�@��y@��\@�M�@�J@��T@���@�@�G�@��`@�r�@�(�@��m@�\)@�C�@�+@���@��!@��+@�-@���@�p�@�O�@�?}@��@���@�Ĝ@�z�@�bN@�Q�@�A�@��@��@�l�@�C�@�
=@��!@��+@��@��@�@��h@�x�@�p�@�X@��@�r�@�b@�t�@�"�@���@���@�v�@�5?@�@��T@�@��7@��@�O�@��@���@�Ĝ@��j@��@���@��D@�r�@�Z@�1@��w@�t�@���@���@�v�@�-@���@�p�@�G�@���@�bN@�9X@��@��@l�@\)@K�@~�@}�-@|�@{��@z�@z��@z=q@y��@y��@y�7@yhs@y�@x�`@xĜ@x��@xQ�@w�@vE�@u�@u/@t�@tz�@t1@s��@r�H@rJ@q�@q�@q��@qx�@q�@p�9@p�@o�;@oK�@n�R@nE�@m�h@m/@l��@l��@l�@k��@kdZ@j�@jn�@j=q@i�#@ix�@i%@h�9@hr�@g�w@g+@f��@f$�@e/@d9X@c�m@c��@c�@b��@b-@a��@a�^@aX@`r�@` �@_�@_;d@_+@_�@_�@_�@_
=@^��@^��@^�y@^ff@]`B@]V@\�/@\�@\j@\1@[C�@Z��@Z-@Y�^@X�`@XbN@Xb@W�@W�@V�+@V5?@U�@U`B@UV@T�j@Tz�@T(�@S�F@S��@R�!@Q��@QX@Q�@PbN@O�@O��@O\)@O;d@O+@N��@N��@N�+@NE�@N@M�@Mp�@LI�@K��@KdZ@Ko@J��@JM�@J�@I�#@IX@H��@H��@H�@Hr�@HbN@Hb@G�@G\)@G;d@G
=@F�@F�+@F{@E��@EO�@EV@D�/@D��@DI�@D(�@D�@D1@C��@C�m@C��@C33@B�@B��@Bn�@A��@A&�@@��@@Ĝ@@�@@bN@@1'@?�@?�P@?K�@>��@>�@>�R@>��@>��@>V@>{@=@=p�@=?}@<�j@<�D@<Z@<9X@<�@;�m@;ƨ@;�F@;dZ@:�H@:=q@9�@9x�@9G�@9%@8�u@8A�@81'@8b@7�@7��@7�@7��@7|�@7;d@6�R@6V@6@5�T@5��@5V@4��@4�j@49X@3��@3"�@2�\@2�@1G�@1&�@0r�@/�;@/�@/l�@/K�@.��@.ff@.E�@-@-?}@,��@,j@,Z@,I�@,(�@+�F@+o@*��@*^5@)��@)�7@)�7@)�7@)x�@)x�@)hs@)X@)�@(bN@'�@'�@'��@'|�@';d@'
=@&ȴ@&�+@&�+@&V@&E�@&E�@%�@%��@%O�@%V@$��@$Z@#��@"�@"�H@"�H@"��@"��@"^5@!�@!�^@!hs@!&�@ �`@ r�@ Q�@ Q�@ A�@ b@�;@�;@�w@�P@\)@;d@+@
=@��@ȴ@ff@5?@@��@`B@�@�@��@z�@I�@�@�@1@�m@�
@ƨ@ƨ@ƨ@ƨ@�F@�F@�F@��@��@�@dZ@C�@@~�@��@��@hs@7L@�`@�9@r�@A�@b@�w@;d@ȴ@��@��@��@V@@�-@�-@�h@O�@V@��@V@V@��@��@�@�@�D@j@I�@9X@1@�m@�
@��@C�@33@@��@��@�\@^5@=q@�@�@��@��@�7@x�@G�@7L@�@%@��@��@��@�9@r�@1'@ �@�@l�@;d@
=@�@�R@��@v�@V@E�@$�@@��@@�-@�h@�@p�@O�@/@�@�@��@j@(�@1@��@��@�m@��@�A���A���A�ĜA���A���A�ĜA�ȴAδ9Aΰ!Aβ-Aβ-AζFAθRAβ-AζFAζFAζFA�ĜA�ĜA���A���A�ĜA���A���A���A�ƨA���A�ĜAζFAζFAζFAκ^AθRAδ9Aδ9AμjA�AμjAκ^AζFAζFAζFAδ9Aβ-AθRAθRAζFAΰ!Aβ-Aβ-AήAήAβ-Aβ-AήAάAήAήAΣ�AΣ�AΣ�AΣ�AΡ�AΟ�AΡ�AΡ�AΟ�AΝ�AΝ�AΛ�AΙ�AΙ�AΝ�AΝ�AΙ�AΙ�AΛ�AΟ�AΝ�AΛ�AΝ�AΡ�AΛ�AΛ�AΝ�AΝ�AΙ�AΙ�AΙ�AΛ�AΙ�AΕ�AΗ�AΗ�AΗ�AΓuAΓuAΗ�AΗ�AΓuAΑhAΕ�AΕ�AΓuAΏ\AΑhAΕ�AΗ�AΑhAΑhAΓuAΑhA΍PA΋DAΏ\AΑhA΍PAΉ7A΋DAΑhAΑhA΍PA΍PAΏ\AΑhAΏ\A΋DAΉ7A΍PAΏ\A΍PA΋DA΋DA΋DA΍PA·+A΅A·+AΉ7A΅A΃A΁A΅A΅A΁A�|�A�|�A�~�A΁A�|�A�|�A΁A�~�A΁A�|�A�z�A�|�A΁A�~�A�|�A�z�A�|�A�~�A΁A�~�A�|�A΁A΃A΁A�|�A�|�A�z�A�x�A�p�A�n�A�n�A�t�A�p�A�jA�jA�r�A�n�A�r�A�p�A�hsA�\)A�\)A�VA�;dA�1'A�(�A�+A�&�A�&�A�$�A� �A��A�oA�VA�A���A�A�  A��A���A͸RA͡�A͙�A͍PA�x�A�^5A�A�A�
=A̺^A̬A̡�A̟�A̙�A̍PÁA�v�A�t�A�t�A�jA�S�A�=qA��A�%A��A�ĜAˣ�A�|�A�ZA� �A�{A���A���A���A���A��A��`A���Aʝ�AʑhA�~�A�ZA�-A�oA��
Aɥ�AɁA�bNA�G�A�-A��AȸRAȇ+A��A�K�A��yA�Q�A�oA�E�A��A��jA��RA�  A��^A��A���A��TA�"�A���A�;dA��`A��9A�G�A�?}A�S�A�JA�r�A��A�M�A���A���A��A���A���A���A��DA�n�A�bNA�Q�A�A�A�-A�-A�+A�
=A��#A��A�~�A�C�A��mA�|�A�$�A�1A��!A��TA���A��uA��A�dZA�(�A��HA��^A���A�v�A�9XA�K�A�1A��A���A���A���A���A���A��#A��`A��A�%A�{A�{A�oA���A���A��A��`A���A�A���A���A���A���A�ĜA��9A���A��+A�~�A��A�z�A�^5A�G�A�;dA�5?A�&�A��A�
=A��yA��!A�v�A�(�A�bA�A��yA�ƨA��hA�dZA�I�A�A�A�9XA�9XA�7LA�5?A�1'A�(�A� �A��A��A��A��A�bA�
=A��A���A��jA���A���A��PA�l�A�^5A�VA�E�A�;dA�;dA�5?A��A�1A�%A�{A�VA���A��RA���A�z�A�Q�A�
=A��wA�|�A�\)A�I�A���A��
A��A��DA�M�A�;dA�G�A�Q�A�\)A�bNA�dZA�hsA�hsA�p�A�z�A��A�v�A�jA�VA�E�A�1'A�&�A� �A��A�A���A��mA��;A��#A��A���A���A���A��wA���A��\A���A���A��^A���A���A���A��DA�~�A�XA�/A�oA�VA�1A�A��;A�ĜA��-A���A���A���A��hA�x�A�?}A��A��TA�ȴA��FA���A��+A�l�A�Q�A�33A��A�  A��TA�ȴA���A��-A���A�x�A�p�A�hsA�\)A�G�A�5?A�{A��A��;A���A�ȴA��FA���A��DA��A�\)A�XA�M�A�A�A�9XA�33A�$�A�"�A��A�{A�bA�
=A�  A���A��A��A��A���A���A�ȴA��A��A�~�A��A�~�A�z�A�r�A�dZA�K�A�G�A�E�A�C�A�7LA��A�%A��A��9A���A���A�z�A�;dA�"�A���A�v�A�
=A��;A���A�ZA�1A�ĜA�&�A��jA��DA�K�A�bA��#A���A��\A�n�A�`BA�"�A���A���A�|�A�O�A��A���A�+A��A��FA�~�A�1'A��
A���A�/A��9A�p�A�33A�  A���A���A��hA��7A��+A�^5A�%A��uA� �A���A�5?A��;A���A�7LA��wA�7LA�ƨA�33A��yA���A�|�A�Q�A���A��9A��PA�  A�r�A��mA���A�I�A�oA��TA���A���A���A��hA�x�A�n�A�l�A�^5A�ZA�XA�S�A�O�A�K�A�M�A�G�A�?}A�/A��A�
=A��A���A�jA�E�A�%A�A�|�A�$�A���A��
A��A�A�A�1A��
A��7A�5?A�ĜA�v�A�K�A�5?A�+A��A�{A�JA���A��`A���A��-A���A��DA�z�A�ZA�K�A�?}A�1'A��A���A�JA7LA~�RA~5?A}�A|��A|E�A|bA{�A{�-Az��Ay�#Ay�AxM�Aw�FAw�PAw`BAv��Av��Av=qAu�FAup�At�`As�Asx�AshsAs�FAs?}Arn�ArbAq�hAp��Ao��Ao&�An��An�`Am��Al��Al�Al�HAlȴAl�uAlVAk��Ak�Ajn�Aj1Aip�Ah�Ah�Ah{AgAg|�AgK�Af�yAfbAd��AcƨAb�AbZAa��Aa��AaC�A`��A`��A`z�A_�TA^�A]O�A\�A[VAY�AY`BAX��AX �AW��AW;dAWAV��AV��AVn�AV-AV(�AVJAUATȴAT5?AT1'AT(�AS��AS�AS33AR��ARz�AR1'AQ��AQ��AQK�AP�AP-AO��AO�7AOdZAOS�AO
=AN�/AN�!AN�AN9XAM�PAMAL��AK��AJ��AI"�AHr�AGO�AFM�AFAE�FAE
=AD�ADZAC�AC+AB�`AB�uABQ�AB(�AA�AAx�AA%A@��A@bNA@E�A@1'A@1A?�A?t�A>�9A=|�A<ȴA<5?A;��A;��A;�PA;hsA;;dA;
=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111                                                                                                                                                                                                 A���AξwAβ-AζFAμjA���A�ƨA�ĜAκ^AζFAκ^Aκ^Aδ9Aΰ!AΩ�AΡ�AΛ�AΛ�AΝ�AΙ�AΕ�AΓuAΓuA΍PAΏ\A΍PA΋DA΅A�~�A�~�A�|�A�~�A�|�A�p�A�n�A�I�A�$�A�%AͶFA�Ả7A�?}A�x�A��A�K�A��A��A�(�A�K�A��A�XA��A��PA�ĜA�~�A�"�A��!A�(�A�I�A���A�jA�5?A���A���A���A�?}A�5?A�n�A��9A�/A��;A�dZA���A��A��#A��
A�$�A�C�A��!A��DA�;dA���A��A�1A�dZA~�Az��Av�RAs��Ao�-AlZAh��Ad��A`ĜAZ��AV�HAT��ARĜAP9XANVAI��AD��AB5?A@I�A=�A;VA9+A5oA6��A9;dA:{A81'A;K�A=oA<��A:{A:=qA9�A4jA1�A/C�A+�A*��A)\)A'��A&��A&^5A&{A&5?A%��A%&�A#�A#\)A#�A"�jA"=qA!�
A!�hA!l�A ��A 9XA�hA`BA�A%A�RAz�A��AhsA`BA��A33AG�AS�AK�AȴAM�AJA�TA��A�A�A��At�A��A��Av�A��AoA�+AƨAx�A%A��AZA$�A��AXA�AbNA(�A  A�PA�A�A��AVA��A�AK�A�An�A��A�AhsAG�A��AE�A�AƨA�FA��A��A�PAdZA33A
�/A
~�A
VA
9XA	��A	\)A	�A�A��AA�AbA��A��AO�A&�A��AȴAn�A$�AƨA�7AK�AA�A��A�+AM�A1'AA�A1'A(�AA�PA+A��AQ�A-A�wA&�A �RA �uA �\A �@��F@��@���@���@�Q�@�1'@���@�K�@���@�v�@��@��#@���@�p�@�(�@�K�@���@�E�@�@��-@�/@���@��;@�K�@���@�ff@�J@�x�@�V@��@�w@�t�@��y@�!@�{@�@��@�u@�F@���@�^@�9@���@�l�@�M�@噚@���@�A�@�@��@�@��@߾w@�;d@�+@ޟ�@��@���@�x�@܃@�C�@�
=@ڧ�@�-@�@�hs@���@ش9@�r�@� �@���@�l�@�J@�1@�+@���@ҧ�@�~�@�J@�p�@���@�Z@ϝ�@�S�@�
=@�ȴ@Ο�@��@�`B@̬@�1'@��m@ˍP@ʧ�@�=q@�p�@��@�A�@�ƨ@�;d@Ɨ�@��@�O�@���@�1@å�@ÍP@��@�v�@�5?@���@���@���@��@�G�@��@�(�@�\)@��@�J@�hs@�V@�I�@�1'@��@��w@�K�@��@�@�O�@�Ĝ@���@�(�@�C�@��R@�5?@���@���@�p�@��@���@�bN@� �@��
@��@���@���@��9@��@�9X@� �@���@��;@��w@�l�@��y@��\@��T@���@��h@�G�@�%@���@��`@��@�j@��P@�
=@�ȴ@�ff@��@��h@�hs@�`B@���@�r�@�I�@��@��m@��w@�S�@���@���@�n�@��@���@���@�G�@�Ĝ@�Q�@�(�@�  @�ƨ@�dZ@�o@���@��+@�$�@���@���@��u@�Z@�I�@�b@��
@��F@�t�@�+@��@���@���@�$�@��@���@��-@�x�@��@�%@���@��@�j@�9X@�ƨ@���@�dZ@��@��@�ff@��@��@���@��-@�x�@�G�@��@���@�9X@���@���@�l�@�;d@��H@��!@�v�@�^5@�-@��@���@�O�@��j@� �@���@�t�@�\)@�K�@�;d@�"�@��y@��\@�M�@�J@��T@���@�@�G�@��`@�r�@�(�@��m@�\)@�C�@�+@���@��!@��+@�-@���@�p�@�O�@�?}@��@���@�Ĝ@�z�@�bN@�Q�@�A�@��@��@�l�@�C�@�
=@��!@��+@��@��@�@��h@�x�@�p�@�X@��@�r�@�b@�t�@�"�@���@���@�v�@�5?@�@��T@�@��7@��@�O�@��@���@�Ĝ@��j@��@���@��D@�r�@�Z@�1@��w@�t�@���@���@�v�@�-@���@�p�@�G�@���@�bN@�9X@��@��@l�@\)@K�@~�@}�-@|�@{��@z�@z��@z=q@y��@y��@y�7@yhs@y�@x�`@xĜ@x��@xQ�@w�@vE�@u�@u/@t�@tz�@t1@s��@r�H@rJ@q�@q�@q��@qx�@q�@p�9@p�@o�;@oK�@n�R@nE�@m�h@m/@l��@l��@l�@k��@kdZ@j�@jn�@j=q@i�#@ix�@i%@h�9@hr�@g�w@g+@f��@f$�@e/@d9X@c�m@c��@c�@b��@b-@a��@a�^@aX@`r�@` �@_�@_;d@_+@_�@_�@_�@_
=@^��@^��@^�y@^ff@]`B@]V@\�/@\�@\j@\1@[C�@Z��@Z-@Y�^@X�`@XbN@Xb@W�@W�@V�+@V5?@U�@U`B@UV@T�j@Tz�@T(�@S�F@S��@R�!@Q��@QX@Q�@PbN@O�@O��@O\)@O;d@O+@N��@N��@N�+@NE�@N@M�@Mp�@LI�@K��@KdZ@Ko@J��@JM�@J�@I�#@IX@H��@H��@H�@Hr�@HbN@Hb@G�@G\)@G;d@G
=@F�@F�+@F{@E��@EO�@EV@D�/@D��@DI�@D(�@D�@D1@C��@C�m@C��@C33@B�@B��@Bn�@A��@A&�@@��@@Ĝ@@�@@bN@@1'@?�@?�P@?K�@>��@>�@>�R@>��@>��@>V@>{@=@=p�@=?}@<�j@<�D@<Z@<9X@<�@;�m@;ƨ@;�F@;dZ@:�H@:=q@9�@9x�@9G�@9%@8�u@8A�@81'@8b@7�@7��@7�@7��@7|�@7;d@6�R@6V@6@5�T@5��@5V@4��@4�j@49X@3��@3"�@2�\@2�@1G�@1&�@0r�@/�;@/�@/l�@/K�@.��@.ff@.E�@-@-?}@,��@,j@,Z@,I�@,(�@+�F@+o@*��@*^5@)��@)�7@)�7@)�7@)x�@)x�@)hs@)X@)�@(bN@'�@'�@'��@'|�@';d@'
=@&ȴ@&�+@&�+@&V@&E�@&E�@%�@%��@%O�@%V@$��@$Z@#��@"�@"�H@"�H@"��@"��@"^5@!�@!�^@!hs@!&�@ �`@ r�@ Q�@ Q�@ A�@ b@�;@�;@�w@�P@\)@;d@+@
=@��@ȴ@ff@5?@@��@`B@�@�@��@z�@I�@�@�@1@�m@�
@ƨ@ƨ@ƨ@ƨ@�F@�F@�F@��@��@�@dZ@C�@@~�@��@��@hs@7L@�`@�9@r�@A�@b@�w@;d@ȴ@��@��@��@V@@�-@�-@�h@O�@V@��@V@V@��@��@�@�@�D@j@I�@9X@1@�m@�
@��@C�@33@@��@��@�\@^5@=q@�@�@��@��@�7@x�@G�@7L@�@%@��@��@��@�9@r�@1'@ �@�@l�@;d@
=@�@�R@��@v�@V@E�@$�@@��@@�-@�h@�@p�@O�@/@�@�@��@j@(�@1@��@��@�m@��@�A���A���A�ĜA���A���A�ĜA�ȴAδ9Aΰ!Aβ-Aβ-AζFAθRAβ-AζFAζFAζFA�ĜA�ĜA���A���A�ĜA���A���A���A�ƨA���A�ĜAζFAζFAζFAκ^AθRAδ9Aδ9AμjA�AμjAκ^AζFAζFAζFAδ9Aβ-AθRAθRAζFAΰ!Aβ-Aβ-AήAήAβ-Aβ-AήAάAήAήAΣ�AΣ�AΣ�AΣ�AΡ�AΟ�AΡ�AΡ�AΟ�AΝ�AΝ�AΛ�AΙ�AΙ�AΝ�AΝ�AΙ�AΙ�AΛ�AΟ�AΝ�AΛ�AΝ�AΡ�AΛ�AΛ�AΝ�AΝ�AΙ�AΙ�AΙ�AΛ�AΙ�AΕ�AΗ�AΗ�AΗ�AΓuAΓuAΗ�AΗ�AΓuAΑhAΕ�AΕ�AΓuAΏ\AΑhAΕ�AΗ�AΑhAΑhAΓuAΑhA΍PA΋DAΏ\AΑhA΍PAΉ7A΋DAΑhAΑhA΍PA΍PAΏ\AΑhAΏ\A΋DAΉ7A΍PAΏ\A΍PA΋DA΋DA΋DA΍PA·+A΅A·+AΉ7A΅A΃A΁A΅A΅A΁A�|�A�|�A�~�A΁A�|�A�|�A΁A�~�A΁A�|�A�z�A�|�A΁A�~�A�|�A�z�A�|�A�~�A΁A�~�A�|�A΁A΃A΁A�|�A�|�A�z�A�x�A�p�A�n�A�n�A�t�A�p�A�jA�jA�r�A�n�A�r�A�p�A�hsA�\)A�\)A�VA�;dA�1'A�(�A�+A�&�A�&�A�$�A� �A��A�oA�VA�A���A�A�  A��A���A͸RA͡�A͙�A͍PA�x�A�^5A�A�A�
=A̺^A̬A̡�A̟�A̙�A̍PÁA�v�A�t�A�t�A�jA�S�A�=qA��A�%A��A�ĜAˣ�A�|�A�ZA� �A�{A���A���A���A���A��A��`A���Aʝ�AʑhA�~�A�ZA�-A�oA��
Aɥ�AɁA�bNA�G�A�-A��AȸRAȇ+A��A�K�A��yA�Q�A�oA�E�A��A��jA��RA�  A��^A��A���A��TA�"�A���A�;dA��`A��9A�G�A�?}A�S�A�JA�r�A��A�M�A���A���A��A���A���A���A��DA�n�A�bNA�Q�A�A�A�-A�-A�+A�
=A��#A��A�~�A�C�A��mA�|�A�$�A�1A��!A��TA���A��uA��A�dZA�(�A��HA��^A���A�v�A�9XA�K�A�1A��A���A���A���A���A���A��#A��`A��A�%A�{A�{A�oA���A���A��A��`A���A�A���A���A���A���A�ĜA��9A���A��+A�~�A��A�z�A�^5A�G�A�;dA�5?A�&�A��A�
=A��yA��!A�v�A�(�A�bA�A��yA�ƨA��hA�dZA�I�A�A�A�9XA�9XA�7LA�5?A�1'A�(�A� �A��A��A��A��A�bA�
=A��A���A��jA���A���A��PA�l�A�^5A�VA�E�A�;dA�;dA�5?A��A�1A�%A�{A�VA���A��RA���A�z�A�Q�A�
=A��wA�|�A�\)A�I�A���A��
A��A��DA�M�A�;dA�G�A�Q�A�\)A�bNA�dZA�hsA�hsA�p�A�z�A��A�v�A�jA�VA�E�A�1'A�&�A� �A��A�A���A��mA��;A��#A��A���A���A���A��wA���A��\A���A���A��^A���A���A���A��DA�~�A�XA�/A�oA�VA�1A�A��;A�ĜA��-A���A���A���A��hA�x�A�?}A��A��TA�ȴA��FA���A��+A�l�A�Q�A�33A��A�  A��TA�ȴA���A��-A���A�x�A�p�A�hsA�\)A�G�A�5?A�{A��A��;A���A�ȴA��FA���A��DA��A�\)A�XA�M�A�A�A�9XA�33A�$�A�"�A��A�{A�bA�
=A�  A���A��A��A��A���A���A�ȴA��A��A�~�A��A�~�A�z�A�r�A�dZA�K�A�G�A�E�A�C�A�7LA��A�%A��A��9A���A���A�z�A�;dA�"�A���A�v�A�
=A��;A���A�ZA�1A�ĜA�&�A��jA��DA�K�A�bA��#A���A��\A�n�A�`BA�"�A���A���A�|�A�O�A��A���A�+A��A��FA�~�A�1'A��
A���A�/A��9A�p�A�33A�  A���A���A��hA��7A��+A�^5A�%A��uA� �A���A�5?A��;A���A�7LA��wA�7LA�ƨA�33A��yA���A�|�A�Q�A���A��9A��PA�  A�r�A��mA���A�I�A�oA��TA���A���A���A��hA�x�A�n�A�l�A�^5A�ZA�XA�S�A�O�A�K�A�M�A�G�A�?}A�/A��A�
=A��A���A�jA�E�A�%A�A�|�A�$�A���A��
A��A�A�A�1A��
A��7A�5?A�ĜA�v�A�K�A�5?A�+A��A�{A�JA���A��`A���A��-A���A��DA�z�A�ZA�K�A�?}A�1'A��A���A�JA7LA~�RA~5?A}�A|��A|E�A|bA{�A{�-Az��Ay�#Ay�AxM�Aw�FAw�PAw`BAv��Av��Av=qAu�FAup�At�`As�Asx�AshsAs�FAs?}Arn�ArbAq�hAp��Ao��Ao&�An��An�`Am��Al��Al�Al�HAlȴAl�uAlVAk��Ak�Ajn�Aj1Aip�Ah�Ah�Ah{AgAg|�AgK�Af�yAfbAd��AcƨAb�AbZAa��Aa��AaC�A`��A`��A`z�A_�TA^�A]O�A\�A[VAY�AY`BAX��AX �AW��AW;dAWAV��AV��AVn�AV-AV(�AVJAUATȴAT5?AT1'AT(�AS��AS�AS33AR��ARz�AR1'AQ��AQ��AQK�AP�AP-AO��AO�7AOdZAOS�AO
=AN�/AN�!AN�AN9XAM�PAMAL��AK��AJ��AI"�AHr�AGO�AFM�AFAE�FAE
=AD�ADZAC�AC+AB�`AB�uABQ�AB(�AA�AAx�AA%A@��A@bNA@E�A@1'A@1A?�A?t�A>�9A=|�A<ȴA<5?A;��A;��A;�PA;hsA;;dA;
=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111                                                                                                                                                                                                 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	F?B	FB	E9B	E9B	EB	EmB	EmB	E9B	EmB	EB	C�B	EB	D�B	E9B	E9B	E9B	EB	D�B	D�B	EB	D�B	D�B	D�B	DgB	D3B	DgB	DgB	C�B	C�B	B�B	B�B	B�B	B�B	@�B	?�B	<�B	7�B	4nB	,B	qB	�B	�B	%FB	6B	S�B	ZQB	��B
G�B
�B
�"B
��B
�B
��B
�B!-B*0B'�B0UBU�BQ�BWsB\�BTaB]�BT�BT,BU�BYBZ�B[WB`�Bp�BiB9XB 4B
��B
�7B
u�B
QB
:*B
3�B
'RB
�B	��B	�B	��B	�[B	�-B	��B	��B	��B	o�B	Z�B	:�B	)_B	�B	�B	�B	�B	B	�B	
rB��B��B�`B�B�|B�B	
�B	?�B	tB	c�B	��B	̘B	��B	��B
	�B
,�B
�B	�B	�TB	�dB	�&B	�B	��B	��B	�B	��B
�B

�B
hB
CB
xB
IB
!B
%�B
.}B
/�B
/�B
1�B
6FB
6zB
6�B
7�B
6B
6B
9XB
>wB
@�B
H�B
L�B
V�B
[WB
]�B
`vB
d&B
c�B
d�B
d�B
e�B
iyB
l�B
oiB
p�B
rGB
q�B
r�B
r|B
qB
n�B
n/B
n�B
r�B
r|B
n/B
oiB
m]B
jKB
iB
gmB
iDB
lWB
l�B
h�B
h>B
h�B
i�B
iDB
f�B
e`B
dZB
bNB
cTB
`vB
_�B
^�B
_B
\]B
Z�B
[#B
\)B
\]B
\)B
\)B
[�B
Z�B
[�B
YB
XEB
W�B
X�B
T�B
TaB
R�B
S[B
QNB
P�B
P�B
P�B
N�B
N<B
M�B
M6B
LdB
K^B
J�B
J#B
JXB
I�B
I�B
HKB
G�B
HB
GEB
H�B
J�B
JXB
I�B
G�B
E9B
D3B
B�B
@B
@�B
=�B
:�B
9�B
:�B
9�B
7�B
8B
4�B
4�B
1'B
0�B
/�B
1�B
0!B
/OB
.IB
-wB
,qB
,qB
-�B
,B
)�B
*eB
(�B
(XB
(�B
(�B
($B
'�B
&�B
'B
'�B
&�B
&B
&�B
$@B
$tB
#nB
#B
$B
#:B
!bB
!-B
 �B
 'B
 �B
�B
IB
~B
�B
�B
	B
kB
�B
�B
�B
B
+B
$B
�B
�B
�B
MB
{B
B
B
�B
{B
uB
�B
uB
@B
uB
B
oB
hB
bB
B
�B
�B
\B
�B
(B
(B
.B
(B
(B
�B
�B
uB
�B
uB
�B
B
B
B
�B
�B
MB
{B
�B
�B
�B
�B
�B
MB
B
�B
SB
YB
�B
�B
�B
YB
YB
�B
YB
$B
$B
$B
$B
�B
�B
�B
kB
7B
kB
	B
kB
7B
�B
	B
�B
xB
CB
CB
�B
B
�B
B
B
xB
B
CB
B
IB
IB
�B
CB
~B
IB
xB
B
xB
�B
B
B
B
B
~B
�B
OB
�B
OB
OB
�B
OB
OB
B
�B
B
�B
�B
�B
�B
VB
VB
!B
�B
 �B
 �B
 �B
 �B
 �B
 �B
!�B
!-B
!bB
!bB
!�B
!�B
!�B
"4B
"hB
#:B
#B
#nB
#nB
#�B
$@B
$B
$�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
(XB
($B
($B
'�B
(�B
(�B
(�B
(�B
(�B
)*B
)�B
)�B
*0B
*0B
*�B
+B
,=B
,=B
-B
-�B
.}B
/OB
/�B
/OB
/�B
/�B
/�B
/�B
/�B
/�B
/�B
/OB
/�B
/OB
/�B
0!B
0UB
0UB
0!B
/�B
/�B
/�B
/�B
0UB
0�B
0�B
0�B
0UB
0UB
0UB
0UB
0�B
0�B
1'B
1[B
1[B
1'B
0�B
2aB
1�B
2�B
2�B
33B
49B
3�B
4B
4�B
4�B
4�B
5?B
6B
6FB
6B
6B
6FB
6zB
6�B
7�B
7LB
7B
7B
7�B
8RB
8B
8B
8�B
9$B
8�B
9�B
9�B
9�B
:*B
:*B
9�B
9�B
:�B
;0B
;�B
<�B
<�B
=qB
=B
=<B
=�B
=�B
=�B
=�B
>B
>B
>BB
>wB
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
?}B
?B
?�B
A B
@�B
@�B
AUB
A�B
B[B
B'B
C-B
C�B
D3B
D�B
D�B
D�B
DgB
D3B
D�B
E�B
F�B
GzB
GzB
G�B
HB
HKB
HKB
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
J�B
J�B
J�B
J�B
K)B
K^B
K�B
LdB
LdB
LdB
K�B
L0B
L0B
LdB
LdB
K�B
M6B
M6B
MjB
M�B
N<B
N<B
NB
M�B
NpB
N�B
OB
OvB
O�B
O�B
O�B
PHB
P}B
P�B
P}B
QNB
QNB
Q�B
Q�B
R�B
S[B
S&B
S[B
S&B
T�B
T,B
TaB
TaB
T�B
U2B
UgB
U�B
VmB
VB
VB
V9B
VB
VB
U�B
U�B
U�B
VmB
W
B
V�B
W
B
V�B
V�B
WsB
XyB
YB
X�B
Y�B
Z�B
Z�B
Z�B
[WB
[�B
\)B
\]B
\]B
\�B
\�B
\�B
\�B
\�B
]/B
]/B
]�B
]�B
^B
^5B
_pB
_B
_�B
_�B
_�B
_�B
`B
`BB
_�B
`BB
`B
`B
`BB
a|B
`vB
`�B
`�B
a|B
a�B
a�B
a�B
c B
c B
cTB
cTB
cTB
cTB
c�B
d&B
dZB
d�B
d�B
d�B
e,B
e`B
e�B
e�B
e�B
e�B
e�B
f�B
ffB
f�B
f2B
f2B
f2B
f�B
gB
f�B
f�B
gB
g�B
hsB
g�B
hsB
hsB
hsB
h�B
h�B
iB
iB
iDB
iDB
iyB
iDB
iDB
iyB
iyB
i�B
jB
jKB
j�B
kB
kB
kB
kQB
kB
kQB
kB
kB
k�B
l"B
l"B
l�B
l�B
l�B
m)B
m�B
m]B
m�B
m�B
m�B
m�B
m�B
m�B
n/B
ncB
n�B
n�B
ncB
n�B
o B
n�B
n�B
o B
oiB
o�B
pB
poB
qB
p�B
qvB
q�B
q�B
q�B
q�B
r�B
rGB
r|B
r�B
s�B
s�B
tB
tB
s�B
s�B
tTB
t�B
u%B
uZB
v�B
v+B
v+B
v+B
v`B
v+B
v`B
v+B
v�B
wfB
w�B
w�B
w�B
w�B
xB
xB
x�B
xlB
xlB
x�B
x�B
xlB
x�B
y	B
yrB
y	B
y�B
y�B
z�B
z�B
{B
z�B
{B
{B
{B
{�B
{�B
|PB
|�B
|�B
}�B
}VB
}�B
}�B
}�B
~(B
}�B
~(B
~]B
~�B
~�B
~�B
~�B
~�B
~�B
.B
~�B
cB
�B
� B
�B
�;B
�oB
��B
��B
�B
�B
�B
�AB
�AB
�uB
�uB
�uB
�AB
�uB
�uB
�uB
�uB
�uB
�uB
��B
��B
�B
�{B
�B
�MB
��B
��B
�B
�B
�B
��B
�B
��B
�%B
��B
�+B
�+B
�+B
��B
��B
��B
�1B
�fB
��B
�B
�B
�B
��B
�B
�lB
��B
��B
��B
�	B
�	B
�	B
�rB
�=B
�rB
�B
�xB
�xB
��B
��B
��B
�B
�B
�~B
�~B
��B
��B
��B
�B
��B
�PB
�PB
�PB
��B
��B
��B
��B
��B
��B
�"B
��B
�"B
��B
��B
�(B
��B
��B
��B
��B
��B
��B
��B
�.B
�bB
�bB
�bB
��B
��B
��B
��B
� B
��B
�4B
�hB
��B
��B
�:B
�B
�:B
�:B
��B
�oB	HKB	HKB	EmB	AUB	HKB	IB	C�B	D�B	FB	E�B	D�B	E�B	EB	E�B	D�B	FB	FB	A�B	C�B	E9B	FtB	DgB	B�B	FtB	G�B	C�B	EB	FtB	F�B	E�B	EB	C�B	C�B	E�B	E9B	B�B	DgB	D�B	EB	FtB	D�B	D3B	C�B	E�B	C�B	C-B	EmB	GB	EmB	C�B	E�B	F?B	D�B	D�B	EmB	FtB	E9B	D�B	E�B	F�B	EmB	EB	D3B	FB	EmB	DgB	DgB	E�B	D�B	EB	E�B	FB	C�B	C�B	D�B	EB	D�B	B�B	D�B	E�B	EB	C�B	EmB	E9B	D�B	C�B	D�B	FB	D�B	C�B	EB	E�B	EB	C�B	DgB	FB	E�B	DgB	D3B	E�B	EmB	D�B	CaB	DgB	FB	D�B	C�B	C�B	E9B	EB	C�B	C�B	D�B	E�B	D3B	CaB	D3B	E9B	D�B	C�B	C�B	EB	E9B	DgB	C-B	DgB	E�B	E9B	D�B	C-B	D3B	E�B	C�B	B�B	CaB	EmB	EmB	EmB	C-B	C�B	D3B	EB	C�B	B�B	C�B	C�B	C�B	B�B	B�B	B�B	C-B	C�B	D�B	A�B	B�B	DgB	B�B	B'B	B'B	CaB	C�B	B�B	B'B	B'B	B�B	DgB	B�B	B'B	B�B	DgB	B�B	A�B	@�B	D3B	A�B	@�B	@�B	>wB	?�B	@�B	=�B	?�B	?�B	?}B	CaB	>BB	>BB	>�B	:*B	:*B	;�B	:�B	7LB	6zB	7LB	7�B	6�B	49B	2-B	;�B	1�B	2�B	1�B	0UB	2�B	4nB	)�B	'�B	$B	'RB	$�B	�B	�B	�B	VB	�B	�B	�B	�B	�B	"B	�B	B	�B	1B	�B	�B	 �B	 �B	%�B	'�B	 'B	'�B	$@B	+�B	1[B	/�B	0�B	2�B	7�B	=qB	GzB	K�B	K�B	NpB	YKB	X�B	V�B	[�B	Z�B	Y�B	Y�B	U�B	V�B	\]B	^5B	]/B	jB	��B	�BB	�)B	�|B	�sB	�2B	��B	�B
�B
$�B
(B
s�B
�	B
_pB
[�B
d�B
e,B
\�B
s�B
�hB
��B
�B
�B
�YB
�(B
�\B
�rB
�7B
��B
��B
��B
�_B
�xB
��B
�~B
��B
��B
��B
�DB
��B
�B
��B
�4B
��B
�B
��B
��B
��B
�4B
��B
�_B
��B
��B
�B
�4B
��B
�rB
�	B
�~B
��B
�B
��B
��B
�CB
�kB
�B
�B
�B
�OB
�tB
��B
��B
�B
ΥB
�HB
��B
�&B
��B
�&B
�9B
�B
� B
�B
�B
�B
�B
��B
��B
��B
�xB
�JB
�(B
�"B
�8B
�xB
��B iB�B%B�BB7B�B(B�BuB�B#:B#nB&�B&�B)_B)�B)�B*�B*�B-�B,�B+B(�B(XB(XB'�B)*B.IB'RB+B&B&�B&�B)�B#�B$B%�B$B$�B($B.IB33BAUBJ�BQBV�BY�BP�BT�BT�B\]BYBW�BQ�BR�B\]BPHBPHBNpBT�BD�BM�BRTBQ�BT�BS�BU�BX�BTaBYB\�B_pB`BaHB_�B_B[�BZ�B[#BYBVmBWsBS�BS[BS&BS�BR�BTaBS�BVBTaBIBb�Bb�Bd�BbBa�B`BaBd�Bc�BZ�BT,BS[BR�BV9BQBLdBJ�BR�BR�BP�BTaBW?BS�BV�BT,BV�BX�BZQBW�BW�BX�BS�BR�BR�BP�BM6BNpBR�BS�BYB^5B`�B`BB^�BbB^�BZ�BXBY�BY�BYBX�BX�BZ�BWsBW�BYBYB[�B\]B\�B^B\�B\�B^jB]�B^jB]/B\]Be�B`�Ba�BcTBh�Bm]BiDBkBo5Bo�BtTBu%Bv�BqvBqABq�Bq�Bt�Bm�BoiBm�BgBc�Bg8Bf2B\�B^�BgmBOvBJ#BG�BCaB;0B?�B@�B+B!�B&�B�BB\B�B	lB
�cBB
�]B
�|B
��B
��B�B
�yB
�NB
��B
�KB
�B
��B
��B
�B
�HB
��B
��B
��B
�@B
��B
�IB
�eB
��B
�uB
��B
�B
�B
��B
�{B
�DB
��B
{B
��B
|�B
w�B
v`B
rB
`�B
gmB
W�B
`�B
ZQB
QB
QNB
ZQB
V9B
S�B
H�B
D�B
C-B
@�B
>�B
<jB
:*B
:�B
;0B
9�B
8B
5B
7�B
6�B
6B
5B
5�B
2�B
4�B
0�B
2-B
2�B
1[B
3�B
2-B
)�B
(�B
*�B
#�B
'B
 �B
�B
�B
�B
�B
�B
�B
.B
�B
�B
B
YB
B
B
�B	��B	�]B	��B	�]B	��B	�rB	�B	�ZB	��B	��B	�vB	�B	��B	��B
�B	��B	�B	�)B	�)B	�vB	�sB	�EB	ĜB	��B	��B	�dB	�B	�OB	�XB	�$B	��B	�4B	�B	��B	�VB	�-B	�B	��B	��B	��B	{JB	��B	�IB	�hB	��B	��B	�	B	��B	��B	|�B	}�B	�-B	�B	�B	�B	�{B	�fB	�1B	�JB	�SB	cB	uZB	xlB	rGB	m]B	h>B	c�B	`vB	_�B	a�B	f�B	`B	W�B	W
B	G�B	C�B	<jB	@B	5�B	5B	6�B	8�B	;�B	F?B	0�B	1�B	#�B	B		B	kB	�B	eB	�B	�B	�B	�B	{B	{B	�B	�B	�B	oB	
�B	B	(B	(B	�B	�B	�B	�B	CB	kB	xB	�B	 �B	1B	+B	�B	�B	�B	{B	B	�B	�B	�B	
rB	uB	�B	!�B	L0B	�B	+�B	:B	B	�B	
rB	hB	
rB	�B	�B	 iB�B�PB�JB�(B	 4B��B��B�B�B�5B��B�;B�%B��B	B��B��B�B��B�sB�B�B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111                                                                                                                                                                                                 B	?�B	?�B	>�B	>�B	>�B	?B	?B	>�B	?B	>�B	=�B	>�B	>�B	>�B	>�B	>�B	>�B	>NB	>�B	>�B	>�B	>NB	>�B	>B	=�B	>B	>B	=�B	=HB	<�B	<�B	<vB	<AB	:jB	9�B	6�B	1gB	. B	%�B	#B	kB	�B	�B	/�B	MuB	TB	��B
A`B
}�B
��B
�gB
�1B
��B�B�B#�B!�B*BOMBK5BQ%BVDBNBW~BN|BM�BO�BR�BT8BU	BZ\Bj�Bb�B3
B
��B
�cB
��B
o@B
J�B
3�B
-NB
!B
UB	�IB	�hB	�{B	�B	��B	��B	�OB	�B	i�B	T�B	4�B	#B	EB	�B	3B	RB	�B	�B	$B�wB�B�B�eB�.B�CB	�B	9cB	m�B	]�B	�CB	�JB	�=B	�kB
RB
&�B
	CB	�lB	�B	�B	��B	�1B	�B	�B	�bB	�B	��B
XB
B
�B
*B
�B
�B
aB
(/B
)5B
)5B
+�B
/�B
0,B
0�B
12B
/�B
/�B
3
B
8)B
:jB
B�B
FB
PSB
U	B
W~B
Z(B
]�B
]oB
^AB
^�B
_GB
c+B
frB
iB
jVB
k�B
k\B
l�B
l.B
j�B
h~B
g�B
hJB
l�B
l.B
g�B
iB
gB
c�B
b�B
aB
b�B
f	B
f=B
b�B
a�B
b�B
c_B
b�B
`�B
_B
^B
\ B
]B
Z(B
YVB
XPB
X�B
VB
TlB
T�B
U�B
VB
U�B
U�B
U�B
T�B
UrB
S1B
Q�B
Q�B
R`B
NGB
NB
LoB
MB
K B
JcB
JcB
JcB
HWB
G�B
GQB
F�B
FB
EB
DsB
C�B
D
B
C8B
ClB
A�B
A`B
A�B
@�B
B�B
DsB
D
B
C�B
A�B
>�B
=�B
<�B
9�B
:5B
7WB
4�B
3>B
4EB
3�B
1�B
1�B
.TB
.�B
*�B
*�B
)�B
+vB
)�B
)B
'�B
')B
&#B
&#B
'�B
%�B
#�B
$B
"�B
"
B
"�B
"?B
!�B
!�B
 �B
 �B
!mB
 gB
�B
 3B
�B
&B
 B
�B
�B
�B
B
�B
wB
�B
wB
�B
�B
0B
dB
XB
�B
B
LB
LB
�B
�B
�B
�B
9B
�B
�B
�B
-B
�B
�B
[B
-B
'B
[B
'B
�B
'B
�B
!B
B

B
�B

IB
	CB
	B
qB
�B
�B
	�B
�B
�B
	�B
�B
'B
�B
'B
aB
�B
�B
�B
�B
�B
�B
-B
3B
�B
3B
�B
gB
�B
�B
gB
B
B
9B
�B
?B
B
B
tB
B
�B
�B
�B
�B
zB
EB
zB
B
�B
B
�B
B
�B
RB
�B
RB
*B
�B
�B
XB
�B
dB
�B
�B
*B
�B
�B
�B
�B
�B
^B
�B
0B
�B
*B
�B
*B
^B
�B
�B
�B
�B
0B
�B
B
6B
B
B
jB
B
B
�B
�B
�B
�B
jB
jB
�B
B
B
�B
�B
BB
�B
wB
�B
�B
�B
}B
�B
B
B
�B
HB
}B
�B
B
�B
�B
 B
 B
�B
�B
�B
�B
aB
aB
 gB
 3B
 3B
 3B
 gB
 �B
!mB
"
B
!�B
!�B
!�B
"?B
"�B
"�B
"sB
"�B
"�B
#yB
#EB
#�B
#�B
$�B
$�B
%�B
%�B
&�B
'�B
(/B
)B
)jB
)B
)5B
)5B
)jB
)jB
)5B
)jB
)jB
)B
)jB
)B
)�B
)�B
*B
*B
)�B
)jB
)jB
)5B
)jB
*B
*pB
*pB
*pB
*B
*B
*B
*B
*pB
*pB
*�B
+B
+B
*�B
*pB
,B
+�B
,|B
,�B
,�B
-�B
-�B
-�B
.TB
.TB
.�B
.�B
/�B
/�B
/�B
/�B
/�B
0,B
0`B
12B
0�B
0�B
0�B
1gB
2B
1�B
1�B
28B
2�B
2�B
3�B
3sB
3�B
3�B
3�B
3�B
3�B
4EB
4�B
5KB
6�B
6QB
7#B
6�B
6�B
7WB
7WB
7�B
7�B
7�B
7�B
7�B
8)B
8]B
8�B
8]B
8]B
8�B
8]B
8�B
8]B
9/B
8�B
9�B
:�B
:jB
:jB
;B
;pB
<B
;�B
<�B
=HB
=�B
>NB
>NB
>NB
>B
=�B
>�B
?�B
@ZB
A,B
A,B
A�B
A�B
A�B
A�B
B2B
B2B
BfB
BfB
BfB
B2B
B�B
C�B
D>B
D�B
DsB
DsB
D�B
EB
EDB
FB
FB
FB
E�B
E�B
E�B
FB
FB
E�B
F�B
F�B
GB
G�B
G�B
G�B
G�B
G�B
H"B
H�B
H�B
I(B
I]B
I]B
I�B
I�B
J/B
JcB
J/B
K B
K B
KiB
KiB
L�B
MB
L�B
MB
L�B
NGB
M�B
NB
NB
N�B
N�B
OB
O�B
PB
O�B
O�B
O�B
O�B
O�B
O�B
O�B
OMB
PB
P�B
P�B
P�B
P�B
P�B
Q%B
R+B
R�B
R�B
S�B
T8B
T8B
T8B
U	B
UrB
U�B
VB
VB
VxB
VDB
VDB
VDB
V�B
V�B
V�B
WJB
W~B
W�B
W�B
Y"B
X�B
YVB
YVB
YVB
YVB
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
[.B
Z(B
Z\B
Z�B
[.B
[cB
[cB
[cB
\�B
\�B
]B
]B
]B
]B
]oB
]�B
^B
^AB
^uB
^AB
^�B
_B
_GB
_�B
_{B
_{B
_�B
`MB
`B
`MB
_�B
_�B
_�B
`�B
`�B
`�B
`�B
`�B
aSB
b%B
a�B
b%B
b%B
b%B
bYB
bYB
b�B
b�B
b�B
b�B
c+B
b�B
b�B
c+B
c+B
c�B
c�B
c�B
deB
d�B
d�B
d�B
eB
d�B
eB
d�B
d�B
elB
e�B
e�B
f=B
f=B
f�B
f�B
gCB
gB
gxB
gCB
gxB
g�B
gxB
gxB
g�B
hB
hJB
hJB
hB
h~B
h�B
h~B
hJB
h�B
iB
iPB
i�B
j!B
j�B
jVB
k(B
k�B
k\B
k�B
k\B
lbB
k�B
l.B
lbB
mhB
m�B
m�B
m�B
m�B
m�B
nB
n�B
n�B
oB
pFB
o�B
o�B
o�B
pB
o�B
pB
o�B
pFB
qB
qLB
q�B
qLB
q�B
q�B
q�B
rSB
rB
rB
r�B
rSB
rB
r�B
r�B
s$B
r�B
sYB
sYB
t_B
t�B
t�B
t�B
t�B
t�B
u1B
ueB
u�B
vB
v7B
v�B
w=B
wB
w=B
w=B
wqB
w�B
w�B
w�B
xB
xCB
xCB
xCB
xwB
xwB
x�B
x�B
x�B
yB
y~B
y�B
z�B
z�B
{!B
{UB
{�B
{�B
{�B
{�B
{�B
{�B
|'B
|'B
|'B
{�B
|'B
|'B
|'B
|'B
|'B
|'B
|\B
|\B
|�B
}-B
}�B
}�B
~3B
~�B
~�B
~�B
~�B
~�B
~�B
nB
�B
�tB
��B
��B
��B
�FB
��B
��B
��B
�B
��B
��B
��B
��B
��B
��B
�B
�RB
�RB
��B
��B
��B
��B
�$B
��B
�$B
��B
�*B
�*B
�^B
��B
��B
��B
��B
�0B
�0B
��B
�eB
��B
��B
��B
�B
�B
�B
�6B
�6B
�6B
�6B
�6B
��B
��B
��B
��B
��B
��B
��B
�CB
�CB
�CB
�wB
��B
�wB
��B
��B
�B
�B
�B
�IB
�}B
�IB
�}B
��B
�}B
��B
�B
��B
��B
��B
��B
��B
��B
��B
�!B	A�B	A�B	?B	;B	A�B	B�B	=|B	>NB	?�B	?TB	>�B	?TB	>�B	?TB	>NB	?�B	?�B	;;B	=|B	>�B	@&B	>B	<vB	@&B	A`B	=HB	>�B	@&B	@�B	?TB	>�B	=HB	=HB	?TB	>�B	<AB	>B	>NB	>�B	@&B	>NB	=�B	=|B	?TB	=�B	<�B	?B	@�B	?B	=�B	?TB	?�B	>�B	>NB	?B	@&B	>�B	>�B	?�B	@ZB	?B	>�B	=�B	?�B	?B	>B	>B	?�B	>�B	>�B	?TB	?�B	=|B	=�B	>�B	>�B	>�B	<�B	>�B	?TB	>�B	=|B	?B	>�B	>NB	=|B	>�B	?�B	>�B	=|B	>�B	?TB	>�B	=�B	>B	?�B	?TB	>B	=�B	?TB	?B	>NB	=B	>B	?�B	>�B	=�B	=HB	>�B	>�B	=�B	=�B	>�B	?�B	=�B	=B	=�B	>�B	>NB	=|B	=HB	>�B	>�B	>B	<�B	>B	?TB	>�B	>NB	<�B	=�B	?�B	=|B	<vB	=B	?B	?B	?B	<�B	=�B	=�B	>�B	=|B	<vB	=|B	=�B	=�B	<AB	<vB	<�B	<�B	=|B	>NB	;�B	<vB	>B	<AB	;�B	;�B	=B	=|B	<�B	;�B	;�B	<�B	>B	<vB	;�B	<AB	>B	<AB	;pB	:jB	=�B	;;B	:�B	:jB	8)B	9�B	:�B	7�B	9cB	9cB	9/B	=B	7�B	7�B	8�B	3�B	3�B	5KB	4�B	0�B	0,B	0�B	1gB	0�B	-�B	+�B	5KB	+vB	,|B	+�B	*B	,�B	. B	#yB	!9B	�B	!B	�B	�B	�B	�B	B	6B	eB	eB	�B	eB	�B	
IB	�B	�B	�B	6B	<B	�B	�B	aB	!9B	�B	!�B	�B	%QB	+B	)jB	*pB	,�B	1gB	7#B	A,B	E�B	E�B	H"B	R�B	R�B	PSB	U>B	T8B	SfB	S�B	OMB	P�B	VB	W�B	V�B	d1B	�zB	��B	��B	�.B	�%B	��B	�rB	��B
<B
�B
�B
mhB
��B
Y"B
UrB
^uB
^�B
VxB
m4B
�B
��B
yIB
��B
�B
��B
�B
�$B
��B
�B
�tB
�@B
�B
�*B
��B
�0B
�qB
�<B
�^B
��B
��B
��B
��B
��B
�gB
��B
��B
��B
��B
��B
��B
�B
��B
��B
��B
��B
�OB
�$B
��B
�0B
�gB
��B
��B
�pB
��B
�B
��B
��B
��B
�B
�&B
��B
�]B
��B
�WB
��B
ΰB
��B
�|B
��B
��B
׳B
�B
��B
��B
�:B
�B
�7B
�kB
�=B
�*B
��B
��B
��B
��B
�*B
�=B
�B
�\B
��B�B�B�B�B�B	�B'BRB�B B 3B �B#B#EB#EB$KB$�B'^B&WB$�B"sB"
B"
B!9B"�B'�B!B$�B�B 3B gB#�B�B�BaB�B[B!�B'�B,�B;BDsBJ�BP�BS�BJcBNGBNGBVBS1BQZBKiBL�BVBI�BI�BH"BN�B>�BG�BLBK5BNGBMABO�BR`BNBR�BVDBY"BY�BZ�BY�BX�BUrBT�BT�BS1BPBQ%BM�BMBL�BMuBLoBNBM�BO�BNBB�B\4B\�B^uB[�B[cBY�BZ�B^uB]oBTlBM�BMBL;BO�BJ�BFBDsBL;BLoBJcBNBP�BMuBPSBM�BP�BR`BTBQZBQ�BR`BMABLoBL;BJcBF�BH"BL;BMABS1BW�BZ�BY�BXPB[�BXPBTlBQ�BSfBSfBR�BR�BR`BT�BQ%BQ�BS1BR�BU>BVBV�BW�BV�BVxBXBWJBXBV�BVB_GBZ�B[cB]BbYBgBb�Bd�Bh�BiPBnBn�Bp{Bk(Bj�Bk\Bk\Bn:BgxBiBgxB`�B]:B`�B_�BVxBXPBaBI(BC�BA�B=B4�B9�B:�B$�B�B gB
}B�B	B
��BB
�B
��B
�B
�.B
�CB
�B tB
�+B
� B
ħB
��B
��B
��B
��B
��B
��B
�EB
��B
�EB
��B
�BB
��B
�B
�aB
�'B
�EB
��B
��B
��B
�-B
��B
~�B
t�B
�@B
v7B
q�B
pB
k�B
Z�B
aB
QZB
Z�B
TB
J�B
K B
TB
O�B
MuB
BfB
>NB
<�B
:5B
8]B
6B
3�B
4yB
4�B
3>B
1�B
.�B
12B
0�B
/�B
.�B
/�B
,|B
.�B
*<B
+�B
,HB
+B
-NB
+�B
#EB
"?B
$KB
�B
 �B
�B
tB
nB
LB
gB
�B
�B
	�B
	�B
[B
�B
 B	��B	��B	��B	��B	�B	��B	�B	�7B	�$B	�B	�B	�B	�B	�(B	�VB	�~B	�xB
LB	�B	��B	��B	��B	�(B	�%B	��B	�NB	�vB	��B	�B	��B	�B	�
B	��B	�HB	��B	��B	�BB	�B	��B	��B	�}B	�6B	�gB	t�B	��B	��B	�B	�<B	�LB	��B	��B	:B	v�B	w=B	��B	y~B	}�B	}�B	}-B	�B	��B	��B	B	yB	oB	rB	k�B	gB	a�B	]�B	Z(B	YVB	[�B	`�B	Y�B	Q�B	P�B	A`B	=|B	6B	9�B	/ZB	.�B	0`B	2�B	5B	?�B	*<B	+BB	�B	�B	�B	B	^B	B	zB	�B	?B	�B	-B	-B	�B	RB	6B	!B	XB	�B	�B	�B	<B	�B	�B	9B	�B	B	*B	dB	wB	�B	�B	�B	�B	�B	-B	�B	
IB	�B		wB	$B	'B	6B	�B	E�B	�B	%�B	�B	�B	6B	$B	B	$B	 @B	zB�B�1B�B��B��B��B�eB��B�:B�4B��B�B��B��B�=B��B�LB�FB�JB�B�%B�1B��B�7G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111                                                                                                                                                                                                 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0061566                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0061566                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 24 11 2022 179 -0.0061566 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 24 11 2022 179 -0.0061566 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230426223302                            20230426223302AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023042622330220230426223302  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622330220230426223302QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F83E           783E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622330220230426223302QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               