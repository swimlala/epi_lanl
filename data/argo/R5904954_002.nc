CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:16:47Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        X  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ?�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  A`   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  G�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  IP   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  O�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  V    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  W�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  _�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  e�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  l8   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  m�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  u�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  |   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    |H   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    H   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �H   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �H   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �t   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �x   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �|   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005191647  20181005191647  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @ל��g1   @ל��n�@3,������c����m1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   A   @9��@�  @���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A���B   B  B  B��B��B(  B0  B8  B@  BHffBP  BX  B`  BhffBpffBx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�fC  C�C
  C�fC  C�C�C  C  C  C  C  C�fC�fC"  C$  C&  C(  C*  C,  C.  C0  C2  C4�C6  C8  C:  C<�C>  C?�fCA�fCD  CF�CH�CJ  CK�fCN  CP  CR  CT  CU�fCW�fCY�fC\  C^�C`  Cb�Cd  Cf  Ch  Cj  Cl�Cn  Co�fCq�fCs�fCu��Cw�fCz  C{�fC~  C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C��3C��3C�  C��C��C��C��C��C�  C��3C��3C��3C��3C��3C�  C��C��C��3C��3C��C��C��3C��3C�  C��3C�  C�  C��3C��3C��3C��3C��3C�  C��C��C�  C�  C�  C��3C��3C��3C��3C��3C��3C��3C�  C�  C��3C�  C��C�  C�  C��3C�  C�  C��3C��3C��3C�  C��C��C��C��C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��C��C�  C�  C��C��C��C��C�  C��3C��3C��C�  C��3C��C�  C��3C��C��3C�  C�  C�  C��C�  C��3C�  C�  C��C��C��3C�  C��C��3C��C��C��3C�  C��C�  C��3C�  C��C��3D   D � D  D� D��D�fD��D�fD  D� D��D�fD  D� DfD� D  D�fD	  D	� D
fD
�fD  D�fD  D� D  D� DfD� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D�fD   D � D!  D!� D"  D"� D#  D#�fD$  D$� D$��D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D+��D,� D-  D-� D.  D.� D.��D/�fD/��D0� D1fD1� D2fD2y�D3  D3y�D4  D4� D4��D5� D6  D6� D6��D7�fD7��D8�fD8��D9y�D:fD:� D;  D;�fD;��D<�fD=  D=y�D>fD>y�D?fD?�fD?��D@�fDA  DAy�DBfDBy�DC  DC�fDDfDDs3DD��DE� DF  DFy�DF��DG� DHfDH�fDIfDIy�DI�3Dy��D�9�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @:=q@�Q�@��A (�A (�A@(�A`(�A�{A�{A�{A�{A�{A�{A��HA��HB 
=B
=B
=B��B��B(
=B0
=B8
=B@
=BHp�BP
=BX
=B`
=Bhp�Bpp�Bx
=B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C �C�C��C�C)C
�C��C�C)C)C�C�C�C�C�C��C��C"�C$�C&�C(�C*�C,�C.�C0�C2�C4)C6�C8�C:�C<)C>�C?��CA��CD�CF)CH)CJ�CK��CN�CP�CR�CT�CU��CW��CY��C\�C^)C`�Cb)Cd�Cf�Ch�Cj�Cl)Cn�Co��Cq��Cs��Cu�\Cw��Cz�C{��C~�C�HC��{C�HC�HC�HC�HC�HC�C�HC�HC��{C��{C�HC�C�C�C�C�C�HC��{C��{C��{C��{C��{C�HC�C�C��{C��{C�C�C��{C��{C�HC��{C�HC�HC��{C��{C��{C��{C��{C�HC�C�C�HC�HC�HC��{C��{C��{C��{C��{C��{C��{C�HC�HC��{C�HC�C�HC�HC��{C�HC�HC��{C��{C��{C�HC�C�C�C�C�HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C�HC�C�C�HC�HC�C��C��C�C�HC��{C��{C�C�HC��{C�C�HC��{C�C��{C�HC�HC�HC��C�HC��{C�HC�HC�C�C��{C�HC�C��{C�C�C��{C�HC�C�HC��{C�HC�C��{D  �D ��D �D��D�>D�
D�>D�
D �D��D�>D�
D �D��D
D��D �D�
D	 �D	��D

D
�
D �D�
D �D��D �D��D
D��D �D�
D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �Dz>D �D��D �D��D �D�
D  �D ��D! �D!��D" �D"��D# �D#�
D$ �D$��D$�>D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D+�>D,��D- �D-��D. �D.��D.�>D/�
D/�>D0��D1
D1��D2
D2z>D3 �D3z>D4 �D4��D4�>D5��D6 �D6��D6�>D7�
D7�>D8�
D8�>D9z>D:
D:��D; �D;�
D;�>D<�
D= �D=z>D>
D>z>D?
D?�
D?�>D@�
DA �DAz>DB
DBz>DC �DC�
DD
DDs�DD�>DE��DF �DFz>DF�>DG��DH
DH�
DI
DIz>DI��Dy��D�9�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�$�A�$�A�(�A�+A�-A�-A�-A�-A�-A�1'A�1'A�1'A�1'A�1'A�33A�9XA�5?A�33A�-A�-A�1'A�1'A�33A�33A�33A�33A�5?A�5?A�5?A�33A�1'A�/A�1'A�/A�"�A��A�%AƼjAƍPA�M�A���A�\)A��HA�A\A��A��uA���A��TA�dZA�A���A��RA���A�r�A���A��7A��DA�v�A�  A��A�bA��jA��!A���A�O�A�|�A�  A�^5A��!A��A�C�A���A�%A���A�bNA�%A�A�A���A�p�A���A�I�A�?}A��A�+A�AO�A~=qA}7LA|��A|ZAy�7Av(�At�Ar�`Aq�Apv�Am�wAl�AgdZAe�^AehsAd�AbbNA^��A]&�A\z�A[&�AZ��AZ�HAY�wAWAS�FAP  AM��AL��AJ�AGl�AF��AE�TAB5?A=%A:~�A7�A4��A0�A.�A-��A-O�A,��A,�yA,M�A*bNA)�A'�A%��A%K�A%/A$��A$$�A#�;A#?}A"�jA" �A!��A!oA�A�/A��A{A�A
=A�A�HA��A�TAbA�AVA��A��A�A
=AZA��A1A�AQ�A`BA	��AI�AƨA�A�uAVA�hA�A1A I�@��7@�+@�@�`B@�?}@��9@�@�~�@��@��
@��@�t�@�C�@�"�@�@��@���@�+@�V@�$�@���@�/@�bN@�P@��H@��@��@���@�;d@��@�x�@@�ƨ@�"�@��@�J@���@�-@� �@�S�@⟾@ާ�@�n�@�C�@���@�x�@�9X@�v�@Ѳ-@�1'@�C�@��H@�5?@ͩ�@ͺ^@�@͙�@͉7@�`B@�G�@�&�@��@��@���@���@���@�j@�  @ǅ@�"�@Ə\@�{@Ł@�/@���@�j@��;@�"�@�V@���@�X@�Ĝ@�Q�@�o@�-@��T@��-@���@�x�@�%@��9@�j@�9X@�dZ@���@��H@���@���@��@�hs@�&�@���@���@��D@�|�@�C�@�C�@�;d@���@�=q@���@�X@�7L@�%@�r�@�b@���@���@��@���@�r�@�1@��P@��@��!@��+@�n�@�E�@��-@��@��D@�bN@�9X@� �@�b@��@���@��@�+@���@��#@�7L@���@�j@� �@�1@�  @���@���@��@��@��@���@��+@�hs@�Z@�9X@�1'@��
@��H@��R@�~�@�@�hs@�x�@��7@�X@�O�@��@��@���@�Q�@���@�l�@��@�"�@��@�v�@���@�Q�@���@�l�@�C�@�33@�;d@�C�@�;d@�
=@�o@��H@��+@�{@�J@��T@���@���@���@��-@��7@�%@���@���@�b@��;@�;d@�C�@�@�~�@�J@���@���@��h@�x�@��@��@��w@�"�@���@�`B@�/@��@��`@���@��@��u@vh
@f=q1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�$�A�$�A�(�A�+A�-A�-A�-A�-A�-A�1'A�1'A�1'A�1'A�1'A�33A�9XA�5?A�33A�-A�-A�1'A�1'A�33A�33A�33A�33A�5?A�5?A�5?A�33A�1'A�/A�1'A�/A�"�A��A�%AƼjAƍPA�M�A���A�\)A��HA�A\A��A��uA���A��TA�dZA�A���A��RA���A�r�A���A��7A��DA�v�A�  A��A�bA��jA��!A���A�O�A�|�A�  A�^5A��!A��A�C�A���A�%A���A�bNA�%A�A�A���A�p�A���A�I�A�?}A��A�+A�AO�A~=qA}7LA|��A|ZAy�7Av(�At�Ar�`Aq�Apv�Am�wAl�AgdZAe�^AehsAd�AbbNA^��A]&�A\z�A[&�AZ��AZ�HAY�wAWAS�FAP  AM��AL��AJ�AGl�AF��AE�TAB5?A=%A:~�A7�A4��A0�A.�A-��A-O�A,��A,�yA,M�A*bNA)�A'�A%��A%K�A%/A$��A$$�A#�;A#?}A"�jA" �A!��A!oA�A�/A��A{A�A
=A�A�HA��A�TAbA�AVA��A��A�A
=AZA��A1A�AQ�A`BA	��AI�AƨA�A�uAVA�hA�A1A I�@��7@�+@�@�`B@�?}@��9@�@�~�@��@��
@��@�t�@�C�@�"�@�@��@���@�+@�V@�$�@���@�/@�bN@�P@��H@��@��@���@�;d@��@�x�@@�ƨ@�"�@��@�J@���@�-@� �@�S�@⟾@ާ�@�n�@�C�@���@�x�@�9X@�v�@Ѳ-@�1'@�C�@��H@�5?@ͩ�@ͺ^@�@͙�@͉7@�`B@�G�@�&�@��@��@���@���@���@�j@�  @ǅ@�"�@Ə\@�{@Ł@�/@���@�j@��;@�"�@�V@���@�X@�Ĝ@�Q�@�o@�-@��T@��-@���@�x�@�%@��9@�j@�9X@�dZ@���@��H@���@���@��@�hs@�&�@���@���@��D@�|�@�C�@�C�@�;d@���@�=q@���@�X@�7L@�%@�r�@�b@���@���@��@���@�r�@�1@��P@��@��!@��+@�n�@�E�@��-@��@��D@�bN@�9X@� �@�b@��@���@��@�+@���@��#@�7L@���@�j@� �@�1@�  @���@���@��@��@��@���@��+@�hs@�Z@�9X@�1'@��
@��H@��R@�~�@�@�hs@�x�@��7@�X@�O�@��@��@���@�Q�@���@�l�@��@�"�@��@�v�@���@�Q�@���@�l�@�C�@�33@�;d@�C�@�;d@�
=@�o@��H@��+@�{@�J@��T@���@���@���@��-@��7@�%@���@���@�b@��;@�;d@�C�@�@�~�@�J@���@���@��h@�x�@��@��@��w@�"�@���@�`B@�/@��@��`@���@��@��u@vh
@f=q1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bq�Bq�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bq�Bp�Bp�Bp�Bp�Bp�Bq�Bp�Bp�Bp�Bp�Bp�Bp�Bq�Bq�Bq�Bq�Bp�Bp�Bp�Bo�Bo�Bo�Bo�Bo�Bo�Bo�Bn�Bo�Bq�Bt�B�B�VB��B��B��BB1B�B!�B,B33B9XBC�BZB^5BcTBcTBbNBZBZBYBVBW
B]/B\)BXBT�BA�B6FB)�B1B�B�mB��B��B�\B;dB%B
��B
��B
��B
�B
M�B
6FB
<jB
9XB
,B
"�B
�B
�B
bB
B	��B	�B	�ZB	�5B	��B	ÖB	�XB	��B	��B	�uB	�VB	~�B	jB	`BB	[#B	S�B	Q�B	O�B	H�B	<jB	!�B	
=B��B�B�5B��BȴBÖB�RB��B��B��B��B��B��B��B��B��B��B�{B�uB�\B�DB�=B�1B�7B�=B�PB�PB�\B�\B�\B�VB�VB�VB�DB�1B�B� B~�B~�B}�B{�Bx�Bs�Bt�Bq�Bo�Bl�BiyBhsBffBe`BbNB^5BZBT�BO�BK�BI�BH�BF�BE�BD�BC�BB�BG�BC�BC�BC�BD�BD�BD�BD�BE�BF�BI�BJ�BK�BL�BL�BL�BL�BK�BK�BK�BK�BK�BN�B`BBbNBdZBhsBn�Bt�By�B� B�1B�bB��B��B��B��B��B��B��B�{B�oB��B��B��B��B��B��B�!B�-B�LB�dB�jB�qB��BƨBȴB��B�B�B�B�B�B�B�B�)B�TB�yB�B�B�B�B�B��B��B��B��B��B	B	B	B	B	B	B	  B	  B	  B	  B	  B	B	B	B	B	%B	JB	\B	bB	bB	hB	�B	�B	�B	�B	�B	�B	"�B	#�B	#�B	#�B	%�B	,B	0!B	2-B	33B	49B	7LB	9XB	;dB	?}B	B�B	G�B	H�B	J�B	M�B	Q�B	S�B	S�B	S�B	S�B	W
B	ZB	\)B	]/B	]/B	^5B	^5B	_;B	_;B	_;B	bNB	cTB	gmB	iyB	k�B	l�B	m�B	n�B	n�B	n�B	n�B	o�B	o�B	n�B	o�B	s�B	x�B	�B	�B	�1B	�DB	�\B	�\B	�hB	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�3B	�FB	�RB	�RB	�XB	�qB	�wB	�wB	�}B	��B	��B	��B	��B	ÖB	ŢB	ŢB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�B	�B	�)B	�/B	�5B	�HB	�HB	�NB	�NB	�NB	�ZB	�fB
5B
,q2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  Bq�Bq�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bq�Bp�Bp�Bp�Bp�Bp�Bq�Bp�Bp�Bp�Bp�Bp�Bp�Bq�Bq�Bq�Bq�Bp�Bp�Bp�Bo�Bo�Bo�Bo�Bo�Bo�Bo�Bn�Bo�Bq�Bt�B�B�VB��B��B��BB1B�B!�B,B33B9XBC�BZB^5BcTBcTBbNBZBZBYBVBW
B]/B\)BXBT�BA�B6FB)�B1B�B�mB��B��B�\B;dB%B
��B
��B
��B
�B
M�B
6FB
<jB
9XB
,B
"�B
�B
�B
bB
B	��B	�B	�ZB	�5B	��B	ÖB	�XB	��B	��B	�uB	�VB	~�B	jB	`BB	[#B	S�B	Q�B	O�B	H�B	<jB	!�B	
=B��B�B�5B��BȴBÖB�RB��B��B��B��B��B��B��B��B��B��B�{B�uB�\B�DB�=B�1B�7B�=B�PB�PB�\B�\B�\B�VB�VB�VB�DB�1B�B� B~�B~�B}�B{�Bx�Bs�Bt�Bq�Bo�Bl�BiyBhsBffBe`BbNB^5BZBT�BO�BK�BI�BH�BF�BE�BD�BC�BB�BG�BC�BC�BC�BD�BD�BD�BD�BE�BF�BI�BJ�BK�BL�BL�BL�BL�BK�BK�BK�BK�BK�BN�B`BBbNBdZBhsBn�Bt�By�B� B�1B�bB��B��B��B��B��B��B��B�{B�oB��B��B��B��B��B��B�!B�-B�LB�dB�jB�qB��BƨBȴB��B�B�B�B�B�B�B�B�)B�TB�yB�B�B�B�B�B��B��B��B��B��B	B	B	B	B	B	B	  B	  B	  B	  B	  B	B	B	B	B	%B	JB	\B	bB	bB	hB	�B	�B	�B	�B	�B	�B	"�B	#�B	#�B	#�B	%�B	,B	0!B	2-B	33B	49B	7LB	9XB	;dB	?}B	B�B	G�B	H�B	J�B	M�B	Q�B	S�B	S�B	S�B	S�B	W
B	ZB	\)B	]/B	]/B	^5B	^5B	_;B	_;B	_;B	bNB	cTB	gmB	iyB	k�B	l�B	m�B	n�B	n�B	n�B	n�B	o�B	o�B	n�B	o�B	s�B	x�B	�B	�B	�1B	�DB	�\B	�\B	�hB	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�3B	�FB	�RB	�RB	�XB	�qB	�wB	�wB	�}B	��B	��B	��B	��B	ÖB	ŢB	ŢB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�B	�B	�)B	�/B	�5B	�HB	�HB	�NB	�NB	�NB	�ZB	�fB
5B
,q2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.01 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191647                              AO  ARCAADJP                                                                    20181005191647    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191647  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191647  QCF$                G�O�G�O�G�O�8000            