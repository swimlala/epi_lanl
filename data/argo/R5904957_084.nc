CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  _   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:19Z creation      
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
resolution        =���   axis      Z        |  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 `  >�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     |  @L   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 `  E�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     |  G(   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  L�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 `  R    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  S�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 `  X�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  Z\   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  _�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 `  eT   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  f�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 `  l0   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  m�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  s   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    s<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    v<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    y<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  |<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    |h   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    |l   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    |p   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    |t   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  |x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    |�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    |�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    |�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         |�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         |�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        |�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    |�Argo profile    3.1 1.2 19500101000000  20181024140819  20181024140819  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               TA   AO  6560                            2B  A   APEX                            7471                            062512                          846 @�Ĥ�-�1   @�ĥ\�6�@3�C���c��S���1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      TA   A   A   @9��@�  @�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB0ffB7��B@  BH  BP  BX  B`ffBh  Bo��Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B���B�  B�  C   C  C  C  C�C
  C  C  C  C  C  C  C�C�C  C  C   C"  C$  C%�fC(  C*  C,  C.  C0  C2  C4�C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX�CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C��C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C��3C��3C�  C�  C��C��C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C��D fD � D  D� D  D� D��Dy�D  D� D  D� DfD� D  D�fD  D� D	  D	� D	��D
y�D
��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"y�D"��D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� DyD�@R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @N�R@��\@ʏ\A�A%G�AEG�AeG�A���A���A���A���A£�Aң�A��A��BQ�B	Q�BQ�BQ�B!Q�B)�RB1�RB8�BAQ�BIQ�BQQ�BYQ�Ba�RBiQ�Bp�ByQ�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�u�B���BĨ�BȨ�B̨�BШ�BԨ�Bب�Bܨ�B�u�B��B��B��B��B�u�B���B���C T{CT{CT{CT{CnC
T{CT{CT{CT{CT{CT{CT{CnCnCT{CT{C T{C"T{C$T{C&:�C(T{C*T{C,T{C.T{C0T{C2T{C4nC6T{C8T{C:T{C<T{C>T{C@T{CBT{CDT{CFT{CHT{CJT{CLT{CNT{CPT{CRT{CTT{CVT{CXnCZT{C\T{C^T{C`T{CbT{CdT{CfT{ChT{CjT{ClT{CnT{CpT{CrT{CtT{CvT{CxT{CzT{C|T{C~T{C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�7
C�*=C�pC�*=C�*=C�*=C�*=C�*=C�*=C�7
C�7
C�7
C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�7
C�*=C�*=C�*=C�7
C�7
C�*=C�pC�*=C�*=C�*=C�*=C�*=C�*=C�*=C�pC�*=C�*=C�*=C�*=C�pC�pC�pC�*=C�*=C�7
C�7
C�*=C�*=C�pC�*=C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�7
C�*=C�pC�*=C�7
D �D �DD�DD�D�D��DD�DD�D�D�DD��DD�D	D	�D
�D
��D�D�DD�DD�DD�DD�DD�DD�DD�DD�DD�D�D�DD�DD��DD�DD�DD�DD�DD�DD�DD�DD�D D �D!D!�D"D"��D#�D#�D$D$�D%D%�D&D&�D'D'�D(D(�D)D)�D*D*�D+D+�D,D,�D-D-�D.D.�Dy׮D�J�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aܛ�Aܝ�Aܟ�Aܛ�Aܗ�Aܙ�Aܗ�Aܕ�Aܥ�Aܥ�Aܩ�Aܧ�Aܥ�Aܝ�A܏\A�n�A�$�A��
Aۙ�Aڗ�A�M�A�p�A�7LA�1A�O�AЗ�AϮAΝ�A�?}A��A��DA�oA�p�A���A�
=A�l�A��A�
=A�VA�\)A��TA��+A�hsA���A�/A��A��A��A�1'A�n�A�t�A��RA�M�A�&�A�|�A��PA�"�A��A��
A��A��jA� �A��A���A�$�A���A�|�A���A�v�A�`BA�`BA���A�dZA��9A�ĜA�&�A��-A���AA~bNA|�!Aw�At�As�Aqx�Ao�
An�An�DAn-An(�An  Am��AmdZAl�!AjI�AhZAg&�Ad9XA]�A[S�AZ�AW�AQ33APbAO�^AK|�AG�AEAC�wAA%A?/A<��A;�
A:��A9��A9C�A8�jA7�A6��A5��A3x�A2bA1�7A133A/�
A.  A,�+A(�yA(A'S�A$��A"�/A!��A!|�A $�AM�A1A5?A;dAAĜA"�AE�Ax�A�uA�A&�AdZA�A�;A�7A�A��AA�A\)A
�+A
^5A
~�A
�A	�A	dZA�!AbAXA��A�#A��Ar�A9XAȴA{A�7AVA ff@��@�\)@��!@���@���@�%@�I�@��@�"�@���@���@�V@�Ĝ@�9X@��@�@�E�@�V@�C�@�@�+@�@�@旍@�r�@�!@��@�@�%@��u@�  @ܬ@۾w@�S�@��H@ڸR@ڗ�@��@ٺ^@�`B@ٙ�@ٺ^@�`B@؋D@�Q�@��m@�t�@ְ!@�{@Չ7@�r�@ӍP@��@�\)@�
=@͑h@�+@��#@��@ɑh@�p�@ɉ7@�V@��@��H@��@��@�t�@�o@�p�@�z�@�(�@�%@��h@�{@�&�@��@���@�(�@��@�5?@��@�S�@��@��\@���@�;d@�C�@��@���@�z�@��/@�r�@��`@���@�l�@���@�J@���@���@��#@��7@��P@�C�@��#@�&�@��@�bN@�(�@�1@��@�dZ@�
=@���@��R@���@�n�@�V@�E�@�-@�?}@�1'@��@���@��@��@��@��9@�9X@�bN@�Q�@�1@��;@��w@�ƨ@�ƨ@��@�+@�ff@��@���@�&�@�r�@�1'@�1@��;@��;@��w@��F@��P@�l�@�dZ@�K�@�\)@�C�@�+@��@�ȴ@�J@��T@���@�J@���@��u@�1@��@�(�@���@�V@��/@��@�Z@��F@�(�@�w�@u�>111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aܛ�Aܝ�Aܟ�Aܛ�Aܗ�Aܙ�Aܗ�Aܕ�Aܥ�Aܥ�Aܩ�Aܧ�Aܥ�Aܝ�A܏\A�n�A�$�A��
Aۙ�Aڗ�A�M�A�p�A�7LA�1A�O�AЗ�AϮAΝ�A�?}A��A��DA�oA�p�A���A�
=A�l�A��A�
=A�VA�\)A��TA��+A�hsA���A�/A��A��A��A�1'A�n�A�t�A��RA�M�A�&�A�|�A��PA�"�A��A��
A��A��jA� �A��A���A�$�A���A�|�A���A�v�A�`BA�`BA���A�dZA��9A�ĜA�&�A��-A���AA~bNA|�!Aw�At�As�Aqx�Ao�
An�An�DAn-An(�An  Am��AmdZAl�!AjI�AhZAg&�Ad9XA]�A[S�AZ�AW�AQ33APbAO�^AK|�AG�AEAC�wAA%A?/A<��A;�
A:��A9��A9C�A8�jA7�A6��A5��A3x�A2bA1�7A133A/�
A.  A,�+A(�yA(A'S�A$��A"�/A!��A!|�A $�AM�A1A5?A;dAAĜA"�AE�Ax�A�uA�A&�AdZA�A�;A�7A�A��AA�A\)A
�+A
^5A
~�A
�A	�A	dZA�!AbAXA��A�#A��Ar�A9XAȴA{A�7AVA ff@��@�\)@��!@���@���@�%@�I�@��@�"�@���@���@�V@�Ĝ@�9X@��@�@�E�@�V@�C�@�@�+@�@�@旍@�r�@�!@��@�@�%@��u@�  @ܬ@۾w@�S�@��H@ڸR@ڗ�@��@ٺ^@�`B@ٙ�@ٺ^@�`B@؋D@�Q�@��m@�t�@ְ!@�{@Չ7@�r�@ӍP@��@�\)@�
=@͑h@�+@��#@��@ɑh@�p�@ɉ7@�V@��@��H@��@��@�t�@�o@�p�@�z�@�(�@�%@��h@�{@�&�@��@���@�(�@��@�5?@��@�S�@��@��\@���@�;d@�C�@��@���@�z�@��/@�r�@��`@���@�l�@���@�J@���@���@��#@��7@��P@�C�@��#@�&�@��@�bN@�(�@�1@��@�dZ@�
=@���@��R@���@�n�@�V@�E�@�-@�?}@�1'@��@���@��@��@��@��9@�9X@�bN@�Q�@�1@��;@��w@�ƨ@�ƨ@��@�+@�ff@��@���@�&�@�r�@�1'@�1@��;@��;@��w@��F@��P@�l�@�dZ@�K�@�\)@�C�@�+@��@�ȴ@�J@��T@���@�J@���@��u@�1@��@�(�@���@�V@��/@��@�Z@��F@�(�@�w�@u�>111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bo�Bn�Bn�Bn�Bn�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bn�Bn�Bp�Bs�B{�B�+B�hB�fB!�B5?B5?B1'B/B>wBC�BK�BJ�BW
B_;BffBm�Br�Bs�B{�B�B�1B�Bv�Bk�B33B��B�B�B�TBB�LB�B��B��B�DB�%B�B~�Bw�Bs�Bq�Bn�BffBaHB^5B]/BXB=qBB
�;B
�
B
ĜB
��B
��B
��B
�DB
r�B
cTB
I�B
�B
PB
+B	��B	�B	ǮB	�!B	�-B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�+B	�B	�1B	k�B	.B	'�B	$�B	�B	1B	B��B�B�;B�B��B��BƨB��B�qB�dB�RB�LB�?B�?B�9B�-B�-B�!B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�\B�+B�B�B�B�+B�\B�\B�DB�7B�DB�VB�uB�bB�JB�B�B�1B�%B�DB�JB�7B�=B�JB�DB�7B�PB�B��B��B��B��B��B��B��B�{B�oB�JB�Bx�Bw�By�B~�B�DB��B��B��B��B��B��B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�?B�RB�wB��B��B��B��B��B��BBB��B��B��B��B�wB�qB�}BĜBǮB��B��B��B��BȴB��B��B��B�
B�B��B�#B�HB�`B��B	�B	�B	VB	PB	
=B	%B	1B	DB	VB	�B	�B	"�B	'�B	+B	49B	@�B	B�B	G�B	J�B	H�B	E�B	C�B	A�B	B�B	J�B	N�B	H�B	K�B	L�B	O�B	S�B	W
B	XB	YB	\)B	]/B	aHB	bNB	cTB	e`B	gmB	hsB	hsB	iyB	n�B	s�B	z�B	�B	�%B	�%B	�%B	�1B	�VB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�-B	�?B	�RB	�9B	�9B	�?B	�XB	�jB	�wB	�wB	�qB	�qB	�jB	�}B
#B
*�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Bo�Bn�Bn�Bn�Bn�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bn�Bn�Bp�Bs�B{�B�+B�hB�fB!�B5?B5?B1'B/B>wBC�BK�BJ�BW
B_;BffBm�Br�Bs�B{�B�B�1B�Bv�Bk�B33B��B�B�B�TBB�LB�B��B��B�DB�%B�B~�Bw�Bs�Bq�Bn�BffBaHB^5B]/BXB=qBB
�;B
�
B
ĜB
��B
��B
��B
�DB
r�B
cTB
I�B
�B
PB
+B	��B	�B	ǮB	�!B	�-B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�+B	�B	�1B	k�B	.B	'�B	$�B	�B	1B	B��B�B�;B�B��B��BƨB��B�qB�dB�RB�LB�?B�?B�9B�-B�-B�!B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�\B�+B�B�B�B�+B�\B�\B�DB�7B�DB�VB�uB�bB�JB�B�B�1B�%B�DB�JB�7B�=B�JB�DB�7B�PB�B��B��B��B��B��B��B��B�{B�oB�JB�Bx�Bw�By�B~�B�DB��B��B��B��B��B��B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�?B�RB�wB��B��B��B��B��B��BBB��B��B��B��B�wB�qB�}BĜBǮB��B��B��B��BȴB��B��B��B�
B�B��B�#B�HB�`B��B	�B	�B	VB	PB	
=B	%B	1B	DB	VB	�B	�B	"�B	'�B	+B	49B	@�B	B�B	G�B	J�B	H�B	E�B	C�B	A�B	B�B	J�B	N�B	H�B	K�B	L�B	O�B	S�B	W
B	XB	YB	\)B	]/B	aHB	bNB	cTB	e`B	gmB	hsB	hsB	iyB	n�B	s�B	z�B	�B	�%B	�%B	�%B	�1B	�VB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�-B	�?B	�RB	�9B	�9B	�?B	�XB	�jB	�wB	�wB	�qB	�qB	�jB	�}B
#B
*�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.33 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140819                              AO  ARCAADJP                                                                    20181024140819    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140819  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140819  QCF$                G�O�G�O�G�O�0               