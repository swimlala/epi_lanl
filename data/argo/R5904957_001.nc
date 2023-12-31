CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  r   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:03Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 t  ?8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  @�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 t  Ft   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  G�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  M�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 t  Sx   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 t  Z�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  \(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  a�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 t  g�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i,   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 t  n�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ph   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  v0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    v`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    y`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    |`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  `   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181024140803  20181024140803  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @כ��y41   @כ�{Bt�@3���"���c�ȴ9X1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   A   @�33@�  A   AffA@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(ffB0  B8  B@  BH  BP  BXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C�C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR�CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C}�fC�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C�  C�  C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  D   D � D  D� D  D� D  D�fD  D� D  D� DfD� D  D� D  D� D	fD	� D	��D
y�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D   D � D ��D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D'��D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1�fD2fD2�fD3fD3�fD4  D4� D5  D5y�D6  D6� D7  D7� D8  Dy�fD�T{D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�Q�@��@��A��A>�\A^�\A~�\A�G�A�G�A�G�A�G�A�G�A�{A�G�A�G�B��B��B��B��B(
=B/��B7��B?��BG��BO��BX
=B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C�C�C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CR�CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}�\C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C�HC�HC�HC�HC�HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��C��{C��{C��{C��{C��C��C��{C��{C��{C��{C��C��{C��{C��{C��{C��{C��{C��{C�HC�HC�HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C�HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C�HC��{C��{C��C��{C��{C��C��{C��{C�HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C�HC��{C��{C��{D z=D �=Dz=D�=Dz=D�=D��D�=Dz=D�=Dz=D �Dz=D�=Dz=D�=Dz=D	 �D	z=D	��D
s�D
�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Ds�D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D �Dz=D�=D z=D ��D!z=D!�=D"z=D"�=D#z=D#�=D$z=D$�=D%z=D%�=D&z=D&�=D'z=D'��D(z=D(�=D)z=D)�=D*z=D*�=D+z=D+�=D,z=D,�=D-z=D-�=D.z=D.�=D/z=D/�=D0z=D0�=D1��D2 �D2��D3 �D3��D3�=D4z=D4�=D5s�D5�=D6z=D6�=D7z=D7�=Dy��D�Q�D��>1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A���A�ȴA�ȴA��yA��A��A��A��A��TA��;A��;A��HA��TA��TA��TA��`A��HA��HA��TA��mA��mA��TA��`A��/AʶFAʝ�A��HA�r�A� �A��Aȏ\A��A�`BA�A��A���Aƥ�AƃA�|�A�{AŃA�ƨA�1'A�t�A�bA�I�A�\)A���A���A�A�(�A�1'A�ȴA��DA��A�7LA�ȴA�^5A��wA�A�A�n�A��A���A���A�7LA���A�VA��A�ȴA���A�S�A�9XA��A��^A��A��A��RA�bA���A�-A��TA�bA��+A��9A�"�A���A��^A��HA�-A���A��\A�;dA��#A�x�A���A�$�A���A��A�&�A�$�A|Q�A{XAxjAux�ArffAq��Ap��An�RAj��Ad�9AbjA_A^Q�A[t�AXn�AW"�AU�AU|�AU/AT��AR�HAO�AMAL �AK��AKO�AJ9XAG�AD�9ABZA@�/A?&�A>^5A:��A8ZA6�HA6-A3�A1dZA/�mA-�A+\)A)�
A(�A'x�A&ZA%?}A"��A ��A�FA1'At�A��A1A�hA��AA�A�A��A�RA��A��A��A��A+A��AM�A�mA��AA�FA��AO�AĜA�PA�Az�AM�A �A�A��A
z�A	�A(�A�A�uA�wA�!AA�A\)A �A I�@�5?@���@�ƨ@�^5@�33@�ff@�ff@�ff@�5?@�dZ@��@���@��@蛦@�\)@�@�G�@��@�Ĝ@�ƨ@��@�R@�\@��T@�l�@�hs@܋D@ۍP@���@���@��;@�ff@��@��T@�7L@ԣ�@Ӯ@�ff@��@��T@�?}@��@Ѓ@�I�@��@�b@�1@�1@�1@���@ϥ�@�|�@�+@�@���@Η�@�^5@���@̛�@��@�dZ@�33@��@ɉ7@ȓu@ȋD@ȋD@�z�@�1'@��@ǅ@��@Ƨ�@��#@Ĵ9@�9X@�t�@�@���@�O�@��/@�A�@��
@���@�t�@�;d@���@�~�@�^5@��R@��y@��^@�z�@���@� �@�1@���@��;@��F@��@�J@�X@���@�1@��@���@�ff@�-@�{@���@�-@�ff@�n�@�~�@�~�@�~�@�~�@�~�@�n�@�V@�{@���@�?}@��/@���@��@�bN@�1'@�1@��@��@��@�"�@���@�M�@��@��T@��^@��h@���@��@�ƨ@���@��@�dZ@�
=@���@���@�hs@��`@���@��@�r�@�I�@�(�@�1@���@�|�@�@��+@��#@�x�@�X@�?}@�&�@�%@��@��P@��y@���@���@���@���@��+@���@s)_@ak�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A���A�ȴA�ȴA��yA��A��A��A��A��TA��;A��;A��HA��TA��TA��TA��`A��HA��HA��TA��mA��mA��TA��`A��/AʶFAʝ�A��HA�r�A� �A��Aȏ\A��A�`BA�A��A���Aƥ�AƃA�|�A�{AŃA�ƨA�1'A�t�A�bA�I�A�\)A���A���A�A�(�A�1'A�ȴA��DA��A�7LA�ȴA�^5A��wA�A�A�n�A��A���A���A�7LA���A�VA��A�ȴA���A�S�A�9XA��A��^A��A��A��RA�bA���A�-A��TA�bA��+A��9A�"�A���A��^A��HA�-A���A��\A�;dA��#A�x�A���A�$�A���A��A�&�A�$�A|Q�A{XAxjAux�ArffAq��Ap��An�RAj��Ad�9AbjA_A^Q�A[t�AXn�AW"�AU�AU|�AU/AT��AR�HAO�AMAL �AK��AKO�AJ9XAG�AD�9ABZA@�/A?&�A>^5A:��A8ZA6�HA6-A3�A1dZA/�mA-�A+\)A)�
A(�A'x�A&ZA%?}A"��A ��A�FA1'At�A��A1A�hA��AA�A�A��A�RA��A��A��A��A+A��AM�A�mA��AA�FA��AO�AĜA�PA�Az�AM�A �A�A��A
z�A	�A(�A�A�uA�wA�!AA�A\)A �A I�@�5?@���@�ƨ@�^5@�33@�ff@�ff@�ff@�5?@�dZ@��@���@��@蛦@�\)@�@�G�@��@�Ĝ@�ƨ@��@�R@�\@��T@�l�@�hs@܋D@ۍP@���@���@��;@�ff@��@��T@�7L@ԣ�@Ӯ@�ff@��@��T@�?}@��@Ѓ@�I�@��@�b@�1@�1@�1@���@ϥ�@�|�@�+@�@���@Η�@�^5@���@̛�@��@�dZ@�33@��@ɉ7@ȓu@ȋD@ȋD@�z�@�1'@��@ǅ@��@Ƨ�@��#@Ĵ9@�9X@�t�@�@���@�O�@��/@�A�@��
@���@�t�@�;d@���@�~�@�^5@��R@��y@��^@�z�@���@� �@�1@���@��;@��F@��@�J@�X@���@�1@��@���@�ff@�-@�{@���@�-@�ff@�n�@�~�@�~�@�~�@�~�@�~�@�n�@�V@�{@���@�?}@��/@���@��@�bN@�1'@�1@��@��@��@�"�@���@�M�@��@��T@��^@��h@���@��@�ƨ@���@��@�dZ@�
=@���@���@�hs@��`@���@��@�r�@�I�@�(�@�1@���@�|�@�@��+@��#@�x�@�X@�?}@�&�@�%@��@��P@��y@���@���@���@���@��+@���@s)_@ak�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
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
��B�B7LBF�BZBx�B�1B��B��B�BĜB�B�fBoB!�B9XBM�Bm�Bq�B�oB�RB�jBB��B��B�9BbNB5?B33B33B2-B33B5?B<jBl�BbNBT�BcTBn�Bn�BjB_;BYBS�BE�B<jBI�BJ�B9XB(�B#�B�BVBB��B�B�wB��BffBS�B#�B
��B
�`B
�BB
��B
�qB
�B
��B
�%B
y�B
u�B
e`B
R�B
33B
hB
%B	�B	�B	ĜB	�}B	�XB	�B	��B	t�B	k�B	bNB	YB	D�B	8RB	2-B	-B	,B	)�B	%�B	�B	JB	+B��B��B��B�B�fB�5B��B��BɺBÖB�dB�?B�3B�!B�B��B��B��B��B��B��B�oB�\B�JB�1B�%B�7B�7B�=B�=B�JB�bB��B��B�uB�hB�\B�+B�\B�oB�bB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�-B�?B�RB�^B�^B�^B�jB�qB�qB�qB�}BƨB��B��B��B��B��B��B��B��B��B��B��B�
B�BB�ZB�fB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B	  B	B	B	B	B	
=B	\B	bB	bB	bB	bB	hB	uB	�B	�B	�B	�B	 �B	 �B	�B	�B	�B	!�B	"�B	#�B	$�B	%�B	&�B	)�B	-B	0!B	6FB	9XB	9XB	8RB	=qB	@�B	B�B	C�B	C�B	D�B	F�B	N�B	T�B	W
B	ZB	\)B	`BB	cTB	cTB	dZB	ffB	l�B	o�B	o�B	p�B	p�B	q�B	q�B	r�B	t�B	u�B	w�B	x�B	}�B	�B	�B	�B	�B	�%B	�+B	�1B	�1B	�7B	�PB	�hB	�oB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�-B	�3B	�9B	�?B	�LB	�^B	�jB	�qB	�wB	�wB	�wB	�wB	�wB	�wB	��B	B	B	B	��B	B
�B
$�B
41111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
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
��B�B7LBF�BZBx�B�1B��B��B�BĜB�B�fBoB!�B9XBM�Bm�Bq�B�oB�RB�jBB��B��B�9BbNB5?B33B33B2-B33B5?B<jBl�BbNBT�BcTBn�Bn�BjB_;BYBS�BE�B<jBI�BJ�B9XB(�B#�B�BVBB��B�B�wB��BffBS�B#�B
��B
�`B
�BB
��B
�qB
�B
��B
�%B
y�B
u�B
e`B
R�B
33B
hB
%B	�B	�B	ĜB	�}B	�XB	�B	��B	t�B	k�B	bNB	YB	D�B	8RB	2-B	-B	,B	)�B	%�B	�B	JB	+B��B��B��B�B�fB�5B��B��BɺBÖB�dB�?B�3B�!B�B��B��B��B��B��B��B�oB�\B�JB�1B�%B�7B�7B�=B�=B�JB�bB��B��B�uB�hB�\B�+B�\B�oB�bB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�-B�?B�RB�^B�^B�^B�jB�qB�qB�qB�}BƨB��B��B��B��B��B��B��B��B��B��B��B�
B�BB�ZB�fB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B	  B	B	B	B	B	
=B	\B	bB	bB	bB	bB	hB	uB	�B	�B	�B	�B	 �B	 �B	�B	�B	�B	!�B	"�B	#�B	$�B	%�B	&�B	)�B	-B	0!B	6FB	9XB	9XB	8RB	=qB	@�B	B�B	C�B	C�B	D�B	F�B	N�B	T�B	W
B	ZB	\)B	`BB	cTB	cTB	dZB	ffB	l�B	o�B	o�B	p�B	p�B	q�B	q�B	r�B	t�B	u�B	w�B	x�B	}�B	�B	�B	�B	�B	�%B	�+B	�1B	�1B	�7B	�PB	�hB	�oB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�-B	�3B	�9B	�?B	�LB	�^B	�jB	�qB	�wB	�wB	�wB	�wB	�wB	�wB	��B	B	B	B	��B	B
�B
$�B
41111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.09 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140803                              AO  ARCAADJP                                                                    20181024140803    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140803  QCP$                G�O�G�O�G�O�F03E            AO  ARGQQCPL                                                                    20181024140803  QCF$                G�O�G�O�G�O�0               