CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:16Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  =(   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  >   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  A�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  B�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Fx   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  J0   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  K    TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  N�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  O�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  S�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  W8   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  X(   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  [�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  \�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  `�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    `�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    c�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    f�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  i�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    i�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    i�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    i�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    i�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  i�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    j4   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    jD   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    jH   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         jX   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         j\   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        j`   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    jdArgo profile    3.1 1.2 19500101000000  20181024140816  20181024140816  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               @A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @׿����J1   @׿�>���@2��z�H�c�&�x��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      @A   A   A   @333@�  @�  A   A   A@  A`  A�  A�  A�  A���A���A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  C   C  C�fC�fC  C
  C  C  C�C�C�C  C  C  C  C  C   C"  C$  C&  C(  C*  C,�C.  C0  C2  C4�C6�C8  C:  C<  C>�C@�CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Ci�fCk�fCm�fCo�fCr  Ct  Cv  Cx  Cy�fC|  C~�C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3Dy�3D�J=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @C33@�  @�  A  A$  AD  Ad  A�  A�  A�  A���A���A�  A�  A�  B  B	  B  B  B!  B)  B1  B9  BA  BI  BQ  BY  Ba  Bi  Bq  By  B��3B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B��3B�� B��3B�� B�� BĀ BȀ B̀ BЀ BԀ B؀ B܀ B�� B� B� B� B�3B� B�� B�� C @ C@ C&fC&fC@ C
@ C@ C@ CY�CY�CY�C@ C@ C@ C@ C@ C @ C"@ C$@ C&@ C(@ C*@ C,Y�C.@ C0@ C2@ C4Y�C6Y�C8@ C:@ C<@ C>Y�C@Y�CB@ CD@ CF@ CH@ CJ@ CL@ CN@ CP@ CR@ CT@ CV@ CX@ CZ@ C\@ C^@ C`@ Cb@ Cd@ Cf@ Ch@ Cj&fCl&fCn&fCp&fCr@ Ct@ Cv@ Cx@ Cz&fC|@ C~Y�C�  C�  C�  C�  C�  C�  C�  C�  C�3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�,�C�  C�3C�  C�,�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�3C�3C�  C�  C�  C�  C�  C�3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�,�C�,�C�,�C�,�C�  C�3C�  C�  C�  C�  C�  C�  C�  C�3Dy�3D�R=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�E�A�E�A�E�A�E�A�E�A�E�A�E�A�G�A�E�A�K�A�M�A�Q�A�S�A�Q�A�O�A�O�A�Q�A�Q�A�Q�A�A�A�ȴAُ\A�1Aذ!A�
=Aו�A�(�Aִ9A�l�A�C�A��A���A��
A�ƨA�|�A�M�A��Aԛ�A� �Aө�Aҝ�A�Q�A�ȴA�|�A���A�`BAΏ\AͮA�/A�r�A�-A�M�A��A��yA���Aȝ�A��A��A��;A�ƨA�v�A��Ağ�A�l�A¥�A���A�M�A��hA��PA�v�A�~�A�bA�G�A�M�A���A��DA�XA���A��A���A��PA���A��RA�/A���A�S�A���A�JA�hsA��A�ZA��A��7A�?}A�K�A��!A��A��A�O�A���A��^A�^5A�K�A��A�^5A��yA�=qA�?}A���A��/A�C�A�-A���A���A�7LA�  A��A~�yA{oAx�`Awt�Au��Ar�\Aq|�AqoAo�Ao?}Am��Ak`BAg�^Af1'Ad1AbA_�7A]G�AZ��AV��AT�DASVARffAQS�APbAN�+ALz�AK�AI7LAFZAD��AC�AA�AAp�A?�A=A<�/A<{A;/A9S�A7oA6�\A5?}A1\)A/�
A-��A*�HA(��A&��A$��A#hsA!�A�-A|�A�A%A�jA��A��A~�A�!A��A��Az�A-A��Ap�A�yA�
A�A�#A
=AffAJA�A�A�RA{A��A �A�wA33A
$�A	�A�A�mAAQ�A{AK�A^5A1'A �H@�ƨ@�E�@��h@���@�hs@�(�@���@� �@�+@�@��@�I�@@��@�9@��@�~�@�7L@�Q�@睲@�C�@�R@�{@噚@�j@�@��@vJ�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�E�A�E�A�E�A�E�A�E�A�E�A�E�A�G�A�E�A�K�A�M�A�Q�A�S�A�Q�A�O�A�O�A�Q�A�Q�A�Q�A�A�A�ȴAُ\A�1Aذ!A�
=Aו�A�(�Aִ9A�l�A�C�A��A���A��
A�ƨA�|�A�M�A��Aԛ�A� �Aө�Aҝ�A�Q�A�ȴA�|�A���A�`BAΏ\AͮA�/A�r�A�-A�M�A��A��yA���Aȝ�A��A��A��;A�ƨA�v�A��Ağ�A�l�A¥�A���A�M�A��hA��PA�v�A�~�A�bA�G�A�M�A���A��DA�XA���A��A���A��PA���A��RA�/A���A�S�A���A�JA�hsA��A�ZA��A��7A�?}A�K�A��!A��A��A�O�A���A��^A�^5A�K�A��A�^5A��yA�=qA�?}A���A��/A�C�A�-A���A���A�7LA�  A��A~�yA{oAx�`Awt�Au��Ar�\Aq|�AqoAo�Ao?}Am��Ak`BAg�^Af1'Ad1AbA_�7A]G�AZ��AV��AT�DASVARffAQS�APbAN�+ALz�AK�AI7LAFZAD��AC�AA�AAp�A?�A=A<�/A<{A;/A9S�A7oA6�\A5?}A1\)A/�
A-��A*�HA(��A&��A$��A#hsA!�A�-A|�A�A%A�jA��A��A~�A�!A��A��Az�A-A��Ap�A�yA�
A�A�#A
=AffAJA�A�A�RA{A��A �A�wA33A
$�A	�A�A�mAAQ�A{AK�A^5A1'A �H@�ƨ@�E�@��h@���@�hs@�(�@���@� �@�+@�@��@�I�@@��@�9@��@�~�@�7L@�Q�@睲@�C�@�R@�{@噚@�j@�@��@vJ�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
N�B
O�B
O�B
O�B
P�B
O�B
O�B
O�B
O�B
O�B
P�B
O�B
P�B
I�B
F�B
F�B
F�B
D�B
C�B
D�B
E�B
H�B
K�B
J�B
J�B
J�B
C�B
A�B
D�B
O�B
YB
hsB
iyB
k�B
ÖB
�B
ǮB
��B
��BoB�B[#B��B�dB�qB��B�TB	7B,BC�BP�BT�B]/BcTBjB�JB��B��B�B��B��B�jB��B��BŢB�RB��B�JBv�BK�B �B�B\B  B��B�B�B�mB�;BŢB�B��Bm�BhsBXBT�B`BB�+B�hB�JB~�Bv�BcTBO�BE�B6FB/B+B"�B�B
=B
��B
�`B
��B
�}B
��B
�=B
x�B
jB
W
B
=qB
.B
#�B
�B
B	��B	�B	�B	�ZB	�B	ɺB	�3B	��B	��B	�VB	�B	r�B	e`B	O�B	A�B	9XB	49B	-B	'�B	�B	uB	
=B	B��B�B�yB�ZB�HB�)B��B��B��B��BĜB�wB�jB�FB�B��B��B��B��B�\B�=B�%B�B{�B{�By�Bx�Bv�Br�Br�B�B�uB��B�B�B�B�3B�-B�3B�FB�FB�RB�LB�^B�dB�dB�qB�wB�}BĜBÖBB��B�jB�jB�qB�wB�qB�qB�jB�dB�^B�^B�XB�LB�FB�9B�?B�?B�9B�!B�B�!B�!B�B�B�B�'B�'B�3B�3B�9B�?B�FB�LB�RB�LB�FB�9B�3B
hB
$�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
N�B
O�B
O�B
O�B
P�B
O�B
O�B
O�B
O�B
O�B
P�B
O�B
P�B
I�B
F�B
F�B
F�B
D�B
C�B
D�B
E�B
H�B
K�B
J�B
J�B
J�B
C�B
A�B
D�B
O�B
YB
hsB
iyB
k�B
ÖB
�B
ǮB
��B
��BoB�B[#B��B�dB�qB��B�TB	7B,BC�BP�BT�B]/BcTBjB�JB��B��B�B��B��B�jB��B��BŢB�RB��B�JBv�BK�B �B�B\B  B��B�B�B�mB�;BŢB�B��Bm�BhsBXBT�B`BB�+B�hB�JB~�Bv�BcTBO�BE�B6FB/B+B"�B�B
=B
��B
�`B
��B
�}B
��B
�=B
x�B
jB
W
B
=qB
.B
#�B
�B
B	��B	�B	�B	�ZB	�B	ɺB	�3B	��B	��B	�VB	�B	r�B	e`B	O�B	A�B	9XB	49B	-B	'�B	�B	uB	
=B	B��B�B�yB�ZB�HB�)B��B��B��B��BĜB�wB�jB�FB�B��B��B��B��B�\B�=B�%B�B{�B{�By�Bx�Bv�Br�Br�B�B�uB��B�B�B�B�3B�-B�3B�FB�FB�RB�LB�^B�dB�dB�qB�wB�}BĜBÖBB��B�jB�jB�qB�wB�qB�qB�jB�dB�^B�^B�XB�LB�FB�9B�?B�?B�9B�!B�B�!B�!B�B�B�B�'B�'B�3B�3B�9B�?B�FB�LB�RB�LB�FB�9B�3B
hB
$�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.25 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140816                              AO  ARCAADJP                                                                    20181024140816    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140816  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140816  QCF$                G�O�G�O�G�O�0               