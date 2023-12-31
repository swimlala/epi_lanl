CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  E   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:15:34Z creation      
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
resolution        =���   axis      Z          9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H  >�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       ?�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H  D�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       F(   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       K<   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H  PP   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       Q�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H  V�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       W�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       ]   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H  b   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       cd   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H  hx   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       i�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  n�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    o   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    r   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    u   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    x0   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    x4   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    x8   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    x<   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  x@   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    x�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    x�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    x�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         x�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         x�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        x�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    x�Argo profile    3.1 1.2 19500101000000  20181024141534  20181024141534  5904970 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6785                            2B  A   APEX                            7726                            111215                          846 @�र캉1   @��8㠖@82n��O��d"��vȴ1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�  @�  A   A   A@  A`  A���A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C�fC  C  C  C  C  C   C"  C$  C&  C(  C*  C+�fC.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	y�D
  D
� D  D� D  D� D  D�fD  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� Dy�HD�O�D��=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��\@\AG�A!G�AAG�AaG�A�p�A���A���A���A���AУ�A��A��B Q�BQ�BQ�BQ�B Q�B(Q�B0Q�B8Q�B@Q�BHQ�BPQ�BXQ�B`Q�BhQ�BpQ�BxQ�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�\)B�\)B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�\)B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�C {C{C{C{C{C
{C{C{C{C{C��C{C{C{C{C{C {C"{C${C&{C({C*{C+��C.{C0{C2{C4{C6{C8{C:{C<{C>{C@{CB{CD{CF{CH{CJ{CL{CN{CP{CR{CT{CV{CX{CZ{C\{C^{C`{Cb{Cd{Cf{Ch{Cj{Cl{Cn{Cp{Cr{Ct{Cv{Cx{Cz{C|{C~{C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
C�
C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C��pC�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=D D �DD�DD�DD�DD�DD�DD�DD�DD�D	D	~�D
D
�DD�DD�DD��DD�DD�DD~�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�D D �D!D!�Dy�gD�R=D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aɩ�AɬAɬAɧ�Aə�A�l�A�I�A��A��A�r�A�XA�(�A�A��A��#A�ƨAǲ-Aǧ�Aǩ�AǛ�AǗ�AǑhAǅA�|�A�x�A�p�A�hsA�hsA�ffA�ZA�VA�M�A�M�A�C�A�A�A�A�A�A�A�7LA�&�A�&�A�"�A�oA���AƗ�A�&�Aţ�A�$�Ać+A�/A���A�oA�dZA��A�/A�JA�7LA��A���A�"�A�bNA��^A�l�A�p�A��
A�z�A��uA�?}A��TA��A��-A��TA��A��
A�+A�1'A�ƨA���A��9A��7A�9XA�ƨA�5?A�t�A��A�"�A�n�A�{A��!A��A�p�A��A��A�dZA�ZA�&�A�jA���A�M�A�VA��!A�A�jA��;A��A��A�^5A��A�jA�&�A���A�ƨA�(�A��!A�x�A�~�A��HA���A��^A�;dA��A��A���A�VA�VA�K�A���A�oA��\A��A�~�A�?}A��#A���A��/A�K�A�&�A}��Az9XAw"�Av��Av��Av^5Au�Ar�!Aq�Ao��Ao;dAk�Ai�TAix�Ah�Ag/AfA�Ae�TAd�HAcXAb-AaC�A^��A]��A]�7A\��AYl�AX-AWl�AV �AT^5ARr�AP��AN�ALE�AJ�/AJ�RAIC�AF�!AFjAE�AC�mAA33A=�hA<r�A<9XA<(�A;�^A9�-A8^5A4-A2�9A2Q�A1�-A1�A0��A0��A01'A/K�A-�TA,�\A,9XA+��A+�A*�/A)�A(-A&�9A%�
A%t�A$�/A$�A$bA#+A"��A!�7A $�A�AVA�A7LAȴA��A�A"�Az�A��A%At�A�jA-A7LAn�A��A7LAbNA�;A�PA~�AhsAr�A��A%A
=qA	�A	ƨA	�FA	��A	O�A�mA�-AQ�AbNAVA $�@�C�@�@�z�@��@��@��@�K�@�=q@�o@�P@�j@�ƨ@�;d@�^5@���@��/@�t�@�@�(�@�@◍@��@��/@��m@�l�@�ff@�X@�r�@�{@ؓu@ְ!@�ff@�5?@�hs@�b@��@҇+@�{@щ7@�z�@̓u@���@�|�@��@���@ƸR@�^5@�x�@�G�@őh@�^5@��@�dZ@�  @�I�@�j@�z�@�r�@�b@��m@ǥ�@�\)@�
=@�V@��T@�V@���@���@ģ�@�bN@�1@��@v��@a�S1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aɩ�AɬAɬAɧ�Aə�A�l�A�I�A��A��A�r�A�XA�(�A�A��A��#A�ƨAǲ-Aǧ�Aǩ�AǛ�AǗ�AǑhAǅA�|�A�x�A�p�A�hsA�hsA�ffA�ZA�VA�M�A�M�A�C�A�A�A�A�A�A�A�7LA�&�A�&�A�"�A�oA���AƗ�A�&�Aţ�A�$�Ać+A�/A���A�oA�dZA��A�/A�JA�7LA��A���A�"�A�bNA��^A�l�A�p�A��
A�z�A��uA�?}A��TA��A��-A��TA��A��
A�+A�1'A�ƨA���A��9A��7A�9XA�ƨA�5?A�t�A��A�"�A�n�A�{A��!A��A�p�A��A��A�dZA�ZA�&�A�jA���A�M�A�VA��!A�A�jA��;A��A��A�^5A��A�jA�&�A���A�ƨA�(�A��!A�x�A�~�A��HA���A��^A�;dA��A��A���A�VA�VA�K�A���A�oA��\A��A�~�A�?}A��#A���A��/A�K�A�&�A}��Az9XAw"�Av��Av��Av^5Au�Ar�!Aq�Ao��Ao;dAk�Ai�TAix�Ah�Ag/AfA�Ae�TAd�HAcXAb-AaC�A^��A]��A]�7A\��AYl�AX-AWl�AV �AT^5ARr�AP��AN�ALE�AJ�/AJ�RAIC�AF�!AFjAE�AC�mAA33A=�hA<r�A<9XA<(�A;�^A9�-A8^5A4-A2�9A2Q�A1�-A1�A0��A0��A01'A/K�A-�TA,�\A,9XA+��A+�A*�/A)�A(-A&�9A%�
A%t�A$�/A$�A$bA#+A"��A!�7A $�A�AVA�A7LAȴA��A�A"�Az�A��A%At�A�jA-A7LAn�A��A7LAbNA�;A�PA~�AhsAr�A��A%A
=qA	�A	ƨA	�FA	��A	O�A�mA�-AQ�AbNAVA $�@�C�@�@�z�@��@��@��@�K�@�=q@�o@�P@�j@�ƨ@�;d@�^5@���@��/@�t�@�@�(�@�@◍@��@��/@��m@�l�@�ff@�X@�r�@�{@ؓu@ְ!@�ff@�5?@�hs@�b@��@҇+@�{@щ7@�z�@̓u@���@�|�@��@���@ƸR@�^5@�x�@�G�@őh@�^5@��@�dZ@�  @�I�@�j@�z�@�r�@�b@��m@ǥ�@�\)@�
=@�V@��T@�V@���@���@ģ�@�bN@�1@��@v��@a�S1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B"�B$�B&�B+B7LB:^BA�BG�BJ�BK�BL�BM�BM�BP�BR�BS�BW
BZB\)B^5B_;B`BB`BBaHBcTBdZBe`BffBhsBhsBiyBiyBiyBhsBhsBgmBffBe`BffBgmBjBl�Bo�Bo�Bo�Bp�Bs�B� B}�B�B�JB�JB�\B�uB��B��B��B��B�B��B��B�uB�bB�B�%Bz�B�+B��B�{B��B�FB�qBȴB�B�TB�/B�B��B��B�B��B��B�VB�Bx�Bt�Bp�BiyB[#BD�B9XB/B(�B#�B�B{BDBB��B�B��B��BȴBÖB�dB�B��B��B��B��B~�Bm�Bs�Bv�BhsB\)BI�B33B!�B
��B
�sB
�yB
�5B
�B
��B
ƨB
�}B
�dB
�'B
��B
�1B
p�B
\)B
@�B
9XB
7LB
33B
.B
�B
hB
  B	��B	�B	�5B	�/B	�)B	��B	��B	ǮB	B	�LB	�B	��B	��B	�PB	�7B	�%B	m�B	aHB	]/B	W
B	H�B	:^B	.B	!�B	�B	\B	JB		7B��B��B�B�/B��B�^B�!B�B�B��B��B��B��B�JB�=B�7B�1B�+B�+B�%B�B�1B�+B�%B�%B�B�B�7B�+B�B�B� B� B~�B}�B|�By�Bx�Bv�Bt�Bs�Br�Bq�Bp�Bo�Bn�Bk�BiyBiyBdZBcTBaHBffBbNB^5B]/B]/B[#BW
BW
BT�BP�BN�BK�BI�BG�BF�BF�BE�BE�BC�B?}B5?B/B,B'�B&�B%�B$�B#�B#�B"�B!�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B"�B"�B#�B%�B%�B&�B'�B&�B&�B&�B(�B,B6FB8RB:^B<jB?}BD�BL�BW
BaHBhsBm�Br�Bv�Bw�Bx�B{�B}�B~�B�B�bB�{B��B��B��B��B��B��B��B��B	��B	�yB
{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�B�B�B"�B$�B&�B+B7LB:^BA�BG�BJ�BK�BL�BM�BM�BP�BR�BS�BW
BZB\)B^5B_;B`BB`BBaHBcTBdZBe`BffBhsBhsBiyBiyBiyBhsBhsBgmBffBe`BffBgmBjBl�Bo�Bo�Bo�Bp�Bs�B� B}�B�B�JB�JB�\B�uB��B��B��B��B�B��B��B�uB�bB�B�%Bz�B�+B��B�{B��B�FB�qBȴB�B�TB�/B�B��B��B�B��B��B�VB�Bx�Bt�Bp�BiyB[#BD�B9XB/B(�B#�B�B{BDBB��B�B��B��BȴBÖB�dB�B��B��B��B��B~�Bm�Bs�Bv�BhsB\)BI�B33B!�B
��B
�sB
�yB
�5B
�B
��B
ƨB
�}B
�dB
�'B
��B
�1B
p�B
\)B
@�B
9XB
7LB
33B
.B
�B
hB
  B	��B	�B	�5B	�/B	�)B	��B	��B	ǮB	B	�LB	�B	��B	��B	�PB	�7B	�%B	m�B	aHB	]/B	W
B	H�B	:^B	.B	!�B	�B	\B	JB		7B��B��B�B�/B��B�^B�!B�B�B��B��B��B��B�JB�=B�7B�1B�+B�+B�%B�B�1B�+B�%B�%B�B�B�7B�+B�B�B� B� B~�B}�B|�By�Bx�Bv�Bt�Bs�Br�Bq�Bp�Bo�Bn�Bk�BiyBiyBdZBcTBaHBffBbNB^5B]/B]/B[#BW
BW
BT�BP�BN�BK�BI�BG�BF�BF�BE�BE�BC�B?}B5?B/B,B'�B&�B%�B$�B#�B#�B"�B!�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B"�B"�B#�B%�B%�B&�B'�B&�B&�B&�B(�B,B6FB8RB:^B<jB?}BD�BL�BW
BaHBhsBm�Br�Bv�Bw�Bx�B{�B}�B~�B�B�bB�{B��B��B��B��B��B��B��B��B	��B	�yB
{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.08 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024141534                              AO  ARCAADJP                                                                    20181024141534    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024141534  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024141534  QCF$                G�O�G�O�G�O�0               