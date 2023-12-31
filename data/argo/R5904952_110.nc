CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  K   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:29Z creation      
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
resolution        =���   axis      Z        ,  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 L  >�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     ,  ?�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 L  E   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     ,  F`   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     ,  K�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 L  P�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     ,  R   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 L  W0   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     ,  X|   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     ,  ]�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 L  b�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     ,  d    PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 L  iL   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     ,  j�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  o�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    o�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    r�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    u�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  x�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    y    HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    y$   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    y(   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    y,   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  y0   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    yp   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    y�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    y�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         y�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         y�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        y�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    y�Argo profile    3.1 1.2 19500101000000  20181005190529  20181005190529  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               nA   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��'��@�1   @��(W:�"@1���S���c������1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      nA   A   B   @�33@�  A   A   A@  Aa��A���A���A�  A�  A�33A�33A�33A�33B   B  B  BffB   B(  B/��B7��B@  BHffBP  BX  B_��Bh  BpffBx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B���C   C  C  C  C  C
�C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.�C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Ca�fCc�fCf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C��C��C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D �fD  D� D  D� D  D� D  Dy�D��D� D  D�fD  D� D��Dy�D	  D	y�D
  D
�fD  Dy�D  D� D  D� D  D� D��D� DfD� D  D� D��D� D� D  D� D   D � D!  D!� D"  D"� D#fD#�fD$fD$�fD%  D%� D&  D&� D'  D'� D'��D(y�D)  D5  D5� D6  D6� D7  D7� D7��D8y�D8��D9y�D:  D:� D;  D;� D;��D<y�D<��D=y�D=��D>y�D?  D?� D@  D@�fDA  DA� DB  DB� DB��DC� DDfDD�fDE  DE� DF  DF�fDGfDG� DH  DH� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��A��A$��AD��Af�]A�G�A�G�A�z�A�z�A��AѮA�A�B=qB	=qB=qB��B!=qB)=qB0�B8�BA=qBI��BQ=qBY=qB`�Bi=qBq��By=qB���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BĞ�B�k�B̞�BО�BԞ�B؞�Bܞ�B���B䞸B螸B잸B�k�B���B���B�k�C O\CO\CO\CO\CO\C
h�CO\CO\CO\CO\CO\CO\CO\CO\CO\CO\C O\C"O\C$O\C&O\C(O\C*O\C,O\C.h�C0O\C2O\C4O\C6O\C8O\C:O\C<O\C>O\C@O\CBO\CDO\CFO\CHO\CJO\CLO\CNO\CPO\CRO\CTO\CVO\CXO\CZO\C\O\C^O\C`O\Cb5�Cd5�CfO\ChO\CjO\ClO\CnO\CpO\CrO\CtO\CvO\CxO\CzO\C|O\C~O\C�'�C�'�C�'�C�'�C�4{C�4{C�'�C�4{C�4{C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C��C��C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�4{C�4{C�4{C�4{C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C��C��C�'�C�'�C�'�C�'�C��C�'�C�'�C�'�C�'�C�4{C�4{C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�4{C�4{C�'�C�'�C�4{C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�D �D �=D�D��D�D��D�D��D�D�qDqD��D�D�=D�D��DqD�qD	�D	�qD
�D
�=D�D�qD�D��D�D��D�D��DqD��D=D��D�D��DqD��D��D�D��D �D ��D!�D!��D"�D"��D#=D#�=D$=D$�=D%�D%��D&�D&��D'�D'��D(qD(�qD)�D5�D5��D6�D6��D7�D7��D8qD8�qD9qD9�qD:�D:��D;�D;��D<qD<�qD=qD=�qD>qD>�qD?�D?��D@�D@�=DA�DA��DB�DB��DCqDC��DD=DD�=DE�DE��DF�DF�=DG=DG��DH�DH��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�
=A�bA�{A��A��A�{A�bA��A��A��A��A��A��A��A�{A��A� �A�&�A�(�A�(�A�(�A�+A�/A�1'A�7LA�=qA�=qA�C�A�E�A�G�A�VA�t�AׁAץ�A�ƨA��#A���A�"�A�M�A؏\Aؙ�A�jA�C�A���A�A�A�t�A�ffA���A�;dA�ĜA�ZA�1A��/A�n�A��`A���AĴ9A���A²-A�%A��A�t�A�v�A���A��A�hsA��A�t�A��jA��wA��hA�C�A��wA�O�A��TA��HA���A���A�t�A�jA���A���A��^A�1A�?}A�oA�  A���A��/A��uA���A��FA�|�A��A��A��jA��jA�&�A�  A�ȴA�ZA�z�A�oA��A���A�5?A��hA��PA�v�A��mA�ffA���A�l�A��A|��Aw�As��Ar9XAp�9Al��Ah��Ae%A`9XA^ �A]`BA]C�A[�AY\)AT�AQx�AMdZAI"�AF�`AE�-AD�ACdZA@��A>�A<�A;�#A9�^A7+A6ĜA5��A4$�A3|�A37LA1�PA0�DA/A.��A-+A+?}A*VA)��A'�TA&��A%�A%�wA%K�A$��A"��A!%A {A7LAjA1A�^A�HAz�AQ�A��A�jA��A/AS�A�A�/A�+A��Ax�A�+A�^A(�A-AI�A�A��Ao@��@��h@���@��m@�p�@� �@���@�E�@�
=@�X@���@�l�@�V@�J@�n�@��@�=q@��@��@�X@�b@띲@��H@�@�Ĝ@�V@� �@ݲ-@��@��@�
=@ٺ^@�bN@ָR@�X@Ԭ@� �@��@��@�O�@Л�@�l�@�x�@̼j@̃@�1@�
=@�ff@�hs@��@Ȭ@ǥ�@��#@�G�@��`@� �@�v�@��7@���@���@��P@�M�@��@�7L@��m@��P@�+@�
=@�@�ȴ@�M�@�@��^@�?}@��/@��u@�z�@�j@�j@�I�@��j@��D@�r�@�Z@�Q�@�(�@�ƨ@�t�@�;d@��H@��\@�n�@�^5@�V@�5?@�@��7@�?}@�%@��@��u@�z�@��@���@���@�X@��@�Ĝ@�1'@���@��H@�M�@��@�@��@���@�O�@�V@��@��9@���@�z�@�1@��w@�l�@�;d@�
=@���@��+@�~�@�ff@�~�@��\@���@���@���@�V@�@���@���@��-@�&�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�
=A�bA�{A��A��A�{A�bA��A��A��A��A��A��A��A�{A��A� �A�&�A�(�A�(�A�(�A�+A�/A�1'A�7LA�=qA�=qA�C�A�E�A�G�A�VA�t�AׁAץ�A�ƨA��#A���A�"�A�M�A؏\Aؙ�A�jA�C�A���A�A�A�t�A�ffA���A�;dA�ĜA�ZA�1A��/A�n�A��`A���AĴ9A���A²-A�%A��A�t�A�v�A���A��A�hsA��A�t�A��jA��wA��hA�C�A��wA�O�A��TA��HA���A���A�t�A�jA���A���A��^A�1A�?}A�oA�  A���A��/A��uA���A��FA�|�A��A��A��jA��jA�&�A�  A�ȴA�ZA�z�A�oA��A���A�5?A��hA��PA�v�A��mA�ffA���A�l�A��A|��Aw�As��Ar9XAp�9Al��Ah��Ae%A`9XA^ �A]`BA]C�A[�AY\)AT�AQx�AMdZAI"�AF�`AE�-AD�ACdZA@��A>�A<�A;�#A9�^A7+A6ĜA5��A4$�A3|�A37LA1�PA0�DA/A.��A-+A+?}A*VA)��A'�TA&��A%�A%�wA%K�A$��A"��A!%A {A7LAjA1A�^A�HAz�AQ�A��A�jA��A/AS�A�A�/A�+A��Ax�A�+A�^A(�A-AI�A�A��Ao@��@��h@���@��m@�p�@� �@���@�E�@�
=@�X@���@�l�@�V@�J@�n�@��@�=q@��@��@�X@�b@띲@��H@�@�Ĝ@�V@� �@ݲ-@��@��@�
=@ٺ^@�bN@ָR@�X@Ԭ@� �@��@��@�O�@Л�@�l�@�x�@̼j@̃@�1@�
=@�ff@�hs@��@Ȭ@ǥ�@��#@�G�@��`@� �@�v�@��7@���@���@��P@�M�@��@�7L@��m@��P@�+@�
=@�@�ȴ@�M�@�@��^@�?}@��/@��u@�z�@�j@�j@�I�@��j@��D@�r�@�Z@�Q�@�(�@�ƨ@�t�@�;d@��H@��\@�n�@�^5@�V@�5?@�@��7@�?}@�%@��@��u@�z�@��@���@���@�X@��@�Ĝ@�1'@���@��H@�M�@��@�@��@���@�O�@�V@��@��9@���@�z�@�1@��w@�l�@�;d@�
=@���@��+@�~�@�ff@�~�@��\@���@���@���@�V@�@���@���@��-@�&�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
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
��B
��B
�B
�-B
ÖB
ɺB
�B
�mB
�B
��B  B
=B�B�B�B{BPB(�BC�BM�BI�B>wB$�BbBhB�B�B�B1'B;dB2-B:^BP�B[#BaHBt�B�B�7B��B��B�XB�}B��B�`B�B��B��BB!�B?}B?}B=qB8RB6FB2-B,B �B�B�B�B�BPB�B�9B�{Bt�BP�B5?BVB
��B
�B
�sB
�mB
��B
�B
��B
��B
�fB
�)B
�5B
�RB
s�B
6FB
#�B
5?B
#�B
DB	�B	��B	�-B	��B	�{B	z�B	_;B	C�B	$�B	�B	uB	bB	PB	VB��B�fB��BǮBĜBĜBB��B�qB�FB�?B�-B�B��B��B��B��B�-B�XB�^B�LB�RB�'B�?B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�VB�B�Bbo�B��B��B��B��BƨBƨB��B�`B�B�mB�ZB�BB��BɺB��B��BBÖB��B�B��B�B�B�B�)B�#B�#B�yB��B��B��B	B	B��B��B�B�BB�#B�B�B��B��B��B�B�#B�/B�5B�5B�BB�HB�TB�ZB�mB�B�B�B�B�B�B�B��B��B��B	  B	B	B	JB	hB	uB	�B	�B	!�B	"�B	$�B	+B	-B	0!B	2-B	2-B	49B	8RB	:^B	<jB	@�B	E�B	J�B	K�B	L�B	M�B4N�B	{�B	{�B	{�B	{�B	{�B	|�B	}�B	}�B	}�B	}�B	~�B	~�B	~�B	� B	�B	�B	�B	�B	�B	�B	�+B	|�B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�?B	�FB	�LB	�RB	�RB	�jB	��B	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�5B	�;B	�BB	�NB	�NB	�NB	�TB	�TB	�`2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222422222222222222222222222222222222222222222222222222222222222222222222222222222222222222222422222222222222222222222222222222222222222222222222222222222222 B
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
��B
��B
�B
�-B
ÖB
ɺB
�B
�mB
�B
��B  B
=B�B�B�B{BPB(�BC�BM�BI�B>wB$�BbBhB�B�B�B1'B;dB2-B:^BP�B[#BaHBt�B�B�7B��B��B�XB�}B��B�`B�B��B��BB!�B?}B?}B=qB8RB6FB2-B,B �B�B�B�B�BPB�B�9B�{Bt�BP�B5?BVB
��B
�B
�sB
�mB
��B
�B
��B
��B
�fB
�)B
�5B
�RB
s�B
6FB
#�B
5?B
#�B
DB	�B	��B	�-B	��B	�{B	z�B	_;B	C�B	$�B	�B	uB	bB	PB	VB��B�fB��BǮBĜBĜBB��B�qB�FB�?B�-B�B��B��B��B��B�-B�XB�^B�LB�RB�'B�?B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�VB�B�Bbo�B��B��B��B��BƨBƨB��B�`B�B�mB�ZB�BB��BɺB��B��BBÖB��B�B��B�B�B�B�)B�#B�#B�yB��B��B��B	B	B��B��B�B�BB�#B�B�B��B��B��B�B�#B�/B�5B�5B�BB�HB�TB�ZB�mB�B�B�B�B�B�B�B��B��B��B	  B	B	B	JB	hB	uB	�B	�B	!�B	"�B	$�B	+B	-B	0!B	2-B	2-B	49B	8RB	:^B	<jB	@�B	E�B	J�B	K�B	L�B	M�B4N�B	{�B	{�B	{�B	{�B	{�B	|�B	}�B	}�B	}�B	}�B	~�B	~�B	~�B	� B	�B	�B	�B	�B	�B	�B	�+B	|�B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�?B	�FB	�LB	�RB	�RB	�jB	��B	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�5B	�;B	�BB	�NB	�NB	�NB	�TB	�TB	�`2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222422222222222222222222222222222222222222222222222222222222222222222222222222222222222222222422222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.31 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190529                              AO  ARCAADJP                                                                    20181005190529    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190529  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190529  QCF$                G�O�G�O�G�O�18000           