CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:27Z creation      
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
resolution        =���   axis      Z        ,  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ?�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     ,  A(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  GT   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     ,  H�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     ,  O   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  U8   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     ,  V�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     ,  ^|   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     ,  d�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  j�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     ,  l`   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  r�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     ,  t   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  zD   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    zt   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    }t   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �t   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �t   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  20181005191727  20181005191727  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��$�	1   @��%WM@5��+�d}����1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�33A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�33B�  C   C  C  C�fC  C
  C  C  C  C  C  C�C�C  C  C  C   C"�C$  C&  C'�fC*  C,  C.  C0  C2  C4  C6  C8�C:�C<  C=�fC@  CB  CD  CF  CH�CJ  CK�fCN  CP  CR  CT  CV  CX  CZ�C\�C^  C`  Cb  Cd  Ce�fCh  Cj  Cl  Cn  Cp�Cr  Ct  Cv�Cx�Cy�fC|  C~  C�  C��C��C�  C�  C��3C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C��3C�  C��C��C�  C�  C��C��C��C��C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C��3C��C�  C�  C��3C��3C��3C��C�  C��3C�  C��C�  C�  C��C�  C��3C��3C�  C��C��C��C��C�  C�  C�  C��3C�  C��C�  C��3C��3C�  C��C�  C��3C�  C�  C�  C�  C��C��C��C�  C��3C�  C��C�  C�  C��C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C�  C��C��C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C��3C��3C�  C��C�  C�  C��C�  C�  C�  C��3C�  D   D y�D  D�fD  D�fDfD� D  D� D  D� D  D� D  D�fD  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� DfD�fD  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D'��D(� D)  D)� D*  D*� D+  D+�fD,  D,� D-  D-� D.  D.� D/  D/� D0  D0y�D0��D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:y�D:��D;y�D<  D<� D=  D=� D>  D>�fD?fD?�fD@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  Dy��D�/
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�(�@���Az�A$z�ADz�Adz�A�=qA�=qA�=qA�=qA�=qA�p�A�=qA�=qB�B	�B�B�B!�B)�B1�B9�BA�BI�BQ�BY�Ba�Bi�Bq�By�B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B�B��\B�\)B�\)B̏\BЏ\Bԏ\B؏\B܏\B��\B�\B�\B�\B�B�B�B��\C G�CG�CG�C.CG�C
G�CG�CG�CG�CG�CG�CaHCaHCG�CG�CG�C G�C"aHC$G�C&G�C(.C*G�C,G�C.G�C0G�C2G�C4G�C6G�C8aHC:aHC<G�C>.C@G�CBG�CDG�CFG�CHaHCJG�CL.CNG�CPG�CRG�CTG�CVG�CXG�CZaHC\aHC^G�C`G�CbG�CdG�Cf.ChG�CjG�ClG�CnG�CpaHCrG�CtG�CvaHCxaHCz.C|G�C~G�C�#�C�0�C�=qC�#�C�#�C�
C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�#�C�
C�#�C�0�C�0�C�#�C�#�C�0�C�0�C�0�C�0�C�#�C�#�C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�#�C�0�C�#�C�
C�0�C�#�C�#�C�
C�
C�
C�0�C�#�C�
C�#�C�0�C�#�C�#�C�0�C�#�C�
C�
C�#�C�0�C�0�C�0�C�0�C�#�C�#�C�#�C�
C�#�C�0�C�#�C�
C�
C�#�C�0�C�#�C�
C�#�C�#�C�#�C�#�C�0�C�0�C�0�C�#�C�
C�#�C�0�C�#�C�#�C�0�C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�#�C�#�C�#�C�#�C�
C�#�C�#�C�0�C�0�C�#�C�#�C�#�C�#�C�
C�
C�#�C�#�C�#�C�#�C�
C�
C�#�C�0�C�#�C�#�C�0�C�#�C�#�C�#�C�
C�#�D �D ��D�D�RD�D�RDRD��D�D��D�D��D�D��D�D�RD�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�RD�D��D�D��D�D��D�D��D�D��DRD�RD�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+�RD,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>�RD?RD?�RD@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�Dy�{D�8 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���AؾwA�ĜA���A���A���A���A���A���A��
A��A��A��#A��#A��#A��/A��/A��/A��;A��;A��HA��HA��HA��HA��HA��HA��HA��TA��TA��`A��`A��`A��HA��yA���Aأ�A�C�Aכ�A�5?A��/A�t�A���A�~�A�?}A�  AͶFA���A�t�A�oAƬA�5?AŅA�1'A���A�33A�{A��A��A���A�^5A��TA��9A�A�A���A�9XA�?}A��A�bA��A�ĜA���A�r�A��RA��wA���A��A��A�ƨA���A�E�A��9A�p�A�E�A��HA��A�C�A�%A��hA�bA���A�  A��A�bA�r�A��A�dZA�p�A���A���A�
=A�bA��A�33A��+A��A�"�A�ZA��HA���A�
=A��A�ĜA��mA�VA�/A|�Az��Az$�Aw��At�yAt  Ar�AqƨAo��An(�Am�Al��Al-Ak��AkoAj5?Aip�Ah�Af$�Aa��A`n�A_%A]dZAY�AW|�AWAV(�AT�HAS�FAR�DAQ�AP��AO��AMhsAI��AHI�AGXAFQ�ADv�AA�TA@��A?�TA?7LA=�A;��A8z�A7?}A65?A5�
A5l�A5&�A4��A3��A1dZA01A.I�A-K�A,^5A+hsA*bA)`BA'�^A&r�A%7LA$^5A#�FA!�A!��A �!A�hA�/A��AA�Al�A&�AVA
=AA�/A�uA7LAt�A%Ax�A�AVA��A�wA�TAt�A�A
5?A�9A;dA�+A7LAdZA�`A��A��A�A�RAQ�A��A �/A V@���@��@�Ĝ@�+@�n�@��#@��@���@��T@�33@�ff@�S�@���@�$�@�G�@��@���@�
=@�  @��@�9X@�@�7L@߅@ݡ�@�z�@�9X@���@ٙ�@��m@���@պ^@���@�^5@ѡ�@�Ĝ@�1'@�1@�+@�@̛�@�Z@�b@ˮ@�K�@�"�@ʇ+@�@��@Ǯ@���@��T@�?}@�x�@��@��;@°!@�V@���@�G�@���@�z�@�Q�@� �@�A�@�Z@�S�@�v�@���@��@���@�r�@�9X@�(�@��@�  @�K�@�5?@�r�@�ff@�r�@��F@��@��R@�v�@���@�p�@�Ĝ@�|�@�ȴ@�O�@��@�9X@�r�@�%@�x�@�Z@��+@�ff@�E�@��\@��@���@�V@�bN@�A�@��@��;@���@�9X@�j@�I�@�b@��@��@��;@�ƨ@���@���@�`B@���@�&�@�1'@��@�{@�$�@���@��;@���@��@��@��@�=q@�J@�J@�ff@�G�@��@��@�9X@��@�dZ@�K�@�C�@�33@��@��@���@��\@�@��^@�/@���@��9@���@�z�@��@���@�ƨ@���@�S�@�C�@�+@�"�@�o@���@��@���@���@�x�@�/@���@��M@|"h11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���AؾwA�ĜA���A���A���A���A���A���A��
A��A��A��#A��#A��#A��/A��/A��/A��;A��;A��HA��HA��HA��HA��HA��HA��HA��TA��TA��`A��`A��`A��HA��yA���Aأ�A�C�Aכ�A�5?A��/A�t�A���A�~�A�?}A�  AͶFA���A�t�A�oAƬA�5?AŅA�1'A���A�33A�{A��A��A���A�^5A��TA��9A�A�A���A�9XA�?}A��A�bA��A�ĜA���A�r�A��RA��wA���A��A��A�ƨA���A�E�A��9A�p�A�E�A��HA��A�C�A�%A��hA�bA���A�  A��A�bA�r�A��A�dZA�p�A���A���A�
=A�bA��A�33A��+A��A�"�A�ZA��HA���A�
=A��A�ĜA��mA�VA�/A|�Az��Az$�Aw��At�yAt  Ar�AqƨAo��An(�Am�Al��Al-Ak��AkoAj5?Aip�Ah�Af$�Aa��A`n�A_%A]dZAY�AW|�AWAV(�AT�HAS�FAR�DAQ�AP��AO��AMhsAI��AHI�AGXAFQ�ADv�AA�TA@��A?�TA?7LA=�A;��A8z�A7?}A65?A5�
A5l�A5&�A4��A3��A1dZA01A.I�A-K�A,^5A+hsA*bA)`BA'�^A&r�A%7LA$^5A#�FA!�A!��A �!A�hA�/A��AA�Al�A&�AVA
=AA�/A�uA7LAt�A%Ax�A�AVA��A�wA�TAt�A�A
5?A�9A;dA�+A7LAdZA�`A��A��A�A�RAQ�A��A �/A V@���@��@�Ĝ@�+@�n�@��#@��@���@��T@�33@�ff@�S�@���@�$�@�G�@��@���@�
=@�  @��@�9X@�@�7L@߅@ݡ�@�z�@�9X@���@ٙ�@��m@���@պ^@���@�^5@ѡ�@�Ĝ@�1'@�1@�+@�@̛�@�Z@�b@ˮ@�K�@�"�@ʇ+@�@��@Ǯ@���@��T@�?}@�x�@��@��;@°!@�V@���@�G�@���@�z�@�Q�@� �@�A�@�Z@�S�@�v�@���@��@���@�r�@�9X@�(�@��@�  @�K�@�5?@�r�@�ff@�r�@��F@��@��R@�v�@���@�p�@�Ĝ@�|�@�ȴ@�O�@��@�9X@�r�@�%@�x�@�Z@��+@�ff@�E�@��\@��@���@�V@�bN@�A�@��@��;@���@�9X@�j@�I�@�b@��@��@��;@�ƨ@���@���@�`B@���@�&�@�1'@��@�{@�$�@���@��;@���@��@��@��@�=q@�J@�J@�ff@�G�@��@��@�9X@��@�dZ@�K�@�C�@�33@��@��@���@��\@�@��^@�/@���@��9@���@�z�@��@���@�ƨ@���@�S�@�C�@�+@�"�@�o@���@��@���@���@�x�@�/@���@��M@|"h11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B1B1B1B1B1B1B1B1B1B1B1B1B1B1B1B1B1B	7B	7B	7B	7B	7B	7B
=BDBJBDBJBVBPBoB�BB�B�3B�BiyBjBp�Bk�Bn�Bo�Br�Bx�B}�B�B�+B�B�1B��B��B�B�3B�jBBȴB��B�B�B�B�B��B��B�/B�5B�;B�/B�;B�;B�BB�BB�HB�BB�;B�)B�B��B��BƨB�XB��B��B�{B�\B�BjBN�B?}B:^B�B��B  B��B�B�B��B�qB�B��B��B�=B}�BiyBYBO�BH�B;dB/B&�BJB
�B
�NB
��B
��B
��B
�bB
v�B
`BB
YB
E�B
1'B
+B
%�B
�B
oB
DB
+B
B	��B	��B	��B	��B	�B	�sB	�B	�^B	�B	��B	�bB	p�B	_;B	\)B	VB	O�B	H�B	B�B	<jB	8RB	33B	(�B	�B	�B	uB	VB	1B	B��B��B��B�B�fB�/B�/B�B�B�B��B��B��B��BÖB�wB�dB�LB�-B�B��B��B��B��B��B��B��B�uB�bB�JB�=B�+B�B�B�B�B�B� B~�B|�B{�Bw�Bu�Bs�Bp�Bn�Bk�BffBbNB\)BW
BR�BM�BJ�BH�BF�BC�BD�BL�BR�BS�BVB^5BbNB^5B[#BVBR�BO�BL�BK�BL�Bk�Bz�Bt�Bo�Bo�Bz�B�B� B�B}�B|�Bv�Bo�BgmBdZB`BBXBS�BS�BYB[#BZBXBT�BVBT�BT�BXBZB[#B\)B[#B[#B]/BaHBaHBaHBaHB`BB`BBaHBbNBdZBgmBiyBjBr�Bw�Bx�Bx�Bv�Bw�Bv�Bu�Bw�Bz�B{�B|�B�B�B� B~�B�B�B�B�B�B�B�B�B�1B�+B�B}�B|�B}�B|�B|�B|�B}�B~�B� B�B�+B�JB�bB�hB��B��B��B��B��B��B��B��B��B�B�9B�FB�RB�XB�qB�}BƨB��B�
B�B�B�#B�/B�BB�sB�B��B	  B	B	B��B	B	+B	DB	bB	�B	�B	�B	$�B	(�B	)�B	+B	0!B	2-B	49B	7LB	9XB	;dB	A�B	C�B	D�B	E�B	G�B	I�B	N�B	S�B	VB	ZB	^5B	`BB	aHB	aHB	bNB	gmB	hsB	hsB	iyB	k�B	k�B	l�B	l�B	l�B	l�B	l�B	m�B	n�B	p�B	q�B	r�B	�5B
�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B1B1B1B1B1B1B1B1B1B1B1B1B1B1B1B1B1B	7B	7B	7B	7B	7B	7B
=BDBJBDBJBVBPBoB�BB�B�3B�BiyBjBp�Bk�Bn�Bo�Br�Bx�B}�B�B�+B�B�1B��B��B�B�3B�jBBȴB��B�B�B�B�B��B��B�/B�5B�;B�/B�;B�;B�BB�BB�HB�BB�;B�)B�B��B��BƨB�XB��B��B�{B�\B�BjBN�B?}B:^B�B��B  B��B�B�B��B�qB�B��B��B�=B}�BiyBYBO�BH�B;dB/B&�BJB
�B
�NB
��B
��B
��B
�bB
v�B
`BB
YB
E�B
1'B
+B
%�B
�B
oB
DB
+B
B	��B	��B	��B	��B	�B	�sB	�B	�^B	�B	��B	�bB	p�B	_;B	\)B	VB	O�B	H�B	B�B	<jB	8RB	33B	(�B	�B	�B	uB	VB	1B	B��B��B��B�B�fB�/B�/B�B�B�B��B��B��B��BÖB�wB�dB�LB�-B�B��B��B��B��B��B��B��B�uB�bB�JB�=B�+B�B�B�B�B�B� B~�B|�B{�Bw�Bu�Bs�Bp�Bn�Bk�BffBbNB\)BW
BR�BM�BJ�BH�BF�BC�BD�BL�BR�BS�BVB^5BbNB^5B[#BVBR�BO�BL�BK�BL�Bk�Bz�Bt�Bo�Bo�Bz�B�B� B�B}�B|�Bv�Bo�BgmBdZB`BBXBS�BS�BYB[#BZBXBT�BVBT�BT�BXBZB[#B\)B[#B[#B]/BaHBaHBaHBaHB`BB`BBaHBbNBdZBgmBiyBjBr�Bw�Bx�Bx�Bv�Bw�Bv�Bu�Bw�Bz�B{�B|�B�B�B� B~�B�B�B�B�B�B�B�B�B�1B�+B�B}�B|�B}�B|�B|�B|�B}�B~�B� B�B�+B�JB�bB�hB��B��B��B��B��B��B��B��B��B�B�9B�FB�RB�XB�qB�}BƨB��B�
B�B�B�#B�/B�BB�sB�B��B	  B	B	B��B	B	+B	DB	bB	�B	�B	�B	$�B	(�B	)�B	+B	0!B	2-B	49B	7LB	9XB	;dB	A�B	C�B	D�B	E�B	G�B	I�B	N�B	S�B	VB	ZB	^5B	`BB	aHB	aHB	bNB	gmB	hsB	hsB	iyB	k�B	k�B	l�B	l�B	l�B	l�B	l�B	m�B	n�B	p�B	q�B	r�B	�5B
�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.28 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191727                              AO  ARCAADJP                                                                    20181005191727    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191727  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191727  QCF$                G�O�G�O�G�O�8000            