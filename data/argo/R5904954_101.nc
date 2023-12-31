CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:11Z creation      
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
resolution        =���   axis      Z          9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   =|   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       >�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   B�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       C�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       G�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       L�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   P�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       Q�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       U�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   Y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       Z�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ^�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       _�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  c�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    d$   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    g$   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    j$   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  m$   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    mP   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    mT   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    mX   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    m\   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  m`   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    m�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    m�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    m�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         m�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         m�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        m�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    m�Argo profile    3.1 1.2 19500101000000  20181005191711  20181005191711  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               eA   AO  6557                            2B  A   APEX                            7468                            062512                          846 @�Ȥ����1   @�ȥ)�,@4��t��dF�x���1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      eA   B   B   @�33@�  A   A   A@  A`  A�  A���A�  A�  A�  A�  A�  A�  B   B  B  B  B ffB(  B0  B7��B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C�fC  C�C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C-�fC0  C2  C4  C5�fC8  C:�C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP�CR�CT�CV  CX  CZ  C\�C^�C`  Ca�fCd  Cf�Ch  Ci�fCl  Cn  Co�fCq�fCs�fCu�fCx  Cz  C|�C~  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C��3C�  C��C��C��3C��3C�  C�  C��3C�  C�  C��C�  C��3C��3C��3C�  C�  C��3C��3C��fC�  C��C��3C�  C��C��C��C��3C�  C��3C�  C�  C�  C�  C��C��3C�  C�  C��3C��C��C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C��C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C��C�  C�  C��3C�  C��fC�  C��C��C��C��C�  C�  C��C�  C��3C��C��C��3D � D  D�fDfD��DfD�fDfD�fD  Dy�D��D� D  Dy�D  D�fD	fD	� D	��D
y�D  D� D  D� Dy�=D�<{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@ə�A��A$��AD��Ad��A�ffA�33A�ffA�ffA�ffA�ffA�ffA�ffB33B	33B33B33B!��B)33B133B8��BA33BI33BQ33BY33Ba33Bi33Bq33By33B���B���B���B���B���B�fgB���B���B���B���B���B���B���B���B���B���B���Bę�Bș�B̙�BЙ�Bԙ�Bؙ�Bܙ�B���B䙚B虚B왚B�B���B���B���C L�CL�CL�CL�CL�C
L�C33CL�CfgCL�CL�CL�CL�CL�CL�CL�C L�C"L�C$L�C&L�C(L�C*L�C,L�C.33C0L�C2L�C4L�C633C8L�C:fgC<L�C>L�C@L�CBL�CDL�CFL�CHL�CJL�CLL�CNL�CPfgCRfgCTfgCVL�CXL�CZL�C\fgC^fgC`L�Cb33CdL�CffgChL�Cj33ClL�CnL�Cp33Cr33Ct33Cv33CxL�CzL�C|fgC~L�C�&fC�&fC�&fC�&fC�&fC�33C�&fC�&fC�&fC��C�&fC��C�&fC�33C�33C��C��C�&fC�&fC��C�&fC�&fC�33C�&fC��C��C��C�&fC�&fC��C��C��C�&fC�33C��C�&fC�33C�@ C�33C��C�&fC��C�&fC�&fC�&fC�&fC�33C��C�&fC�&fC��C�33C�33C�&fC�&fC�&fC��C�&fC�&fC��C�&fC�&fC�&fC�&fC�&fC��C�&fC�&fC�&fC�33C�&fC�33C��C�&fC�&fC�33C�&fC�&fC�&fC�&fC�&fC�&fC�33C�&fC�&fC�33C�&fC�33C�&fC�&fC��C�&fC��C�&fC�33C�33C�33C�33C�&fC�&fC�33C�&fC��C�33C�@ D �D �3D3D��D�D� D�D��D�D��D3D��D�D�3D3D��D3D��D	�D	�3D
�D
��D3D�3D3D�3Dy�pD�F1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AܶFAܶFAܲ-AܮAܰ!Aܲ-AܮAܣ�A�|�A�=qA�/A�&�A��A�oA�
=A�  A��A��;A۸RA�r�A�C�A�G�A�JA�~�A˕�A���Aǰ!A��TA�K�A���A�ZA�%A�v�A�?}A��jA���A���A�ĜA���A��-A�A���A�ZA�O�A���A�I�A�jA���A�/A�A���A���A��A��A���A��mA���A���A�;dA�`BA�jA�A�z�A���A��+A�A�A��HA��A�z�A��TA��A� �A��-A�ffA�(�A�t�A��-A�A�ZA���A�A�p�A��TA�G�A��A��A��;A�;dA�+A���A�A��RA�Q�A�ȴA���A��wA���A��A|A�Az�`Az�Az~�Aw�7Aq7LAlz�Aj�!AhM�Ad�!A`E�A_�A^{AZ=qAVbNASt�AR�uARE�AR{AR�ARJAQ�AQ
=AM`BAKoAJ�DAI�^AIt�AHn�AF��AE�FAC�AA�7AAO�AA/A@ffA?�A>I�A=�PA<��A;��A:jA9�FA8ȴA7%A3+A1��A1�A0^5A/�wA.�A-��A,1A+�hA*ȴA)��A(I�A'%A%�wA#O�A"-A!��A �`AoA�uA%A��Av�Ax�AĜA{A7LA{A��A�A?}A��A��AO�A�HA��A�AA��A$�A�A�-AdZA
ĜA
~�A
1A�yAQ�AAS�A��A��A�9A��A�AM�AC�A ��A ~�A =qA �@��@�G�@��+@��j@�"�@�@���@�ƨ@�7L@�(�@׶F@��y@�J@�&�@�%@��`@�9X@��@�$�@ѡ�@�?}@���@У�@��;@�l�@��@���@�9X@�C�@��@�%@ȴ9@���@���@�@ŉ7@��@��@��;@�ƨ@ÍP@�S�@��@���@���@��9@�1@�t�@�S�@�33@�o@��@�hs@�!�@i�=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111 AܶFAܶFAܲ-AܮAܰ!Aܲ-AܮAܣ�A�|�A�=qA�/A�&�A��A�oA�
=A�  A��A��;A۸RA�r�A�C�A�G�A�JA�~�A˕�A���Aǰ!A��TA�K�A���A�ZA�%A�v�A�?}A��jA���A���A�ĜA���A��-A�A���A�ZA�O�A���A�I�A�jA���A�/A�A���A���A��A��A���A��mA���A���A�;dA�`BA�jA�A�z�A���A��+A�A�A��HA��A�z�A��TA��A� �A��-A�ffA�(�A�t�A��-A�A�ZA���A�A�p�A��TA�G�A��A��A��;A�;dA�+A���A�A��RA�Q�A�ȴA���A��wA���A��A|A�Az�`Az�Az~�Aw�7Aq7LAlz�Aj�!AhM�Ad�!A`E�A_�A^{AZ=qAVbNASt�AR�uARE�AR{AR�ARJAQ�AQ
=AM`BAKoAJ�DAI�^AIt�AHn�AF��AE�FAC�AA�7AAO�AA/A@ffA?�A>I�A=�PA<��A;��A:jA9�FA8ȴA7%A3+A1��A1�A0^5A/�wA.�A-��A,1A+�hA*ȴA)��A(I�A'%A%�wA#O�A"-A!��A �`AoA�uA%A��Av�Ax�AĜA{A7LA{A��A�A?}A��A��AO�A�HA��A�AA��A$�A�A�-AdZA
ĜA
~�A
1A�yAQ�AAS�A��A��A�9A��A�AM�AC�A ��A ~�A =qA �@��@�G�@��+@��j@�"�@�@���@�ƨ@�7L@�(�@׶F@��y@�J@�&�@�%@��`@�9X@��@�$�@ѡ�@�?}@���@У�@��;@�l�@��@���@�9X@�C�@��@�%@ȴ9@���@���@�@ŉ7@��@��@��;@�ƨ@ÍP@�S�@��@���@���@��9@�1@�t�@�S�@�33@�o@��@�hs@�!�@i�=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BW
BW
BW
BVBVBVBVBT�BS�BQ�BP�BP�BO�BN�BN�BM�BM�BK�BI�BD�B2-B�BVB
=B�B%�B:^BA�BD�BP�B`BBcTBgmBq�B~�B�+B�PB�uB��B��B��B��B�'B�?B�qB�jB�3B�!B��B��B��B��B��B��B��B��B�VB� Bt�BhsB_;BL�BF�B?}B9XB2-B(�B�B+BB��B��B�`B��B��B��B��B�^B��B�Bs�BN�B)�BbBB
��B
�sB
�/B
��B
�FB
�B
��B
��B
��B
�7B
t�B
bNB
F�B
1'B
'�B
(�B
'�B
{B	�`B	ÖB	�FB	��B	�DB	p�B	gmB	_;B	J�B	5?B	'�B	"�B	!�B	�B	�B	�B	�B	�B	1B��B��B��B�B�B�sB�NB�)B��B��B��B��B��BȴBŢBB�}B�XB�9B�'B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�hB�VB�JB�7B�1B�+B�B}�By�Bv�Bt�Bs�Bt�Bt�Bx�B}�B}�B~�B�B�7B�\B�PB�VB�bB�\B�oB�hB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�bB�VB�JBE�B�-B�-B�-B�3B�?B�?B�?B�FB�XB�dB�qB�wB�}B�}B��BBÖBƨB��B��B��B��B�B�B�B�B�B�#B�;B�BB�BB�HB�NB�TB�sB�B�B�B�B�B�B�B��B��B	�B
�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222442222222222222222222222222222222222222222222222 BW
BW
BW
BVBVBVBVBT�BS�BQ�BP�BP�BO�BN�BN�BM�BM�BK�BI�BD�B2-B�BVB
=B�B%�B:^BA�BD�BP�B`BBcTBgmBq�B~�B�+B�PB�uB��B��B��B��B�'B�?B�qB�jB�3B�!B��B��B��B��B��B��B��B��B�VB� Bt�BhsB_;BL�BF�B?}B9XB2-B(�B�B+BB��B��B�`B��B��B��B��B�^B��B�Bs�BN�B)�BbBB
��B
�sB
�/B
��B
�FB
�B
��B
��B
��B
�7B
t�B
bNB
F�B
1'B
'�B
(�B
'�B
{B	�`B	ÖB	�FB	��B	�DB	p�B	gmB	_;B	J�B	5?B	'�B	"�B	!�B	�B	�B	�B	�B	�B	1B��B��B��B�B�B�sB�NB�)B��B��B��B��B��BȴBŢBB�}B�XB�9B�'B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�hB�VB�JB�7B�1B�+B�B}�By�Bv�Bt�Bs�Bt�Bt�Bx�B}�B}�B~�B�B�7B�\B�PB�VB�bB�\B�oB�hB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�bB�VB�JBE�B�-B�-B�-B�3B�?B�?B�?B�FB�XB�dB�qB�wB�}B�}B��BBÖBƨB��B��B��B��B�B�B�B�B�B�#B�;B�BB�BB�HB�NB�TB�sB�B�B�B�B�B�B�B��B��B	�B
�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222442222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.30 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191711                              AO  ARCAADJP                                                                    20181005191711    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191711  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191711  QCF$                G�O�G�O�G�O�C000            