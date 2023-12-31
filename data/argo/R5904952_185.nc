CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  d   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:47Z creation      
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
_FillValue                 d  ?    PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  @d   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 d  E�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  GX   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  L�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 d  Rx   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  S�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 d  Yl   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Z�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ``   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 d  e�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  gT   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 d  l�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  nH   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  s�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    t   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    w   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    z   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  }   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    }4   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    }8   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    }<   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    }@   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  }D   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    }�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    }�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    }�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         }�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         }�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        }�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    }�Argo profile    3.1 1.2 19500101000000  20181005190547  20181005190547  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��ĥp1   @��I��6@1�hr� ��c�Ƨ1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @���@�  A   A   A@  A`  A�  A���A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B7��B?��BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C�C  C  C  C  C  C�fC  C  C  C   C!�fC#�fC&  C(  C*  C,  C.  C/�fC2  C4�C6  C8  C:�C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb�Cd�Cf  Ch  Cj  Cl  Cn  Co�fCr  Ct  Cu�fCx  Cz  C|  C~  C��C�  C�  C�  C��C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C��3C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C��3C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C�  C�  C�  C��C��C��C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  Dy�D��D� DfD� D  D� DfD� D  D� D��D� D	  D	� D
  D
� D
��D� D  Dy�D��D� DfD�fD  D� D  D� D  D� D  D�fDfD�fDfD� DfD�fD  D� D  D� D  D� D��D� D  D� D  D�fD  D� D  Dy�D��Dy�D��Dy�D��D y�D ��D!y�D"  D"� D#fD#� D$  D$� D%  D%y�D%��D&� D'  D'� D(  D(� D)  D)� D)��D*y�D*��D+y�D,  D,� D-  D-� D.  D.� D/  D/� D0  D0y�D1  Dy�RD�:=D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�34@ə�A��A$��AD��Ad��A�ffA�33A�ffA�ffA�ffA�ffA�ffA�ffB33B	33B33B33B!33B)33B133B8��B@��BI33BQ33BY33Ba33Bi33Bq33By33B���B���B���B���B���B���B���B���B���B���B���B���B�fgB���B���B���B���Bę�Bș�B̙�BЙ�Bԙ�Bؙ�Bܙ�B���B䙚B虚B왚B�B���B���B���C L�CL�CL�CL�CL�C
L�CfgCL�CL�CL�CL�CL�C33CL�CL�CL�C L�C"33C$33C&L�C(L�C*L�C,L�C.L�C033C2L�C4fgC6L�C8L�C:fgC<L�C>L�C@L�CBL�CDL�CFL�CHL�CJL�CLL�CNL�CPL�CRL�CTL�CVL�CXL�CZL�C\L�C^L�C`L�CbfgCdfgCfL�ChL�CjL�ClL�CnL�Cp33CrL�CtL�Cv33CxL�CzL�C|L�C~L�C�33C�&fC�&fC�&fC�33C�&fC�&fC�&fC�33C�33C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C�&fC�&fC��C�&fC�&fC�&fC�&fC��C�&fC�&fC�33C�&fC�&fC�&fC��C�&fC�&fC�&fC�&fC�33C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�33C�&fC�&fC��C�&fC�&fC�33C�&fC�&fC�&fC�&fC�33C�&fC�&fC�&fC�&fC�&fC��C��C�&fC�33C�&fC�&fC�&fC��C�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C��C�&fC�&fC�&fC�&fC�&fC�&fC�33C�&fC�&fC��C�&fC�&fC�&fC�33C�33C�33C�&fC�&fC��C��C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fD 3D �3D3D�3D3D��D�D�3D�D�3D3D�3D�D�3D3D�3D�D�3D	3D	�3D
3D
�3D�D�3D3D��D�D�3D�D��D3D�3D3D�3D3D�3D3D��D�D��D�D�3D�D��D3D�3D3D�3D3D�3D�D�3D3D�3D3D��D3D�3D3D��D�D��D�D��D �D ��D!�D!��D"3D"�3D#�D#�3D$3D$�3D%3D%��D&�D&�3D'3D'�3D(3D(�3D)3D)�3D*�D*��D+�D+��D,3D,�3D-3D-�3D.3D.�3D/3D/�3D03D0��D13Dy��D�C�D��)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�t�AʁAʏ\AʍPA���A�ƨA���A��A��HA�;dA�M�A�p�AˍPA���A�A̡�A��A��A�=qA�M�A��A���A�z�A�x�A�ZA��A�oA�1A�Q�Aș�A�K�A��yA��A�ffA�\)A�$�AƸRAƃA�`BA�O�A�33A�?}A�ffA�~�A�z�A�v�A�A�A�ĜA�~�A�
=A���A�9XAß�A�A�A�A���A�bNA���A��`A�hsA��A��A��\A��A��PA�v�A��#A�7LA��
A���A��A�5?A���A��A��A�+A��^A�  A���A��A��wA���A�A���A��!A�`BA���A�K�A��A��A��A��A���A��yA�\)A��A�dZA�XA��A��+A�
A}�Awx�As"�AqAp��Ao7LAm�FAl�Ak�Ah��Af�9Ae�Abz�A^�RA\�`A\5?AXVAW��AV��AS�;AS�AR�AP��AM7LAK�hAHA�AFA�AE�AD$�AB��A@�A>��A=A;K�A9��A8ZA5��A4�A2�uA1��A1p�A1%A.��A,�uA*�!A)��A)�wA)dZA'�7A&n�A%;dA#x�A!��A {A1'AVA�mA��A�A~�A�A��A�!A�^Al�A33AȴA�uA^5A�hA�`At�Az�Al�A��AjAVA(�A�AJA/A��A
=A
�uA
$�A��A��A�#A=qA�Ax�AXA��A�
A�^An�A ��A bNA �@�ƨ@�K�@�33@��!@���@�x�@���@��D@�1'@��@�r�@��@�-@�/@��D@�n�@�Q�@�
=@�O�@��@���@�/@��@�@��@�7@�"�@��`@۝�@ڗ�@�-@�J@�x�@�/@�V@�Z@�1@���@�dZ@�^5@�O�@Դ9@�b@ӍP@ҟ�@�7L@Ѓ@���@�33@�S�@�hs@���@̃@��m@˶F@˕�@�C�@�hs@�&�@��@�V@�%@ț�@�r�@��
@��H@�V@�E�@�E�@��T@�x�@ļj@�bN@Å@�o@�{@���@�X@�7L@���@�z�@�A�@�A�@�I�@�(�@��m@��P@�\)@�"�@���@��@��R@���@��+@�v�@�M�@�x�@��`@��u@�A�@�  @�1@�b@�b@�1@���@��@��
@��F@�t�@��@�@���@��7@�Z@��m@�dZ@�o@��@��y@���@���@��+@��@�&�@��F@�dZ@�"�@��H@�{@��#@��@���@��-@�hs@�O�@��@��@��@��@��@�Z@�9X@�1'@��@��m@���@�+@���@�5?@��@��^@�G�@�oi@x�D@h�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�t�AʁAʏ\AʍPA���A�ƨA���A��A��HA�;dA�M�A�p�AˍPA���A�A̡�A��A��A�=qA�M�A��A���A�z�A�x�A�ZA��A�oA�1A�Q�Aș�A�K�A��yA��A�ffA�\)A�$�AƸRAƃA�`BA�O�A�33A�?}A�ffA�~�A�z�A�v�A�A�A�ĜA�~�A�
=A���A�9XAß�A�A�A�A���A�bNA���A��`A�hsA��A��A��\A��A��PA�v�A��#A�7LA��
A���A��A�5?A���A��A��A�+A��^A�  A���A��A��wA���A�A���A��!A�`BA���A�K�A��A��A��A��A���A��yA�\)A��A�dZA�XA��A��+A�
A}�Awx�As"�AqAp��Ao7LAm�FAl�Ak�Ah��Af�9Ae�Abz�A^�RA\�`A\5?AXVAW��AV��AS�;AS�AR�AP��AM7LAK�hAHA�AFA�AE�AD$�AB��A@�A>��A=A;K�A9��A8ZA5��A4�A2�uA1��A1p�A1%A.��A,�uA*�!A)��A)�wA)dZA'�7A&n�A%;dA#x�A!��A {A1'AVA�mA��A�A~�A�A��A�!A�^Al�A33AȴA�uA^5A�hA�`At�Az�Al�A��AjAVA(�A�AJA/A��A
=A
�uA
$�A��A��A�#A=qA�Ax�AXA��A�
A�^An�A ��A bNA �@�ƨ@�K�@�33@��!@���@�x�@���@��D@�1'@��@�r�@��@�-@�/@��D@�n�@�Q�@�
=@�O�@��@���@�/@��@�@��@�7@�"�@��`@۝�@ڗ�@�-@�J@�x�@�/@�V@�Z@�1@���@�dZ@�^5@�O�@Դ9@�b@ӍP@ҟ�@�7L@Ѓ@���@�33@�S�@�hs@���@̃@��m@˶F@˕�@�C�@�hs@�&�@��@�V@�%@ț�@�r�@��
@��H@�V@�E�@�E�@��T@�x�@ļj@�bN@Å@�o@�{@���@�X@�7L@���@�z�@�A�@�A�@�I�@�(�@��m@��P@�\)@�"�@���@��@��R@���@��+@�v�@�M�@�x�@��`@��u@�A�@�  @�1@�b@�b@�1@���@��@��
@��F@�t�@��@�@���@��7@�Z@��m@�dZ@�o@��@��y@���@���@��+@��@�&�@��F@�dZ@�"�@��H@�{@��#@��@���@��-@�hs@�O�@��@��@��@��@��@�Z@�9X@�1'@��@��m@���@�+@���@�5?@��@��^@�G�@�oi@x�D@h�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�=B�7B�=B�JB��B�B�ZB��B	B	2-B	_;B	�uB	�B	�B	�NB
-B
YB
u�B
�\B
��B
�9B
�?B
��B
�B�B%�B2-B@�B�B
�TB
�B
�#B
�mB\B�B�BJB1B1BbB{B�BJ�BiyBjBiyBp�B�\B��B�-B�9B�LB�RB�?B�!B�9B�B��B
=B�BbB�B,B9XBA�BC�BE�BH�BH�BD�BB�BA�B=qB>wB=qB7LB2-B.B+B!�B�BB��B�B�B�LB�hBx�BXB%�BB
��B
�B
� B
XB
A�B
 �B
B	�NB	�B	ƨB	�XB	��B	�%B	�B	z�B	r�B	iyB	aHB	ZB	L�B	A�B	<jB	1'B	#�B	 �B	 �B	{B	JB��B�sB�B�B�BB��B��B��B�dB�XB�RB�9B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�9B�FB�?B�3B�3B�?B�RB�?B�3B�?B�?B�LB�XB�qB�}B�wBƨBǮBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�B�B�5B�HB�`B�fB�mB�B�B�B�B�B��B��B��B	B	B	B	B		7B		7B	
=B	JB	VB	\B	oB	uB	uB	�B	�B	�B	�B	!�B	,B	,B	.B	0!B	49B	5?B	7LB	8RB	;dB	;dB	<jB	=qB	A�B	E�B	D�B	G�B	M�B	Q�B	R�B	T�B	ZB	\)B	`BB	`BB	cTB	e`B	hsB	hsB	jB	jB	k�B	o�B	o�B	q�B	r�B	r�B	s�B	t�B	u�B	v�B	x�B	z�B	|�B	}�B	}�B	}�B	}�B	� B	�B	�B	�B	�%B	�1B	�7B	�7B	�7B	�7B	�=B	�DB	�DB	�PB	�VB	�\B	�hB	�oB	�oB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�-B	�!B	�'B	�B	�!B	�!B	�!B	�'B	�-B	�3B	�9B	�?B	�LB	�LB	�RB	�^B
�B
'�B
0�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222B�=B�7B�=B�JB��B�B�ZB��B	B	2-B	_;B	�uB	�B	�B	�NB
-B
YB
u�B
�\B
��B
�9B
�?B
��B
�B�B%�B2-B@�B�B
�TB
�B
�#B
�mB\B�B�BJB1B1BbB{B�BJ�BiyBjBiyBp�B�\B��B�-B�9B�LB�RB�?B�!B�9B�B��B
=B�BbB�B,B9XBA�BC�BE�BH�BH�BD�BB�BA�B=qB>wB=qB7LB2-B.B+B!�B�BB��B�B�B�LB�hBx�BXB%�BB
��B
�B
� B
XB
A�B
 �B
B	�NB	�B	ƨB	�XB	��B	�%B	�B	z�B	r�B	iyB	aHB	ZB	L�B	A�B	<jB	1'B	#�B	 �B	 �B	{B	JB��B�sB�B�B�BB��B��B��B�dB�XB�RB�9B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�9B�FB�?B�3B�3B�?B�RB�?B�3B�?B�?B�LB�XB�qB�}B�wBƨBǮBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�B�B�5B�HB�`B�fB�mB�B�B�B�B�B��B��B��B	B	B	B	B		7B		7B	
=B	JB	VB	\B	oB	uB	uB	�B	�B	�B	�B	!�B	,B	,B	.B	0!B	49B	5?B	7LB	8RB	;dB	;dB	<jB	=qB	A�B	E�B	D�B	G�B	M�B	Q�B	R�B	T�B	ZB	\)B	`BB	`BB	cTB	e`B	hsB	hsB	jB	jB	k�B	o�B	o�B	q�B	r�B	r�B	s�B	t�B	u�B	v�B	x�B	z�B	|�B	}�B	}�B	}�B	}�B	� B	�B	�B	�B	�%B	�1B	�7B	�7B	�7B	�7B	�=B	�DB	�DB	�PB	�VB	�\B	�hB	�oB	�oB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�-B	�!B	�'B	�B	�!B	�!B	�!B	�'B	�-B	�3B	�9B	�?B	�LB	�LB	�RB	�^B
�B
'�B
0�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.30 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190547                              AO  ARCAADJP                                                                    20181005190547    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190547  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190547  QCF$                G�O�G�O�G�O�8000            