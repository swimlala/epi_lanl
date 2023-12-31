CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  d   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:20Z creation      
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
_FillValue                    }�Argo profile    3.1 1.2 19500101000000  20181005190520  20181005190520  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               CA   AO  6431                            2B  A   APEX                            7466                            062512                          846 @�� ��r:1   @��!m�E�@1vE�����cy?|�h1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      CA   A   A   @���@���A   AffA>ffA`  A�  A�  A�  A�  A�  A�33A�33A�  A�33B  BffB  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B���B���B���B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�fC��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C��3C�  C�  C�  C�  C�  C��3C��C�  C��3C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C��3C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C��3C�  C��C��C��C��3C�  D   D � D  D�fD  D� D  D� D  D� D  D� D  D� D��D� D  D� D	  D	� D
  D
� DfD� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� DfD�fD  Dy�D  D� D��D� D  D� D  D� D  D� D  D� DfD�fD  D� D  Dy�D  D�fD  D� D   D � D!  D!� D"fD"� D"��D#y�D$  D$� D%  D%� D&  D&� D'  D'y�D(  D(� D)  D)� D*  D*y�D+  D+�fD,  D,� D-  D-� D-��D.� D/  D/y�D0  D0�fD1fD1�fD2  D2��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��RA ��A\)A?\)A`��A�z�A�z�A�z�A�z�A�z�AϮA߮A�z�A��B=qB��B=qB =qB(=qB0=qB8=qB@=qBH=qBP=qBX=qB`=qBh��Bp=qBx=qB��B��B��B��B��B��B��B��B��B�Q�B��B�Q�B��B��B��B��B��B��B��B��B��B��B�Q�B��B��B��B��B��B��B��B��B��C \C\C\C\C\C
\C\C\C\C\C\C\C\C\C\C\C \C"\C$\C&\C(\C*\C,\C.\C0\C2\C4\C6\C8\C:\C<\C>\C@\CB\CD\CF\CH\CJ\CL\CN\CP\CR\CT\CV\CX\CZ\C\\C^\C`\Cb\Cd\Cf\Ch\Cj\Cl\Cn\Cp\Cr\Ct\Cv\Cx\Cz\C|\C~\C��C���C��C��C��C�{C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C�{C��C��C��C��C��C�{C��C��C���C��C��C��C��C��C���C�{C��C���C���C��C�{C��C��C��C��C��C��C��C�{C��C��C��C�{C��C��C��C��C���C��C��C��C��C��C��C��C���C��C��C��C���C��C��C��C��C��C��C��C��C��C��C�{C�{C��C���C��C��C�{C��C��C��C���C��C��C��C��C��C��C��C�{C��C��C�{C��C��C��C��C��C��C��C��C��C�{C�{C��C���C��C�{C�{C�{C���C��D �D ��D�D�=D�D��D�D��D�D��D�D��D�D��D�qD��D�D��D	�D	��D
�D
��D
=D��D�D��D�D}qD�D��D�D��D�D��D�D��D�D��D
=D�=D�D}qD�D��D�qD��D�D��D�D��D�D��D�D��D
=D�=D�D��D�D}qD�D�=D�D��D �D ��D!�D!��D"
=D"��D"�qD#}qD$�D$��D%�D%��D&�D&��D'�D'}qD(�D(��D)�D)��D*�D*}qD+�D+�=D,�D,��D-�D-��D-�qD.��D/�D/}qD0�D0�=D1
=D1�=D2�D2��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�JA�oA��A��A�$�A�-A�(�A�&�A�-A�/A�-A�-A�33A�1'A�/A�/A�1'A�33A�5?A�5?A�33A�A��A֝�A�;dA���A�^5A�5?A�5?A��A��/A�Q�Aϴ9A��A�ffA�1A��A�+A��A˟�A���A�(�A��A�bNA�S�A�VA���A���Aç�A�S�A+A�%A��A�
=A�O�A�r�A��A�A��PA�ZA�O�A�O�A��uA���A��A�{A��hA�\)A��A��/A���A�M�A�JA���A���A�C�A��mA�bA�E�A�|�A��RA��+A�A��;A�A�A��A�ƨA��RA�r�A�oA�-A�`BA���A��A���A��;A�l�A�VA�"�A��A�XA�=qA�I�A�^5A���A��uA�$�A��PA�JA���A}K�Ay�#Ax9XAu��Ap$�Akp�Aj-Af��Aa��A^1'A[�;AZM�AV��AR�AQ�AN{AJbNAGoAC�AB^5AAx�A>(�A<M�A;hsA: �A7�A4{A2��A1dZA0I�A/�PA.I�A-�hA-�A,��A+��A*�yA)��A)VA(�A&{A#�
A!�
A ��A v�A (�AC�A��A
=A��A�A�;A��Al�A�AjAffAVAG�AM�A�
A`BA�9A1A7LA-A+AZA�/AE�A\)A
9XA	�A	S�A�A��A��A�AE�Al�A33A(�A{A��A�A�-A��A �`A (�@��@��/@�|�@���@��!@�+@�1@�`B@�&�@��@�bN@��@�^5@�G�@�p�@�"�@��@��T@��@��
@��@�O�@�z�@�I�@��@���@ꗍ@��@�^@��@�O�@�%@�%@�7L@�p�@�O�@�P@��@�^5@�Ĝ@�1@���@��`@�9@�Z@��;@�33@�5?@�&�@ߍP@ݩ�@�Q�@۾w@�=q@ٙ�@��;@�o@�n�@ָR@�l�@�S�@׍P@�\)@�\)@�K�@��@�J@��/@��;@�S�@�ȴ@�@У�@�1@ϕ�@��@Ο�@Ο�@·+@�J@�@�X@̓u@�A�@�I�@��
@�C�@�+@�@ʗ�@�@ə�@�?}@�?}@�%@��`@ȴ9@�j@Ǖ�@�$�@�G�@���@��y@°!@�v�@�J@��-@�x�@�O�@���@�
=@���@��@��@��H@���@�5?@��^@��@���@���@��@��@���@��9@��w@�+@�\)@�p�@��j@�t�@�;d@�;d@��H@�E�@�ff@�n�@�V@�`B@��9@�(�@�z�@��@��D@��@�C�@���@��T@��9@��@�b@�l�@��@�/@���@�I�@�Z11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�JA�oA��A��A�$�A�-A�(�A�&�A�-A�/A�-A�-A�33A�1'A�/A�/A�1'A�33A�5?A�5?A�33A�A��A֝�A�;dA���A�^5A�5?A�5?A��A��/A�Q�Aϴ9A��A�ffA�1A��A�+A��A˟�A���A�(�A��A�bNA�S�A�VA���A���Aç�A�S�A+A�%A��A�
=A�O�A�r�A��A�A��PA�ZA�O�A�O�A��uA���A��A�{A��hA�\)A��A��/A���A�M�A�JA���A���A�C�A��mA�bA�E�A�|�A��RA��+A�A��;A�A�A��A�ƨA��RA�r�A�oA�-A�`BA���A��A���A��;A�l�A�VA�"�A��A�XA�=qA�I�A�^5A���A��uA�$�A��PA�JA���A}K�Ay�#Ax9XAu��Ap$�Akp�Aj-Af��Aa��A^1'A[�;AZM�AV��AR�AQ�AN{AJbNAGoAC�AB^5AAx�A>(�A<M�A;hsA: �A7�A4{A2��A1dZA0I�A/�PA.I�A-�hA-�A,��A+��A*�yA)��A)VA(�A&{A#�
A!�
A ��A v�A (�AC�A��A
=A��A�A�;A��Al�A�AjAffAVAG�AM�A�
A`BA�9A1A7LA-A+AZA�/AE�A\)A
9XA	�A	S�A�A��A��A�AE�Al�A33A(�A{A��A�A�-A��A �`A (�@��@��/@�|�@���@��!@�+@�1@�`B@�&�@��@�bN@��@�^5@�G�@�p�@�"�@��@��T@��@��
@��@�O�@�z�@�I�@��@���@ꗍ@��@�^@��@�O�@�%@�%@�7L@�p�@�O�@�P@��@�^5@�Ĝ@�1@���@��`@�9@�Z@��;@�33@�5?@�&�@ߍP@ݩ�@�Q�@۾w@�=q@ٙ�@��;@�o@�n�@ָR@�l�@�S�@׍P@�\)@�\)@�K�@��@�J@��/@��;@�S�@�ȴ@�@У�@�1@ϕ�@��@Ο�@Ο�@·+@�J@�@�X@̓u@�A�@�I�@��
@�C�@�+@�@ʗ�@�@ə�@�?}@�?}@�%@��`@ȴ9@�j@Ǖ�@�$�@�G�@���@��y@°!@�v�@�J@��-@�x�@�O�@���@�
=@���@��@��@��H@���@�5?@��^@��@���@���@��@��@���@��9@��w@�+@�\)@�p�@��j@�t�@�;d@�;d@��H@�E�@�ff@�n�@�V@�`B@��9@�(�@�z�@��@��D@��@�C�@���@��T@��9@��@�b@�l�@��@�/@���@�I�@�Z11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
|�B
}�B
}�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
|�B
�%B
�BBhB!�B6FB/B2-BH�BaHBy�B�B�\B�uB�oB�uB��B��B�FB��B�/B�BBoB�B�B)�B?}BC�BH�BR�B\)Bo�B�B�%B�%B�bB��B��B�'BB��B��B��B��B��B��B��B��B�B�B��B�B�B�B�HB�ZB�HB�
B�XB��B��B��B�uBr�Bl�Bl�BO�B7LB,B�B�fB�jB�=BS�Bn�Bx�BVB)�BDB
�yB
��B
ŢB
�B
��B
�DB
jB
T�B
@�B
�B
B	��B	�fB	��B	�B	��B	�oB	v�B	^5B	J�B	@�B	1'B	!�B	�B		7B��B�B�HB�#B��B��BÖB��B�jB�RB�B�!B�?B�FB�FB�XB�^B�dB�dB�}BĜBĜBB�wB�?B�!B�B�B�B�B�!B�'B�'B�'B�'B�-B�-B�3B�^B��BǮBȴBȴB��BƨB��BĜBǮBȴB��BĜB��B�}B��BŢBÖBĜBǮBǮB��B��B��B��B��BÖBB��B�}B�jB�wB�}B�wB�dB�}B�jB�^B�jBɺB��B�5B�B�B�B�B�B	B	B��B�fB�NB�NB�NB�TB�NB�NB�TB�ZB�fB�B�B��B��B	B	B	DB	\B	{B	�B	(�B	+B	+B	+B	+B	+B	6FB	9XB	9XB	9XB	;dB	=qB	@�B	A�B	=qB	9XB	9XB	A�B	<jB	:^B	7LB	8RB	:^B	>wB	G�B	R�B	[#B	_;B	cTB	ffB	iyB	l�B	n�B	p�B	q�B	q�B	o�B	n�B	p�B	p�B	p�B	p�B	q�B	q�B	q�B	q�B	q�B	s�B	v�B	y�B	z�B	z�B	{�B	|�B	}�B	~�B	~�B	�B	�B	�%B	�+B	�+B	�+B	�B	�B	� B	~�B	}�B	~�B	� B	� B	�B	�B	�B	� B	~�B	~�B	~�B	~�B	~�B	~�B	� B	� B	�B	�%B	�7B	�JB	�bB	�bB	�bB	�PB	�VB	��B	�uB	�oB	�hB	�uB	�{B	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�3B	�-B	�'B	�!B	�B	�B	�B	�B	��B	��B	��B	��B	��22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
|�B
}�B
}�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
|�B
�%B
�BBhB!�B6FB/B2-BH�BaHBy�B�B�\B�uB�oB�uB��B��B�FB��B�/B�BBoB�B�B)�B?}BC�BH�BR�B\)Bo�B�B�%B�%B�bB��B��B�'BB��B��B��B��B��B��B��B��B�B�B��B�B�B�B�HB�ZB�HB�
B�XB��B��B��B�uBr�Bl�Bl�BO�B7LB,B�B�fB�jB�=BS�Bn�Bx�BVB)�BDB
�yB
��B
ŢB
�B
��B
�DB
jB
T�B
@�B
�B
B	��B	�fB	��B	�B	��B	�oB	v�B	^5B	J�B	@�B	1'B	!�B	�B		7B��B�B�HB�#B��B��BÖB��B�jB�RB�B�!B�?B�FB�FB�XB�^B�dB�dB�}BĜBĜBB�wB�?B�!B�B�B�B�B�!B�'B�'B�'B�'B�-B�-B�3B�^B��BǮBȴBȴB��BƨB��BĜBǮBȴB��BĜB��B�}B��BŢBÖBĜBǮBǮB��B��B��B��B��BÖBB��B�}B�jB�wB�}B�wB�dB�}B�jB�^B�jBɺB��B�5B�B�B�B�B�B	B	B��B�fB�NB�NB�NB�TB�NB�NB�TB�ZB�fB�B�B��B��B	B	B	DB	\B	{B	�B	(�B	+B	+B	+B	+B	+B	6FB	9XB	9XB	9XB	;dB	=qB	@�B	A�B	=qB	9XB	9XB	A�B	<jB	:^B	7LB	8RB	:^B	>wB	G�B	R�B	[#B	_;B	cTB	ffB	iyB	l�B	n�B	p�B	q�B	q�B	o�B	n�B	p�B	p�B	p�B	p�B	q�B	q�B	q�B	q�B	q�B	s�B	v�B	y�B	z�B	z�B	{�B	|�B	}�B	~�B	~�B	�B	�B	�%B	�+B	�+B	�+B	�B	�B	� B	~�B	}�B	~�B	� B	� B	�B	�B	�B	� B	~�B	~�B	~�B	~�B	~�B	~�B	� B	� B	�B	�%B	�7B	�JB	�bB	�bB	�bB	�PB	�VB	��B	�uB	�oB	�hB	�uB	�{B	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�3B	�-B	�'B	�!B	�B	�B	�B	�B	��B	��B	��B	��B	��22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.06 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190520                              AO  ARCAADJP                                                                    20181005190520    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190520  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190520  QCF$                G�O�G�O�G�O�8000            