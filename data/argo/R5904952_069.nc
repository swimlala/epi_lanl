CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  c   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
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
_FillValue                 d  >�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  @`   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 d  E�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  GP   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  L�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 d  Rh   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  S�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 d  YX   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Z�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  `H   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 d  e�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  g8   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 d  l�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  n(   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  s�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    s�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    v�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    y�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  |�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    }   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    }   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    }   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    }   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  }    HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    }`   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    }p   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    }t   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         }�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         }�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        }�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    }�Argo profile    3.1 1.2 19500101000000  20181005190520  20181005190520  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               EA   AO  6431                            2B  A   APEX                            7466                            062512                          846 @�������1   @���So�.@1��S����cv�1'1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      EA   A   A   @9��@�  @�  A��A   A@  A`  A�  A�  A�  A�  A�  A�  A�33A�  B ffBffB  B  B   B(ffB0ffB8  B@  BH  BO��BX  B`  Bh  BpffBxffB�  B�  B�  B���B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B���B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6�C8  C:  C<  C>  C@�CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C[�fC^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cy�fC|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��3C��3C��3C�  C�  C�  C��C��3C��3C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C��3C�  C��C��C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C��3C�  C��C�  C��C�  C��3C��3C��3C��3C�  C�  C�  C��C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C��3C��3D y�D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D	  D	� D
  D
y�D  D� DfD� D  D�fD  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D��Dy�DfD�fD  D� D  D� D  Dy�D��D� D  D� D��Dy�D  D� D fD � D ��D!y�D"  D"�fD#  D#�fD$fD$� D%  D%� D%��D&y�D&��D'� D(  D(� D)fD)� D*  D*�fD+  D+� D,fD,�fD-  D-y�D-��D.� D/  D/� D0  D0� D1  D1�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @K�@���@���A{A$z�ADz�Adz�A�=qA�=qA�=qA�=qA�=qA�=qA�p�A�=qB�B	�B�B�B!�B)�B1�B9�BA�BI�BP�RBY�Ba�Bi�Bq�By�B��\B��\B��\B�\)B��\B��\B��\B�B�B��\B��\B��\B��\B��\B��\B��\B��\Bď\B�\)B̏\BЏ\Bԏ\B؏\B܏\B��\B�\)B�\B�\B��\B�\)B��\B��\C G�CG�CG�CG�CG�C
G�CG�CG�CG�CG�CG�CG�CG�CG�CG�CG�C G�C"G�C$G�C&G�C(G�C*G�C,G�C.G�C0G�C2G�C4G�C6aHC8G�C:G�C<G�C>G�C@aHCBG�CDG�CFG�CHG�CJG�CLG�CNG�CPG�CRG�CTG�CVG�CXG�CZG�C\.C^G�C`G�CbG�CdG�CfG�ChG�CjG�ClG�CnG�CpG�CrG�CtG�CvG�CxG�Cz.C|G�C~G�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�0�C�
C�
C�
C�#�C�#�C�#�C�0�C�
C�
C�
C�#�C�#�C�#�C�#�C�
C�#�C�#�C�#�C�#�C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�0�C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�0�C�0�C�#�C�#�C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�0�C�#�C�#�C�
C�
C�#�C�0�C�0�C�#�C�#�C�#�C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�#�C�#�C�
C�
C�#�C�0�C�#�C�0�C�#�C�
C�
C�
C�
C�#�C�#�C�#�C�0�C�#�C�#�C�0�C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�
D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��DRD��D�D�RD�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��DRD�RD�D��D�D��D�D��D�D��D�D��D�D��D�D��D RD ��D!�D!��D"�D"�RD#�D#�RD$RD$��D%�D%��D&�D&��D'�D'��D(�D(��D)RD)��D*�D*�RD+�D+��D,RD,�RD-�D-��D.�D.��D/�D/��D0�D0��D1�D1�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A���A��
A��
A���A��#A��A��A��A��A��A���A���A���A��
A��
A��A��#A��#A��/A��;A��;A��HA���A�ffAټjA���A�;dA�C�A���AٸRA�\)Aإ�A��#A��A�v�A�-A�A�ĜAсAЉ7A��AΙ�A�33A���A�ffA���A�VA�ȴA�bNAąAÓuA�S�A�$�A���AA�A�(�A�=qA��A�VA���A�ĜA��A���A���A�7LA�O�A���A�`BA���A�
=A���A��;A��uA�A�ĜA��A�A�A��`A��yA��mA�^5A���A��TA��yA��A�ffA�Q�A��A���A�7LA�`BA�A�A�
=A�E�A���A�x�A� �A��A�;dA�^5A��A�?}A��mA��DA���A��wA��A�=qA�A��mA��A�n�A~bAydZAvJArVAlA�AfbNAdVAcoAb1'A^��A[%AZ�uAWO�AQ&�AO|�AN�`ANv�AK�AH�AF��AE�AC�hA@�/A=�TA<9XA:��A8�RA7oA5C�A2�A2JA1ƨA0��A.bNA,�!A+�mA*VA&�A&  A$��A$��A%oA$�9A#XA!�
A!l�A ��A ȴA =qA��Al�AE�A��A�A/A��A-AO�A��A��AA�!A�HAbNAdZAn�A�#AXA��A�Ap�A~�A=qA�+AI�A\)A	�wA	�wA	%AG�A7LA�A��A��A�yA��A �Al�A ��@��P@�@�X@�/@�r�@�;dA ��A M�@�^5@��@�A�@���@���@���@���@��@��@��y@�+@��/@��T@��`@�Q�@��;@�+@�\@�$�@�@�&�@蛦@��m@�9X@��`@�-@�bN@��@��@�^5@⟾@��@�z�@�ƨ@�5?@�Ĝ@۾w@�C�@�o@�%@�hs@��@�V@�O�@�v�@ۅ@ۅ@�dZ@���@��@ٺ^@�X@��/@�  @׮@�S�@��H@�~�@��#@�?}@��@ԣ�@ӥ�@���@�ȴ@ҧ�@�E�@��T@У�@Ͼw@���@�1@��m@�;d@�~�@͑h@���@��`@��@�;d@�^5@��@�A�@���@Ƨ�@���@�?}@���@�Z@���@��@�"�@�33@�ff@�bN@��P@��y@��y@���@�(�@�|�@�dZ@��
@�+@�
=@�+@��!@�n�@�@���@�=q@�X@�G�@�j@��@�|�@�dZ@�33@�o@�C�@�
=@�o@�;d@�;d@���@��\@�M�@�V@��^@��@��u@��@��@�(�@��@���@�o@���@���@��@�n�@��+@���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A���A��
A��
A���A��#A��A��A��A��A��A���A���A���A��
A��
A��A��#A��#A��/A��;A��;A��HA���A�ffAټjA���A�;dA�C�A���AٸRA�\)Aإ�A��#A��A�v�A�-A�A�ĜAсAЉ7A��AΙ�A�33A���A�ffA���A�VA�ȴA�bNAąAÓuA�S�A�$�A���AA�A�(�A�=qA��A�VA���A�ĜA��A���A���A�7LA�O�A���A�`BA���A�
=A���A��;A��uA�A�ĜA��A�A�A��`A��yA��mA�^5A���A��TA��yA��A�ffA�Q�A��A���A�7LA�`BA�A�A�
=A�E�A���A�x�A� �A��A�;dA�^5A��A�?}A��mA��DA���A��wA��A�=qA�A��mA��A�n�A~bAydZAvJArVAlA�AfbNAdVAcoAb1'A^��A[%AZ�uAWO�AQ&�AO|�AN�`ANv�AK�AH�AF��AE�AC�hA@�/A=�TA<9XA:��A8�RA7oA5C�A2�A2JA1ƨA0��A.bNA,�!A+�mA*VA&�A&  A$��A$��A%oA$�9A#XA!�
A!l�A ��A ȴA =qA��Al�AE�A��A�A/A��A-AO�A��A��AA�!A�HAbNAdZAn�A�#AXA��A�Ap�A~�A=qA�+AI�A\)A	�wA	�wA	%AG�A7LA�A��A��A�yA��A �Al�A ��@��P@�@�X@�/@�r�@�;dA ��A M�@�^5@��@�A�@���@���@���@���@��@��@��y@�+@��/@��T@��`@�Q�@��;@�+@�\@�$�@�@�&�@蛦@��m@�9X@��`@�-@�bN@��@��@�^5@⟾@��@�z�@�ƨ@�5?@�Ĝ@۾w@�C�@�o@�%@�hs@��@�V@�O�@�v�@ۅ@ۅ@�dZ@���@��@ٺ^@�X@��/@�  @׮@�S�@��H@�~�@��#@�?}@��@ԣ�@ӥ�@���@�ȴ@ҧ�@�E�@��T@У�@Ͼw@���@�1@��m@�;d@�~�@͑h@���@��`@��@�;d@�^5@��@�A�@���@Ƨ�@���@�?}@���@�Z@���@��@�"�@�33@�ff@�bN@��P@��y@��y@���@�(�@�|�@�dZ@��
@�+@�
=@�+@��!@�n�@�@���@�=q@�X@�G�@�j@��@�|�@�dZ@�33@�o@�C�@�
=@�o@�;d@�;d@���@��\@�M�@�V@��^@��@��u@��@��@�(�@��@���@�o@���@���@��@�n�@��+@���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�1B
�1B
�1B
�1B
�7B
�7B
�bB
ƨB
��B�B@�BM�BK�BH�BE�BE�BE�BA�BD�BhsBs�Bo�BjBp�B�=B��B�B�-BǮB�NB�5B�B+BoB)�B-B1'BA�BVBs�B�B�bB��B��B�-B�jB��BBĜBȴB��B��B�B�B�BB�;B�NB�NB�TB�HB�)B�B��B��B�qB�3B��B��B�1Bu�BG�B2-B-B'�B�BDB�)B�qB�'B�B�-B�XB��Bx�B+B
��B
��B
�RB
�'B
��B
�7B
x�B
l�B
cTB
W
B
?}B
5?B
�B
B	�B	ɺB	��B	�B	u�B	m�B	e`B	S�B	I�B	H�B	>wB	'�B	�B	�B	�B	+B��B�B�`B�#B��BȴBB�jB�LB�9B�B�B��B��B��B��B��B��B��B��B��B��B�B�XB�dB�dB�}BBÖBB�}B�}BBƨBƨBǮBŢB��B�qB�wB�}BŢBƨB��B�
B�
B��B��B��B��B��BɺB��B�
B�)B�5B�fB�;B�B�B�B��B�
B�B�TB�sB�B��BƨBȴB��BǮBBBƨBȴB�fB	B��B��B�B�B�ZB�`B�B�B�B��B�B�B�B�B�B�B�B��B��B��B	  B	B	B		7B	VB	�B	!�B	�B	uB	{B	uB	"�B	"�B	 �B	�B	�B	�B	�B	�B	%�B	;dB	,B	.B	0!B	6FB	A�B	K�B	Q�B	XB	W
B	_;B	aHB	aHB	bNB	gmB	gmB	gmB	gmB	hsB	hsB	jB	l�B	l�B	n�B	p�B	p�B	q�B	q�B	t�B	v�B	v�B	{�B	�B	�B	� B	}�B	{�B	z�B	�B	�B	�B	�B	~�B	�B	�7B	�+B	�%B	�%B	�%B	�%B	�%B	�7B	�DB	�PB	�DB	�1B	�B	�B	�B	�1B	�bB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�uB	�uB	�oB	�bB	�hB	�hB	�hB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�!B	�9B	�9B	�FB	�FB	�?B	�9B	�-B	�'B	�!B	�3B	�FB	�LB	�L2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�1B
�1B
�1B
�1B
�7B
�7B
�bB
ƨB
��B�B@�BM�BK�BH�BE�BE�BE�BA�BD�BhsBs�Bo�BjBp�B�=B��B�B�-BǮB�NB�5B�B+BoB)�B-B1'BA�BVBs�B�B�bB��B��B�-B�jB��BBĜBȴB��B��B�B�B�BB�;B�NB�NB�TB�HB�)B�B��B��B�qB�3B��B��B�1Bu�BG�B2-B-B'�B�BDB�)B�qB�'B�B�-B�XB��Bx�B+B
��B
��B
�RB
�'B
��B
�7B
x�B
l�B
cTB
W
B
?}B
5?B
�B
B	�B	ɺB	��B	�B	u�B	m�B	e`B	S�B	I�B	H�B	>wB	'�B	�B	�B	�B	+B��B�B�`B�#B��BȴBB�jB�LB�9B�B�B��B��B��B��B��B��B��B��B��B��B�B�XB�dB�dB�}BBÖBB�}B�}BBƨBƨBǮBŢB��B�qB�wB�}BŢBƨB��B�
B�
B��B��B��B��B��BɺB��B�
B�)B�5B�fB�;B�B�B�B��B�
B�B�TB�sB�B��BƨBȴB��BǮBBBƨBȴB�fB	B��B��B�B�B�ZB�`B�B�B�B��B�B�B�B�B�B�B�B��B��B��B	  B	B	B		7B	VB	�B	!�B	�B	uB	{B	uB	"�B	"�B	 �B	�B	�B	�B	�B	�B	%�B	;dB	,B	.B	0!B	6FB	A�B	K�B	Q�B	XB	W
B	_;B	aHB	aHB	bNB	gmB	gmB	gmB	gmB	hsB	hsB	jB	l�B	l�B	n�B	p�B	p�B	q�B	q�B	t�B	v�B	v�B	{�B	�B	�B	� B	}�B	{�B	z�B	�B	�B	�B	�B	~�B	�B	�7B	�+B	�%B	�%B	�%B	�%B	�%B	�7B	�DB	�PB	�DB	�1B	�B	�B	�B	�1B	�bB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�uB	�uB	�oB	�bB	�hB	�hB	�hB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�!B	�9B	�9B	�FB	�FB	�?B	�9B	�-B	�'B	�!B	�3B	�FB	�LB	�L2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.28 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190520                              AO  ARCAADJP                                                                    20181005190520    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190520  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190520  QCF$                G�O�G�O�G�O�8000            