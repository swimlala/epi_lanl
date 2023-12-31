CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  d   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:57Z creation      
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
_FillValue                    }�Argo profile    3.1 1.2 19500101000000  20181005190557  20181005190557  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @���ݭg
1   @���q�1p@0��1&��c�I�^1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�  @�  A   AffA@  A`  A�  A�  A�33A�  A�  A���A�  A�33B   B  B  B  B   B(  B0  B8ffB@  BH  BP  BXffB`ffBh  Bp  Bx  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C�fC  C   C!�fC$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CW�fCZ  C\  C^  C`  Cb�Cd�Cf  Ch  Cj  Cl  Cn  Cp  Cr  Cs�fCv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C��C��C�  C��C�  C�  C�  C��3C��3C��3C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  D   D � D  D� D  D� D��D� D  D� D  D�fD  Dy�D  D� D  D� D	  D	� D
fD
� D  D� D��D� D  D� D��D� D  D� D  D� D  D�fD  D� D  D� D  Dy�D��Dy�D  D� D  D� DfD� D��D� DfD�fD  D� D  D� D  D� D  D� D  D�fD fD � D!  D!y�D!��D"� D#fD#�fD$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D)��D*y�D+  D+� D,  D,y�D,��D-� D-��D.y�D/  D/�fD0  D0y�D1  DyuD�2�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�@�A�HA!G�AB�HAb�HA�p�A�p�A���A�p�A�p�A�=qA�p�A��B �RB�RB�RB�RB �RB(�RB0�RB9�B@�RBH�RBP�RBY�Ba�Bh�RBp�RBx�RB�\)B�\)B�(�B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�(�B�(�B�\)B�\)Bȏ\B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B��\C .C.C.C.C.C
.C.C.C.C.C.C.C.C.CzC.C .C"zC$.C&.C(.C*.C,.C..C0.C2.C4.C6.C8.C:.C<.C>.C@.CB.CD.CF.CH.CJ.CL.CN.CP.CR.CT.CV.CXzCZ.C\.C^.C`.CbG�CdG�Cf.Ch.Cj.Cl.Cn.Cp.Cr.CtzCv.Cx.Cz.C|.C~.C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
=C�
=C�
C�#�C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
=C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
=C�
=C�
C�
C�
C�
C�
C�
C�
C�#�C�#�C�
C�#�C�
C�
C�
C�
=C�
=C�
=C�
C�
C�
=C�
=C�
C�
C�
C�
C�
C�
C�
C�
C�#�C�#�C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
=C�
=C�
=C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�#�C�
C�
=C�
C�
C�
C�
C�
C�
=C�
C�
C�
C�
C�#�C�
C�
C�
D �D ��D�D��D�D��DD��D�D��D�D��D�D�D�D��D�D��D	�D	��D
�D
��D�D��DD��D�D��DD��D�D��D�D��D�D��D�D��D�D��D�D�DD�D�D��D�D��D�D��DD��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!�D"D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*D*�D+�D+��D,�D,�D-D-��D.D.�D/�D/��D0�D0�D1�Dy��D�8�D� �11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AʾwAʺ^A�ƨA�ȴA�ȴA�ƨA�ƨA�ƨA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A��
A��
A��A��#A��/A��;A��
A�ĜAʺ^AʓuA� �A��`Aȴ9Aȇ+A���A�"�A�VA�AǃA�$�A�bA��`AƶFA�C�A��AđhAÇ+A�1A�`BA��\A��A�t�A�;dA���A��A�(�A��PA�{A��A��A��TA��TA�I�A��A��mA���A�/A�O�A� �A�l�A�|�A�/A�=qA��
A�^5A��A�Q�A�
=A�O�A���A�ffA���A��A�x�A�Q�A���A��wA���A��A��\A�jA���A���A���A��;A�hsA�G�A��yA��PA��hA��A��PA�\)A}O�A{"�Aw��At�\Ar �An�9Am�Aj��Af��Ae��Ad��Ad$�A^  AY`BAXffAW/ARZAO+AK/AG��AD�yACƨACS�ABA�A?K�A=�A=`BA;�#A:JA8�A7��A7dZA6-A3G�A1A/��A.^5A-K�A,��A+�A*�!A*A(��A'�hA%�
A#��A"�!A" �A!��A!dZA ��A�mA�FA�A��AĜAv�A�`A  AVAȴA1A��A��A�PA��AJAAA�FA�A�jA��A&�A�A	�mA	�An�Ax�AM�Ax�A�+AJA�hA5?AS�A b@�M�@�  @��@���@��H@�M�@�hs@���@�E�@�7@���@�hs@��@�r�@�ff@��@���@�P@�V@���@���@�  @��y@�+@�@��@��
@߮@ߕ�@�
=@�5?@�`B@�%@��m@�@ڇ+@�J@ج@�(�@�|�@�-@թ�@Ցh@�&�@�Q�@� �@�S�@�/@��/@��`@Ϯ@�l�@�|�@��m@ϥ�@���@�-@���@͡�@���@�I�@��
@ˮ@˝�@�C�@�
=@ʧ�@ɑh@ȼj@�9X@��
@Ǖ�@�33@Ƨ�@�5?@���@�  @î@��@��@��@�ff@�V@�5?@��j@��
@��!@��T@��@�O�@���@�j@��@�o@���@�~�@���@��y@�l�@�(�@��@��m@�ƨ@�|�@�C�@�n�@�X@��@��9@�j@��w@��@���@�ff@�@�G�@�%@��@��D@� �@��m@�;d@��@�~�@�V@�$�@��h@�X@�?}@��@��/@�bN@��;@��w@��@�C�@�"�@�@�ȴ@�V@��-@��@�X@�?}@�%@��@�ƨ@�|�@�l�@�"�@���@�ff@�V@���@���@�O�@�7L@���@�V@���@��/@�@|�5@o�V11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AʾwAʺ^A�ƨA�ȴA�ȴA�ƨA�ƨA�ƨA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A��
A��
A��A��#A��/A��;A��
A�ĜAʺ^AʓuA� �A��`Aȴ9Aȇ+A���A�"�A�VA�AǃA�$�A�bA��`AƶFA�C�A��AđhAÇ+A�1A�`BA��\A��A�t�A�;dA���A��A�(�A��PA�{A��A��A��TA��TA�I�A��A��mA���A�/A�O�A� �A�l�A�|�A�/A�=qA��
A�^5A��A�Q�A�
=A�O�A���A�ffA���A��A�x�A�Q�A���A��wA���A��A��\A�jA���A���A���A��;A�hsA�G�A��yA��PA��hA��A��PA�\)A}O�A{"�Aw��At�\Ar �An�9Am�Aj��Af��Ae��Ad��Ad$�A^  AY`BAXffAW/ARZAO+AK/AG��AD�yACƨACS�ABA�A?K�A=�A=`BA;�#A:JA8�A7��A7dZA6-A3G�A1A/��A.^5A-K�A,��A+�A*�!A*A(��A'�hA%�
A#��A"�!A" �A!��A!dZA ��A�mA�FA�A��AĜAv�A�`A  AVAȴA1A��A��A�PA��AJAAA�FA�A�jA��A&�A�A	�mA	�An�Ax�AM�Ax�A�+AJA�hA5?AS�A b@�M�@�  @��@���@��H@�M�@�hs@���@�E�@�7@���@�hs@��@�r�@�ff@��@���@�P@�V@���@���@�  @��y@�+@�@��@��
@߮@ߕ�@�
=@�5?@�`B@�%@��m@�@ڇ+@�J@ج@�(�@�|�@�-@թ�@Ցh@�&�@�Q�@� �@�S�@�/@��/@��`@Ϯ@�l�@�|�@��m@ϥ�@���@�-@���@͡�@���@�I�@��
@ˮ@˝�@�C�@�
=@ʧ�@ɑh@ȼj@�9X@��
@Ǖ�@�33@Ƨ�@�5?@���@�  @î@��@��@��@�ff@�V@�5?@��j@��
@��!@��T@��@�O�@���@�j@��@�o@���@�~�@���@��y@�l�@�(�@��@��m@�ƨ@�|�@�C�@�n�@�X@��@��9@�j@��w@��@���@�ff@�@�G�@�%@��@��D@� �@��m@�;d@��@�~�@�V@�$�@��h@�X@�?}@��@��/@�bN@��;@��w@��@�C�@�"�@�@�ȴ@�V@��-@��@�X@�?}@�%@��@�ƨ@�|�@�l�@�"�@���@�ff@�V@���@���@�O�@�7L@���@�V@���@��/@�@|�5@o�V11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	YB	ZB	ZB	[#B	ZB	XB	XB	XB	XB	YB	YB	YB	YB	ZB	ZB	ZB	YB	YB	[#B	]/B	[#B	[#B	[#B	[#B	ZB	[#B	[#B	[#B	]/B	iyB	n�B	�1B	ĜB
A�B
S�B
ffB
�B
��B
�B
�B
�B
�uB
�B
��B
��BBiyBy�B��B�'B��BɺB�HB��BBPB\B�B(�B5?B9XB<jB>wB@�B;dB:^B33B&�B �B�B\BB��B��B�B�mB�5B��B��B��B�FB�uBz�BM�B;dB8RB49B)�BJB
�B
��B
�^B
��B
�7B
w�B
e`B
@�B
9XB
6FB
0!B
(�B
�B
B	�B	�;B	ȴB	�XB	��B	�PB	~�B	k�B	`BB	P�B	9XB	33B	,B	"�B	B�B�sB�;B��B��B�9B��B��B��B��B��B��B�PB�DB�JB�JB�DB�DB�7B�1B�hB��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�'B�'B�-B�FB�FB�?B�9B�FB�9B�RB�jB�wB��BƨB��B��B��B��BȴB��B�B�/B�)B�;B�;B�B�;B�HB�sB�fB�HB�TB�ZB�ZB�TB�HB�B��BȴBB�dB�9B�'B�-B�-B�9B�-B�3B�3B�wBĜBŢBȴB��B��B��B�B�B�BB�fB�sB�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B	  B	+B	
=B	PB	JB	\B	bB	PB	1B	
=B	�B	�B	�B	�B	 �B	#�B	&�B	'�B	)�B	+B	.B	1'B	49B	49B	49B	5?B	5?B	5?B	7LB	8RB	:^B	<jB	=qB	?}B	?}B	=qB	=qB	@�B	A�B	G�B	K�B	Q�B	S�B	W
B	YB	YB	ZB	\)B	[#B	[#B	[#B	ZB	[#B	]/B	^5B	^5B	^5B	dZB	gmB	l�B	v�B	w�B	y�B	z�B	{�B	|�B	�B	�%B	�%B	�+B	�+B	�%B	�+B	�DB	�JB	�JB	�hB	�uB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�'B	�-B	�3B	�9B	�XB	�dB	�jB	�qB	�qB	�qB	�qB	�jB	�jB	�wB	�wB	�}B	�}B	�}B	��B	��B	ĜB	ĜB	ĜB	ĜB	ǮB	��B	��B	��B
!B
1�B
>]22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222B	YB	ZB	ZB	[#B	ZB	XB	XB	XB	XB	YB	YB	YB	YB	ZB	ZB	ZB	YB	YB	[#B	]/B	[#B	[#B	[#B	[#B	ZB	[#B	[#B	[#B	]/B	iyB	n�B	�1B	ĜB
A�B
S�B
ffB
�B
��B
�B
�B
�B
�uB
�B
��B
��BBiyBy�B��B�'B��BɺB�HB��BBPB\B�B(�B5?B9XB<jB>wB@�B;dB:^B33B&�B �B�B\BB��B��B�B�mB�5B��B��B��B�FB�uBz�BM�B;dB8RB49B)�BJB
�B
��B
�^B
��B
�7B
w�B
e`B
@�B
9XB
6FB
0!B
(�B
�B
B	�B	�;B	ȴB	�XB	��B	�PB	~�B	k�B	`BB	P�B	9XB	33B	,B	"�B	B�B�sB�;B��B��B�9B��B��B��B��B��B��B�PB�DB�JB�JB�DB�DB�7B�1B�hB��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�'B�'B�-B�FB�FB�?B�9B�FB�9B�RB�jB�wB��BƨB��B��B��B��BȴB��B�B�/B�)B�;B�;B�B�;B�HB�sB�fB�HB�TB�ZB�ZB�TB�HB�B��BȴBB�dB�9B�'B�-B�-B�9B�-B�3B�3B�wBĜBŢBȴB��B��B��B�B�B�BB�fB�sB�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B	  B	+B	
=B	PB	JB	\B	bB	PB	1B	
=B	�B	�B	�B	�B	 �B	#�B	&�B	'�B	)�B	+B	.B	1'B	49B	49B	49B	5?B	5?B	5?B	7LB	8RB	:^B	<jB	=qB	?}B	?}B	=qB	=qB	@�B	A�B	G�B	K�B	Q�B	S�B	W
B	YB	YB	ZB	\)B	[#B	[#B	[#B	ZB	[#B	]/B	^5B	^5B	^5B	dZB	gmB	l�B	v�B	w�B	y�B	z�B	{�B	|�B	�B	�%B	�%B	�+B	�+B	�%B	�+B	�DB	�JB	�JB	�hB	�uB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�'B	�-B	�3B	�9B	�XB	�dB	�jB	�qB	�qB	�qB	�qB	�jB	�jB	�wB	�wB	�}B	�}B	�}B	��B	��B	ĜB	ĜB	ĜB	ĜB	ǮB	��B	��B	��B
!B
1�B
>]22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.18 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190557                              AO  ARCAADJP                                                                    20181005190557    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190557  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190557  QCF$                G�O�G�O�G�O�8000            