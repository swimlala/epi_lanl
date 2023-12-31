CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  `   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:19Z creation      
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
_FillValue                 `  >�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  @P   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 `  E�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  G0   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  L�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 `  R0   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  S�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 `  Y   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Zp   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 `  ep   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  f�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 `  lP   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  m�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  s0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    s`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    v`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    y`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  |`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    |�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    |�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    |�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    |�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  |�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    |�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    |�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    |�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         }    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         }   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        }   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    }Argo profile    3.1 1.2 19500101000000  20181005190519  20181005190519  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               AA   AO  6431                            2B  A   APEX                            7466                            062512                          846 @׿��U�1   @׿�`��@1f$�/��c~��O�;1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      AA   A   A   @�  @�  A   A   A>ffA^ffA�  A���A�  A�  A���A���A�  A�  B   B  B  BffB   B(  B0  B8  B?��BG��BP  BX  B`  Bh  BpffBx  B�  B�  B���B���B�  B�  B�33B�  B�  B�  B�  B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C1�fC3�fC5�fC8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl�Cn  Cp  Cr  Ct�Cv  Cx  Cz  C|�C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C��3C�  C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C��C��C��C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� DfD� D��D	� D
  D
� DfD� D��D� D  D� DfD� D  D� D  Dy�D  D� DfD� D��D� D  D� D  D� D  D� D  D� D  D�fDfD� D  Dy�D  D�fD  Dy�D  D� D  D� DfD�fD   D � D!  D!� D!��D"� D#  D#�fD$  D$� D%  D%� D%��D&� D'fD'� D(  D(�fD)  D)� D*  D*� D+  D+�fD,  D,y�D-  D-� D.  D.�fD/fD/� D03D0Ff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�{@�{A
=A#
=AAp�Aap�A��A�Q�A��A��A�Q�A�Q�A�A�B BBB(�B B(B0B8B@\)BH\)BPBXB`BhBq(�BxB�aHB�aHB�.B�.B�aHB�aHB��{B�aHB�aHB�aHB�aHB�ǮB���B�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�.B�aHB�aHB�aHB�aHB�aHC 0�C0�C0�C0�C0�C
0�C0�C0�C0�C0�C0�C0�C0�C0�C0�C0�C 0�C"0�C$0�C&0�C(0�C*0�C,0�C.0�C00�C2
C4
C6
C80�C:0�C<0�C>0�C@0�CB0�CD0�CF0�CH0�CJ0�CL0�CN0�CP0�CR0�CT0�CV0�CX0�CZ0�C\0�C^0�C`0�Cb0�Cd0�Cf0�Ch0�Cj0�ClJ>Cn0�Cp0�Cr0�CtJ>Cv0�Cx0�Cz0�C|J>C~0�C�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�%C�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC��C��C��C��C��C�RC�RC�RC�RC�RC�RC�RC�%C�%C�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC��C�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�%C�RC�RC�%C�RC�RC�RC�RC�RC�RC�%C�RC�RC�RC�RC�RC�RC�RC�RC�RC��C�RC�RC�RC�RC�%C�RC��C�RC�RC��C�RC�RC�RC�RC��C�RC�RC�RC�RC�RC�RC�RC�%C�%C�%C�%C�RC�%C�%C�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RD )D �)D)D��D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D�D�)D	�D	�)D
)D
�)D�D�)D�D�)D)D�)D�D�)D)D�)D)D��D)D�)D�D�)D�D�)D)D�)D)D�)D)D�)D)D�)D)D��D�D�)D)D��D)D��D)D��D)D�)D)D�)D�D��D )D �)D!)D!�)D"�D"�)D#)D#��D$)D$�)D%)D%�)D&�D&�)D'�D'�)D()D(��D))D)�)D*)D*�)D+)D+��D,)D,��D-)D-�)D.)D.��D/�D/�)D0\D0R�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A� �A�"�A�-A�+A�-A�/A�;dA�9XA�=qA�=qA�9XA�9XA�9XA�=qA�?}A�A�A�C�A�C�A�=qA�C�A�A�A�=qA�A��A�O�A�oAԬAӍPA��A���A�oA�z�AЅA�9XA�\)AЁA�jA���A��A���A̛�A��AȲ-A��Aǡ�A�A�AƧ�AŁA�
=A�K�A�\)A���A�5?A�|�A�bA��/A���A���A�XA���A�O�A���A�\)A�t�A��HA�l�A��A��PA���A���A���A��hA��HA�A��PA�Q�A�VA��jA��A��!A�O�A�-A���A��A�M�A�E�A���A���A�E�A���A�?}A�bA��TA�A�A�/A���A�&�A��\A��^A�A�A���A�1A�^5A��9A��HA�XA� �A��A�{A��yA�/A|�AyO�AvA�Ar�/Ao��Am��Ak�7Ai�Af  Ab��A^I�A\�DA[G�AZ(�AXAT��AP�yAO"�AMdZAH��AD��A?�A<~�A:�jA9��A8v�A5��A3�hA1S�A.�!A-�A,�!A+��A+G�A+�A*v�A)�7A)hsA(�uA'`BA%��A%�A$ZA#O�A"(�A!O�AĜA�A�A�jA��A^5AA�A-AK�A�A��A�-A�9A{AG�A�TA"�A�+A  A��A?}A��A$�A�AC�A�`A�A|�A�A
bA	��A	VA��A\)A�A`BA�HA�DAE�A�A�AA�A �@���@��TA ��AXA ��@�p�@�33@�bN@�j@� �@�
=@��^@�J@��@�+@�
=@�M�@�=q@�ff@�v�@�ff@�|�@��`@���@���@�z�@��@��m@�+@�o@�R@�E�@��@�^@�@�O�@�X@�V@�V@�7L@�X@� �@�P@�^@�j@��m@��@�Q�@�@�ȴ@���@�V@݁@�@�`B@��/@�Z@�|�@ڸR@�=q@�x�@�I�@ם�@�7L@�J@�Z@��@�J@���@�33@�1'@�Z@�I�@Լj@���@�Q�@��@��
@�;d@�-@ѡ�@���@д9@϶F@�"�@���@�v�@���@́@�X@�Z@��
@�C�@�ȴ@���@���@ʗ�@�5?@��T@ə�@�V@�b@ư!@�V@�`B@�Q�@� �@Ý�@¸R@�ff@���@��@� �@���@��@���@��7@�=q@�M�@�@�%@��P@�v�@�G�@��/@���@�l�@�o@��@��H@�@��@�  @��j@��m@�^5@���@�5?@�hs@�`B@�%@���@�Z@� �@��
@�+@�dZ@���@�l�@�dZ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A� �A�"�A�-A�+A�-A�/A�;dA�9XA�=qA�=qA�9XA�9XA�9XA�=qA�?}A�A�A�C�A�C�A�=qA�C�A�A�A�=qA�A��A�O�A�oAԬAӍPA��A���A�oA�z�AЅA�9XA�\)AЁA�jA���A��A���A̛�A��AȲ-A��Aǡ�A�A�AƧ�AŁA�
=A�K�A�\)A���A�5?A�|�A�bA��/A���A���A�XA���A�O�A���A�\)A�t�A��HA�l�A��A��PA���A���A���A��hA��HA�A��PA�Q�A�VA��jA��A��!A�O�A�-A���A��A�M�A�E�A���A���A�E�A���A�?}A�bA��TA�A�A�/A���A�&�A��\A��^A�A�A���A�1A�^5A��9A��HA�XA� �A��A�{A��yA�/A|�AyO�AvA�Ar�/Ao��Am��Ak�7Ai�Af  Ab��A^I�A\�DA[G�AZ(�AXAT��AP�yAO"�AMdZAH��AD��A?�A<~�A:�jA9��A8v�A5��A3�hA1S�A.�!A-�A,�!A+��A+G�A+�A*v�A)�7A)hsA(�uA'`BA%��A%�A$ZA#O�A"(�A!O�AĜA�A�A�jA��A^5AA�A-AK�A�A��A�-A�9A{AG�A�TA"�A�+A  A��A?}A��A$�A�AC�A�`A�A|�A�A
bA	��A	VA��A\)A�A`BA�HA�DAE�A�A�AA�A �@���@��TA ��AXA ��@�p�@�33@�bN@�j@� �@�
=@��^@�J@��@�+@�
=@�M�@�=q@�ff@�v�@�ff@�|�@��`@���@���@�z�@��@��m@�+@�o@�R@�E�@��@�^@�@�O�@�X@�V@�V@�7L@�X@� �@�P@�^@�j@��m@��@�Q�@�@�ȴ@���@�V@݁@�@�`B@��/@�Z@�|�@ڸR@�=q@�x�@�I�@ם�@�7L@�J@�Z@��@�J@���@�33@�1'@�Z@�I�@Լj@���@�Q�@��@��
@�;d@�-@ѡ�@���@д9@϶F@�"�@���@�v�@���@́@�X@�Z@��
@�C�@�ȴ@���@���@ʗ�@�5?@��T@ə�@�V@�b@ư!@�V@�`B@�Q�@� �@Ý�@¸R@�ff@���@��@� �@���@��@���@��7@�=q@�M�@�@�%@��P@�v�@�G�@��/@���@�l�@�o@��@��H@�@��@�  @��j@��m@�^5@���@�5?@�hs@�`B@�%@���@�Z@� �@��
@�+@�dZ@���@�l�@�dZ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
|�B
}�B
}�B
{�B
s�B
\)B
P�B
E�B
=qB
>wB
G�B
m�B
�B
��B
�-B
��B
��B�B$�B&�B(�B0!BR�Br�B�LBȴB�
B�BB�BBDBoB�B33B>wBJ�BO�B^5Bu�B�JB��B��B�'B�XB�dB��B��B�B�B�B��BŢB�dB��B�Bo�Bn�Bl�BhsBO�B#�BB��B�sB�B��B��B�NB�)BǮB�B��B�;B�B�sB��B�jB�B�JBdZB2-B\B
�mB
��B
�B
�=B
� B
x�B
s�B
`BB
W
B
C�B
$�B

=B	�B	��B	��B	�B	�'B	�3B	��B	�{B	� B	jB	_;B	ZB	Q�B	@�B	.B	�B	hB	1B��B�HB��B��B��B��B�wB�qB�^B�FB�3B�'B�!B�!B�B�B�B�!B�B�!B�B�!B�FB�LB�9B�B�B�!B�3B�?B�XB�XB�XB�XB��BǮBǮB��BɺBÖBÖB��B��BɺBȴBǮBǮBȴB�
B��B��B�B�B�#B�#B�B��B��B��B��B��BƨBĜBB��B��BÖBÖBĜB��B�?B��B��B�B��B�B�BB�B��B��B��B��B��B�#B�yB�B�B�B�B�B�B�B�B�B�fB�TB�BB�TB�mB�B�B�B�B��B��B	  B		7B	\B	�B	�B	�B	"�B	%�B	-B	0!B	/B	-B	/B	49B	49B	)�B	&�B	+B	33B	7LB	;dB	>wB	>wB	;dB	9XB	>wB	E�B	B�B	A�B	K�B	S�B	O�B	K�B	L�B	M�B	M�B	\)B	^5B	_;B	dZB	hsB	iyB	iyB	iyB	k�B	k�B	k�B	m�B	p�B	r�B	r�B	q�B	r�B	v�B	x�B	x�B	x�B	x�B	x�B	z�B	}�B	~�B	� B	�B	�B	�B	�B	�B	|�B	{�B	|�B	|�B	}�B	~�B	~�B	}�B	}�B	}�B	}�B	}�B	}�B	}�B	� B	�7B	�VB	�\B	�PB	�hB	�bB	�VB	�VB	�\B	�\B	�oB	�hB	�oB	�oB	�\B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
|�B
}�B
}�B
{�B
s�B
\)B
P�B
E�B
=qB
>wB
G�B
m�B
�B
��B
�-B
��B
��B�B$�B&�B(�B0!BR�Br�B�LBȴB�
B�BB�BBDBoB�B33B>wBJ�BO�B^5Bu�B�JB��B��B�'B�XB�dB��B��B�B�B�B��BŢB�dB��B�Bo�Bn�Bl�BhsBO�B#�BB��B�sB�B��B��B�NB�)BǮB�B��B�;B�B�sB��B�jB�B�JBdZB2-B\B
�mB
��B
�B
�=B
� B
x�B
s�B
`BB
W
B
C�B
$�B

=B	�B	��B	��B	�B	�'B	�3B	��B	�{B	� B	jB	_;B	ZB	Q�B	@�B	.B	�B	hB	1B��B�HB��B��B��B��B�wB�qB�^B�FB�3B�'B�!B�!B�B�B�B�!B�B�!B�B�!B�FB�LB�9B�B�B�!B�3B�?B�XB�XB�XB�XB��BǮBǮB��BɺBÖBÖB��B��BɺBȴBǮBǮBȴB�
B��B��B�B�B�#B�#B�B��B��B��B��B��BƨBĜBB��B��BÖBÖBĜB��B�?B��B��B�B��B�B�BB�B��B��B��B��B��B�#B�yB�B�B�B�B�B�B�B�B�B�fB�TB�BB�TB�mB�B�B�B�B��B��B	  B		7B	\B	�B	�B	�B	"�B	%�B	-B	0!B	/B	-B	/B	49B	49B	)�B	&�B	+B	33B	7LB	;dB	>wB	>wB	;dB	9XB	>wB	E�B	B�B	A�B	K�B	S�B	O�B	K�B	L�B	M�B	M�B	\)B	^5B	_;B	dZB	hsB	iyB	iyB	iyB	k�B	k�B	k�B	m�B	p�B	r�B	r�B	q�B	r�B	v�B	x�B	x�B	x�B	x�B	x�B	z�B	}�B	~�B	� B	�B	�B	�B	�B	�B	|�B	{�B	|�B	|�B	}�B	~�B	~�B	}�B	}�B	}�B	}�B	}�B	}�B	}�B	� B	�7B	�VB	�\B	�PB	�hB	�bB	�VB	�VB	�\B	�\B	�oB	�hB	�oB	�oB	�\B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.19 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190519                              AO  ARCAADJP                                                                    20181005190519    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190519  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190519  QCF$                G�O�G�O�G�O�8000            