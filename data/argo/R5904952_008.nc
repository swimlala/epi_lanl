CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  P   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:07Z creation      
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
resolution        =���   axis      Z        @  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 P  >�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     @  @    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 P  E@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     @  F�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     @  K�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 P  Q   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     @  R`   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 P  W�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     @  X�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     @  ^0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 P  cp   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     @  d�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 P  j    PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     @  kP   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  p�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    p�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    s�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    v�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  y�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    y�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    y�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    y�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    y�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  y�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    z<   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    zL   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    zP   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         z`   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         zd   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        zh   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    zlArgo profile    3.1 1.2 19500101000000  20181005190507  20181005190507  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @ע�mD�1   @ע'҈&@3W
=p���c�C��%1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�33A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BO��BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�fC  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C1�fC4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd�Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~�C��C�  C�  C�  C��3C��3C��3C��3C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C��3C��3C�  C�  C��3C��3C��3C��3C�  C��D fD �fDfD�fD  D�fD  D� D  Dy�D  D�fDfD� D  D� D  D� D	fD	� D
  D
� D  D� D  D� DfD�fD  D� D��Dy�D��Dy�D��Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D ��D!y�D!��D"� D"��D#y�D$  D$� D$��D%y�D%��D&y�D&��D'� D(fD(� D(��D)y�D*  D*�fD+fD+� D,fD,�fD-fD-�fD.  D.� D.��D/y�D0  D0y�D1  D1� D2  D2� Dy��D�L�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@ÅAA!AAAaA��HA��HA��HA�{A��HA��HA��HA��HB p�Bp�Bp�Bp�B p�B(p�B0p�B8p�B@p�BHp�BP
>BXp�B`p�Bhp�Bpp�Bxp�B�8RB�8RB�8RB�8RB�8RB�B�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RC )C)C�C)C)C
)C)C)C)C)C)C)C)C)C)C)C )C")C$)C&)C()C*)C,)C.)C0)C2�C4)C6)C8)C:)C<)C>)C@)CB)CD)CF)CH)CJ)CL)CN)CP)CR)CT)CV)CX)CZ)C\)C^)C`)Cb)Cd5�Cf)Ch)Cj)Cl)Cn)Cp)Cr)Ct)Cv)Cx)Cz)C|)C~5�C��C�C�C�C�GC�GC�GC�GC�C�C�C�C�C�GC�GC�C�C�C�C�C�C�C�C�GC�C�C�C�C�C�C��C��C��C�C�C�C�GC�C�C�C�C��C�C�C�C�C��C�C�C�C�C�GC�GC�C�C�C�C�C�C�C�C�C�C��C�C�GC�C�C�C�C�GC�GC�GC�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��C��C��C�GC�GC�C�C�GC�GC�GC�GC�C��D pD �pDpD�pD
D�pD
D�
D
D��D
D�pDpD�
D
D�
D
D�
D	pD	�
D

D
�
D
D�
D
D�
DpD�pD
D�
D �D��D �D��D �D��D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D 
D �
D! �D!��D" �D"�
D# �D#��D$
D$�
D% �D%��D& �D&��D' �D'�
D(pD(�
D) �D)��D*
D*�pD+pD+�
D,pD,�pD-pD-�pD.
D.�
D/ �D/��D0
D0��D1
D1�
D2
D2�
Dy��D�PRD��R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��#A��/A��;A��;A���A�ƨAϓuA�\)A�1'A�7LA��TAήAͥ�A̧�A�bA��#Aʇ+A�&�A���Aɥ�AɃA�bAȴ9A�z�A�G�A�&�A���AǕ�Aǉ7A�7LA�oA���A��;A��A���AƲ-A�p�A�ZA�r�Aƛ�AƧ�Aƣ�A�&�A���Aţ�AŋDA�XA�9XA�/A�
=A��A��TA�Aď\A�1A�`BA��A²-A��A�?}A��
A��-A�p�A��#A�7LA���A���A�ĜA�;dA���A�Q�A���A��9A��`A�ƨA��A�dZA�`BA���A�Q�A��9A���A��yA���A��yA�bA� �A��+A�ƨA�$�A���A���A�~�A��A���A��#A�x�A�jA�`BA�VA�M�A�  A��-A��mA�I�A���A�K�A�\)A��A�K�A��A��TA�r�A��
A�?}A��A��yA�K�A��A�1'A�A��A���A�wA}�At�`ApJAn��AlĜAjZAh��Ae�;AdJAb��Aa�PA`-A]/AZ��AV�ATI�AQ+AP1'AN��AM"�AIC�AF�AD�AB=qA?��A>1'A<~�A;|�A:�A:�!A:�+A9�TA97LA8bA4�jA2�A1+A/�-A-��A-+A,n�A*ĜA)�A)&�A(ĜA(�DA(bA&�/A#\)A!��A!\)A!/A M�A/A-A�7A�HA&�AE�AO�A+A�A�FA�!A(�A��AA�AJA�A�hA�A\)AK�A��A�PAdZA
��A	|�At�At�A-A  A��A&�A�A-A��A�FAdZA �A �u@���@�/@��R@�@���@�/@ߍP@ݙ�@�
=@��@�j@؃@���@��@�A�@�~�@�-@�n�@���@��@ӍP@ҧ�@�V@��@�@Ѳ-@щ7@�Q�@͑h@˅@��@ə�@�p�@�O�@�/@ȃ@���@Ǖ�@�I�@�5?@�?}@�Z@��@���@��P@�\)@�C�@��@���@�M�@��@�|�@��\@�E�@��7@�Ĝ@�bN@�9X@���@�t�@��\@��T@�`B@���@��j@�j@��@��@�z�@�Q�@�b@��@��@���@���@��F@�  @�ƨ@�ƨ@��@�v�@���@��u@�1@�dZ@���@���@�-@�5?@��!@��@��@�33@�K�@��@���@�ff@�M�@�n�@�E�@��T@�O�@��j@�bN@�(�@�1@��P@�K�@�
=@��H@��y@��@��!@��+@�@�O�@��9@�Ĝ@�PH@��@n�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��#A��/A��;A��;A���A�ƨAϓuA�\)A�1'A�7LA��TAήAͥ�A̧�A�bA��#Aʇ+A�&�A���Aɥ�AɃA�bAȴ9A�z�A�G�A�&�A���AǕ�Aǉ7A�7LA�oA���A��;A��A���AƲ-A�p�A�ZA�r�Aƛ�AƧ�Aƣ�A�&�A���Aţ�AŋDA�XA�9XA�/A�
=A��A��TA�Aď\A�1A�`BA��A²-A��A�?}A��
A��-A�p�A��#A�7LA���A���A�ĜA�;dA���A�Q�A���A��9A��`A�ƨA��A�dZA�`BA���A�Q�A��9A���A��yA���A��yA�bA� �A��+A�ƨA�$�A���A���A�~�A��A���A��#A�x�A�jA�`BA�VA�M�A�  A��-A��mA�I�A���A�K�A�\)A��A�K�A��A��TA�r�A��
A�?}A��A��yA�K�A��A�1'A�A��A���A�wA}�At�`ApJAn��AlĜAjZAh��Ae�;AdJAb��Aa�PA`-A]/AZ��AV�ATI�AQ+AP1'AN��AM"�AIC�AF�AD�AB=qA?��A>1'A<~�A;|�A:�A:�!A:�+A9�TA97LA8bA4�jA2�A1+A/�-A-��A-+A,n�A*ĜA)�A)&�A(ĜA(�DA(bA&�/A#\)A!��A!\)A!/A M�A/A-A�7A�HA&�AE�AO�A+A�A�FA�!A(�A��AA�AJA�A�hA�A\)AK�A��A�PAdZA
��A	|�At�At�A-A  A��A&�A�A-A��A�FAdZA �A �u@���@�/@��R@�@���@�/@ߍP@ݙ�@�
=@��@�j@؃@���@��@�A�@�~�@�-@�n�@���@��@ӍP@ҧ�@�V@��@�@Ѳ-@щ7@�Q�@͑h@˅@��@ə�@�p�@�O�@�/@ȃ@���@Ǖ�@�I�@�5?@�?}@�Z@��@���@��P@�\)@�C�@��@���@�M�@��@�|�@��\@�E�@��7@�Ĝ@�bN@�9X@���@�t�@��\@��T@�`B@���@��j@�j@��@��@�z�@�Q�@�b@��@��@���@���@��F@�  @�ƨ@�ƨ@��@�v�@���@��u@�1@�dZ@���@���@�-@�5?@��!@��@��@�33@�K�@��@���@�ff@�M�@�n�@�E�@��T@�O�@��j@�bN@�(�@�1@��P@�K�@�
=@��H@��y@��@��!@��+@�@�O�@��9@�Ĝ@�PH@��@n�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
ɺB
ɺB
ɺB
ȴB
ȴB
ǮB
ŢB
ÖB
��B
��B
�qB
�dB
�dB
�}B
ĜB
��B
��B
��B
�ZB
�B
�B
��B
��BBBBBBB
��B
��BBB  B  B
��B  B1B�B0!B9XB=qB9XB33B49B8RB7LBB�BW
BgmBw�B�B�{B��B�-B��B�B�`B��BVB{B�B�B�B$�B)�B/BA�BN�BR�BR�BT�BT�B\)Bm�Bu�B�+B�JB�DB�PB�JB�+B�Bx�Bu�By�Bz�Bu�Bm�B^5BS�BE�B6FB1'B.B33B.B!�BhB��B��B�B��B�7Bn�BR�B1'B
��B
�ZB
�B
��B
�}B
�LB
�B
��B
��B
��B
�=B
t�B
W
B
F�B
6FB
"�B
{B
B	ȴB	��B	��B	�bB	�B	x�B	jB	bNB	]/B	T�B	L�B	>wB	1'B	 �B	�B	
=B	%B��B��B�B�ZB�;B�B�B��B��B��B��B��B��B��BȴBŢBŢBȴB��B��B��B��B��B��B��B��B��B��BɺBƨB�}B�dB�^B�RB�9B�B��B��B��B��B��B��B��B��BƨB��B�jB�dBÖBȴB�9B��B��B��B�B�FB�RB�LB�3B�B�B��B�!B�3B�?B�3B�!B�B�B��B��B��B��B��B��B��B��B��BL�B�qB�jB�qB�qB�}B��BĜBƨBŢB��BĜB��B��B��B��B��BɺB��B��B��B�B�)B�;B�#B�/B�NB�TB�ZB�`B�`B�ZB�TB�sB�B�B��B��B��B��B��B��B��B	B	B	B	1B	\B	oB	oB	bB	oB	{B	�B	�B	"�B	&�B	'�B	)�B	-B	49B	5?B	9XB	:^B	:^B	;dB	?}B	A�B	A�B	A�B	E�B	N�B	P�B	S�B	T�B	S�B	VB	W
B	VB	VB	VB	XB	]/B	`BB	e`B	gmB	hsB	o�B	s�B	w�B	{�B	{�B	�B	�DB	�JB	�PB	�PB	�PB	�VB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B
�B
�B
+222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222442222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222B
ɺB
ɺB
ɺB
ȴB
ȴB
ǮB
ŢB
ÖB
��B
��B
�qB
�dB
�dB
�}B
ĜB
��B
��B
��B
�ZB
�B
�B
��B
��BBBBBBB
��B
��BBB  B  B
��B  B1B�B0!B9XB=qB9XB33B49B8RB7LBB�BW
BgmBw�B�B�{B��B�-B��B�B�`B��BVB{B�B�B�B$�B)�B/BA�BN�BR�BR�BT�BT�B\)Bm�Bu�B�+B�JB�DB�PB�JB�+B�Bx�Bu�By�Bz�Bu�Bm�B^5BS�BE�B6FB1'B.B33B.B!�BhB��B��B�B��B�7Bn�BR�B1'B
��B
�ZB
�B
��B
�}B
�LB
�B
��B
��B
��B
�=B
t�B
W
B
F�B
6FB
"�B
{B
B	ȴB	��B	��B	�bB	�B	x�B	jB	bNB	]/B	T�B	L�B	>wB	1'B	 �B	�B	
=B	%B��B��B�B�ZB�;B�B�B��B��B��B��B��B��B��BȴBŢBŢBȴB��B��B��B��B��B��B��B��B��B��BɺBƨB�}B�dB�^B�RB�9B�B��B��B��B��B��B��B��B��BƨB��B�jB�dBÖBȴB�9B��B��B��B�B�FB�RB�LB�3B�B�B��B�!B�3B�?B�3B�!B�B�B��B��B��B��B��B��B��B��B��BL�B�qB�jB�qB�qB�}B��BĜBƨBŢB��BĜB��B��B��B��B��BɺB��B��B��B�B�)B�;B�#B�/B�NB�TB�ZB�`B�`B�ZB�TB�sB�B�B��B��B��B��B��B��B��B	B	B	B	1B	\B	oB	oB	bB	oB	{B	�B	�B	"�B	&�B	'�B	)�B	-B	49B	5?B	9XB	:^B	:^B	;dB	?}B	A�B	A�B	A�B	E�B	N�B	P�B	S�B	T�B	S�B	VB	W
B	VB	VB	VB	XB	]/B	`BB	e`B	gmB	hsB	o�B	s�B	w�B	{�B	{�B	�B	�DB	�JB	�PB	�PB	�PB	�VB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B
�B
�B
+222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222442222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.11 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190507                              AO  ARCAADJP                                                                    20181005190507    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190507  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190507  QCF$                G�O�G�O�G�O�C000            