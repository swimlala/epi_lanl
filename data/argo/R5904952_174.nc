CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:44Z creation      
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
resolution        =���   axis      Z        T  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     T  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     T  K�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     T  S   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Zp   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     T  \H   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  c�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     T  et   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     T  l�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     T  u�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  }H   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     T      	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20181005190544  20181005190544  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @���9Yu1   @���lB@1P ě���c�O�;dZ1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�  @�  A��A   A@  A`  A�  A���A���A�ffA�  A�33A�33A�33B   B  B  B  B ffB(��B/33B8  B@  BG��BP  BW��B_��Bh  Bp  Bw��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�33C   C  C�C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C;�fC>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cy�fC|  C~  C�fC��3C��C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C��C�  C��3C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C��C��C��C��C��C�  C�  C�  C�  C�  C��3C��3C��3C��3C��3C�  C�  C��3C�  C�  C�  C�  C�  C��C�  D   D � D  D� D��Dy�D  D�fD  D� D  D� D  D� D  D� D��Dy�D	  D	y�D	��D
� DfD�fD  D� D  Dy�D  D� DfD�fD  Dy�D  D� DfD�fD  D� D  D� D  D� D  D� D  D� D  D� D  D�fD��D� D  D� D��D� D  D� D  D�fD  D� D   D � D!  D!� D"  D"�fD#  D#y�D$  D$� D$��D%� D&  D&�fD'  D'y�D'��D(y�D)  D)� D*  D*� D+  D+� D+��D,� D-fD-�fD.  D.y�D/  D/� D0  D0� D1  D1� D2  D2� D2��D3� D4fD4� D5  D5� D6  D6y�D7fD7� D7��D8� D9  D9� D:  D:� D;  D;� D<  D<y�D=  D=� D=��D>� D?  D?y�D@  D@�fDAfDA�fDBfDB� DC  DCy�DD  DD�fDE  DEy�DE��DF� DG  DG� DHfDH� DI  DI�fDJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQy�DR  DR� DS  DS� DS��DT� DUfDU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� DZ��D[� D\  D\� D]  D]� D^  D^� D_  D_� D_��D`� DafDa� Db  Db� Dc  Dc� Dc��Ddy�De  De� DffDf�fDg  Dgy�Dh  Dh� Di  Di�fDy��D�C3D�θ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��A(�A"�\AB�\Ab�\A�G�A�{A��HA��A�G�A�z�A�z�A�z�B ��B��B��B��B!
=B)p�B/�
B8��B@��BH=qBP��BX=qB`=qBh��Bp��Bx=qB�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B܅B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B��C (�C(�CB�C(�C(�C
(�C(�C(�C(�C(�C(�C(�C(�C(�C(�C(�C (�C"(�C$(�C&(�C((�C*(�C,(�C.(�C0(�C2(�C4(�C6(�C8(�C:(�C<\C>(�C@(�CB(�CD(�CF(�CH(�CJ(�CL(�CN(�CP(�CR(�CT(�CV(�CX(�CZ(�C\(�C^(�C`(�Cb(�Cd(�Cf(�Ch(�Cj(�Cl(�Cn(�Cp(�Cr(�Ct(�Cv(�Cx(�Cz\C|(�C~(�C��C��C�!HC�!HC�{C�{C�{C�{C�!HC�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�!HC�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C��C��C�{C�!HC�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C��C�{C�!HC�!HC�{C��C�{C�{C�{C�{C�{C��C��C�{C�{C�{C�{C�{C�!HC�{C�{C�{C�{C�{C�!HC�!HC�!HC�!HC�!HC�!HC�{C�{C�{C�{C�{C��C��C��C��C��C�{C�{C��C�{C�{C�{C�{C�{C�!HC�{D 
=D �=D
=D�=D�D��D
=D��D
=D�=D
=D�=D
=D�=D
=D�=D�D��D	
=D	��D
�D
�=D�D��D
=D�=D
=D��D
=D�=D�D��D
=D��D
=D�=D�D��D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D��D�D�=D
=D�=D�D�=D
=D�=D
=D��D
=D�=D 
=D �=D!
=D!�=D"
=D"��D#
=D#��D$
=D$�=D%�D%�=D&
=D&��D'
=D'��D(�D(��D)
=D)�=D*
=D*�=D+
=D+�=D,�D,�=D-�D-��D.
=D.��D/
=D/�=D0
=D0�=D1
=D1�=D2
=D2�=D3�D3�=D4�D4�=D5
=D5�=D6
=D6��D7�D7�=D8�D8�=D9
=D9�=D:
=D:�=D;
=D;�=D<
=D<��D=
=D=�=D>�D>�=D?
=D?��D@
=D@��DA�DA��DB�DB�=DC
=DC��DD
=DD��DE
=DE��DF�DF�=DG
=DG�=DH�DH�=DI
=DI��DJ
=DJ�=DK
=DK�=DL
=DL�=DM
=DM�=DN
=DN�=DO
=DO�=DP
=DP�=DQ
=DQ��DR
=DR�=DS
=DS�=DT�DT�=DU�DU�=DV
=DV�=DW
=DW�=DX
=DX�=DY
=DY�=DZ
=DZ�=D[�D[�=D\
=D\�=D]
=D]�=D^
=D^�=D_
=D_�=D`�D`�=Da�Da�=Db
=Db�=Dc
=Dc�=Dd�Dd��De
=De�=Df�Df��Dg
=Dg��Dh
=Dh�=Di
=Di��Dy�D�HRD���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A� �A�&�A�+A�&�A�"�A�(�A�-A�+A�+A�+A�+A�7LA�33A�5?A�33A�7LA�1'A�33A�33A�9XA�5?A�S�A�K�A�Q�A�XA�O�A�^5A͝�A�/A�"�AυA�^5A�M�A�E�A�33A�+A���A�ȴAάA΃A�1'A�bA�(�A�(�A�&�A�t�A�r�Aƕ�A��A�bNA�ȴA�A���A�O�A�jA��-A�  A�O�A�`BA�ƨA�VA���A�jA��
A��DA��A�XA�ȴA�^5A��FA�;dA�G�A�ƨA�bA�C�A���A�-A���A���A��uA��/A�A�A�
=A���A���A�G�A��A��DA�{A��FA���A�oA�^5A���A�\)A�bA�r�A���A��#A��HA�$�A�1'A�JA�/A��
A�K�A��PA��/A��DA�dZA~�9A{�#Az��Ax�/Aw|�AvJAo�Ag�;Af�+Ab�yA_A[;dAV�AS�wAQ�AQVAPI�AN�AKhsAJ=qAIXAI"�AG�ADȴAC��AA��A@M�A=�PA:(�A7O�A4ĜA1p�A.�A-��A+�mA*��A)|�A(r�A&��A$�A$5?A$ �A#�A#t�A"�!A ��A�TA��Ap�A/A�yA��A�HAhsA�jA(�AG�A�;A�A?}A�uA�A�^AA�jAffAK�AM�A�A
�uA	��A	�AVA��A�RA��A�A�RAv�An�AVA�-A�A�+A%A ��A jA �\@���@���@���@�K�@�?}@��m@���@�bN@�C�@�
=@�;d@�"�@��@홚@�1'@�!@�^@�@��m@��@��@�/@�|�@��@��u@�ƨ@�-@ݺ^@��@�  @��@�E�@١�@ج@�z�@�z�@� �@��;@�;d@�v�@Ձ@��@���@ҸR@��@�=q@�J@�p�@��`@д9@�  @�dZ@��y@�n�@�`B@�b@��;@˾w@˾w@���@ˮ@��y@���@ɲ-@ɩ�@ɑh@Ɂ@�O�@���@���@ȓu@�9X@�  @��m@�33@�~�@��@�G�@�9X@�1@��@��
@öF@Ý�@�t�@��y@�ȴ@���@°!@§�@�-@��@�X@��@�Z@��
@��F@��@���@�l�@�@�E�@���@�V@���@�9X@�b@��m@�33@��H@��R@��+@��@���@��@�j@�  @��@���@�G�@�Z@�(�@�  @�S�@�33@�"�@�@���@���@��@�X@��@���@���@�Q�@��@�S�@���@�-@��h@�X@�hs@���@���@�7L@��@��@��@��R@���@���@�=q@�O�@��@��u@��D@��@�Z@�Q�@��@��@���@��w@���@�K�@�K�@�;d@�33@�+@��H@���@���@��+@�V@��@���@��@��;@�\)@��@���@�{@���@�x�@�z�@�ƨ@�;d@�+@��@�v�@�E�@�$�@��@�x�@�?}@��@��u@�I�@� �@�  @��P@�;d@�
=@��@���@�{@�x�@�/@��j@�Q�@��@�ƨ@�Z@���@��@��@�Z@�  @��@�\)@��@��y@��!@�ff@�E�@�@��-@��h@�p�@���@���@�Q�@�  @��
@���@���@�|�@�S�@���@�V@�5?@�@��T@��7@�/@�V@���@��9@�Q�@��;@���@�C�@��@��@��!@��+@�ff@�=q@���@�`B@�X@�G�@�G�@�%@��/@���@��@�(�@��@}��@jq�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A� �A�&�A�+A�&�A�"�A�(�A�-A�+A�+A�+A�+A�7LA�33A�5?A�33A�7LA�1'A�33A�33A�9XA�5?A�S�A�K�A�Q�A�XA�O�A�^5A͝�A�/A�"�AυA�^5A�M�A�E�A�33A�+A���A�ȴAάA΃A�1'A�bA�(�A�(�A�&�A�t�A�r�Aƕ�A��A�bNA�ȴA�A���A�O�A�jA��-A�  A�O�A�`BA�ƨA�VA���A�jA��
A��DA��A�XA�ȴA�^5A��FA�;dA�G�A�ƨA�bA�C�A���A�-A���A���A��uA��/A�A�A�
=A���A���A�G�A��A��DA�{A��FA���A�oA�^5A���A�\)A�bA�r�A���A��#A��HA�$�A�1'A�JA�/A��
A�K�A��PA��/A��DA�dZA~�9A{�#Az��Ax�/Aw|�AvJAo�Ag�;Af�+Ab�yA_A[;dAV�AS�wAQ�AQVAPI�AN�AKhsAJ=qAIXAI"�AG�ADȴAC��AA��A@M�A=�PA:(�A7O�A4ĜA1p�A.�A-��A+�mA*��A)|�A(r�A&��A$�A$5?A$ �A#�A#t�A"�!A ��A�TA��Ap�A/A�yA��A�HAhsA�jA(�AG�A�;A�A?}A�uA�A�^AA�jAffAK�AM�A�A
�uA	��A	�AVA��A�RA��A�A�RAv�An�AVA�-A�A�+A%A ��A jA �\@���@���@���@�K�@�?}@��m@���@�bN@�C�@�
=@�;d@�"�@��@홚@�1'@�!@�^@�@��m@��@��@�/@�|�@��@��u@�ƨ@�-@ݺ^@��@�  @��@�E�@١�@ج@�z�@�z�@� �@��;@�;d@�v�@Ձ@��@���@ҸR@��@�=q@�J@�p�@��`@д9@�  @�dZ@��y@�n�@�`B@�b@��;@˾w@˾w@���@ˮ@��y@���@ɲ-@ɩ�@ɑh@Ɂ@�O�@���@���@ȓu@�9X@�  @��m@�33@�~�@��@�G�@�9X@�1@��@��
@öF@Ý�@�t�@��y@�ȴ@���@°!@§�@�-@��@�X@��@�Z@��
@��F@��@���@�l�@�@�E�@���@�V@���@�9X@�b@��m@�33@��H@��R@��+@��@���@��@�j@�  @��@���@�G�@�Z@�(�@�  @�S�@�33@�"�@�@���@���@��@�X@��@���@���@�Q�@��@�S�@���@�-@��h@�X@�hs@���@���@�7L@��@��@��@��R@���@���@�=q@�O�@��@��u@��D@��@�Z@�Q�@��@��@���@��w@���@�K�@�K�@�;d@�33@�+@��H@���@���@��+@�V@��@���@��@��;@�\)@��@���@�{@���@�x�@�z�@�ƨ@�;d@�+@��@�v�@�E�@�$�@��@�x�@�?}@��@��u@�I�@� �@�  @��P@�;d@�
=@��@���@�{@�x�@�/@��j@�Q�@��@�ƨ@�Z@���@��@��@�Z@�  @��@�\)@��@��y@��!@�ff@�E�@�@��-@��h@�p�@���@���@�Q�@�  @��
@���@���@�|�@�S�@���@�V@�5?@�@��T@��7@�/@�V@���@��9@�Q�@��;@���@�C�@��@��@��!@��+@�ff@�=q@���@�`B@�X@�G�@�G�@�%@��/@���@��@�(�@��@}��@jq�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�dB�dB�dB�dB�dB�dB�jB�jB�dB�dB�dB�dB�XB�^B�^B�dB�RB�RB�RB�XB�XBƨB��BÖBƨBB��B�B	L�B
{B
��B
�NB
�B
��B\B�BB�BL�BN�BQ�BVBXBdZBl�B�B�B�hB��B��B��B��B��B��B�B�RB��B��B�sB�B�B��BDB�B#�B6FB9XB<jB=qB>wBC�BG�BD�B@�B<jB9XB6FB6FB<jB<jB7LB2-B(�B�BVB��B�B�ZB�`B�TB�BB��B��Bs�BS�BR�BO�BG�B=qB2-B"�BB
ǮB
��B
� B
iyB
_;B
O�B
2-B
B	�B	ŢB	�FB	�B	��B	��B	�7B	[#B	/B	$�B	uB��B�B�BȴB��B�}B�jB�^B��BÖB��B�wB�XB�9B�-B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B��B�B�9B�^B�qB�jB��BÖB��BȴB��B��B��B��B��B��BɺBǮBƨB��B��B��BɺB��B��B��B��B��B��B��B��B�B�B�B�B�)B�)B�5B�5B�BB�NB�TB�fB�B�B�B�B�B�B��B��B��B��B	B	B	B	
=B	DB	\B	oB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	"�B	#�B	%�B	&�B	(�B	)�B	+B	+B	-B	1'B	1'B	1'B	1'B	2-B	33B	5?B	6FB	7LB	8RB	9XB	8RB	>wB	C�B	F�B	F�B	M�B	O�B	O�B	P�B	R�B	VB	XB	YB	YB	ZB	ZB	\)B	]/B	aHB	bNB	dZB	hsB	k�B	l�B	l�B	m�B	n�B	q�B	t�B	x�B	|�B	~�B	�B	�B	�B	�B	�B	�B	�%B	�%B	�JB	�VB	�\B	�VB	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�9B	�?B	�?B	�?B	�?B	�FB	�LB	�RB	�RB	�RB	�XB	�dB	�wB	�}B	��B	��B	ÖB	ŢB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�B	�B	�B	�B	�5B	�5B	�;B	�;B	�HB	�BB	�BB	�;B	�;B	�HB	�TB	�TB	�ZB	�`B	�`B	�mB	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
�B
&�B
/2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B�dB�dB�dB�dB�dB�dB�jB�jB�dB�dB�dB�dB�XB�^B�^B�dB�RB�RB�RB�XB�XBƨB��BÖBƨBB��B�B	L�B
{B
��B
�NB
�B
��B\B�BB�BL�BN�BQ�BVBXBdZBl�B�B�B�hB��B��B��B��B��B��B�B�RB��B��B�sB�B�B��BDB�B#�B6FB9XB<jB=qB>wBC�BG�BD�B@�B<jB9XB6FB6FB<jB<jB7LB2-B(�B�BVB��B�B�ZB�`B�TB�BB��B��Bs�BS�BR�BO�BG�B=qB2-B"�BB
ǮB
��B
� B
iyB
_;B
O�B
2-B
B	�B	ŢB	�FB	�B	��B	��B	�7B	[#B	/B	$�B	uB��B�B�BȴB��B�}B�jB�^B��BÖB��B�wB�XB�9B�-B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B��B�B�9B�^B�qB�jB��BÖB��BȴB��B��B��B��B��B��BɺBǮBƨB��B��B��BɺB��B��B��B��B��B��B��B��B�B�B�B�B�)B�)B�5B�5B�BB�NB�TB�fB�B�B�B�B�B�B��B��B��B��B	B	B	B	
=B	DB	\B	oB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	"�B	#�B	%�B	&�B	(�B	)�B	+B	+B	-B	1'B	1'B	1'B	1'B	2-B	33B	5?B	6FB	7LB	8RB	9XB	8RB	>wB	C�B	F�B	F�B	M�B	O�B	O�B	P�B	R�B	VB	XB	YB	YB	ZB	ZB	\)B	]/B	aHB	bNB	dZB	hsB	k�B	l�B	l�B	m�B	n�B	q�B	t�B	x�B	|�B	~�B	�B	�B	�B	�B	�B	�B	�%B	�%B	�JB	�VB	�\B	�VB	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�9B	�?B	�?B	�?B	�?B	�FB	�LB	�RB	�RB	�RB	�XB	�dB	�wB	�}B	��B	��B	ÖB	ŢB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�B	�B	�B	�B	�5B	�5B	�;B	�;B	�HB	�BB	�BB	�;B	�;B	�HB	�TB	�TB	�ZB	�`B	�`B	�mB	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
�B
&�B
/2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.16 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190544                              AO  ARCAADJP                                                                    20181005190544    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190544  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190544  QCF$                G�O�G�O�G�O�8000            