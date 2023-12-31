CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:15Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \x   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^l   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190515  20181005190515  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               *A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @׹�z�Q1   @׹�$��@1���E��c������1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      *A   A   A   @333@�  @�  A��A   A>ffA`  A�  A�  A���A���A�  A�  A�33A�  B   B  B  B  B   B(  B0  B8ffB@  BH  BP  BXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�33B�33B�33B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C#�fC&  C(  C*  C,  C.  C0  C2  C4�C6�C7�fC:  C<  C=�fC?�fCB  CD  CF  CG�fCJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr�Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C��C��C��C��3C��3C��3C�  C��3C��3C�  C�  C��3C�  C��3D   D � D  Dy�D��Dy�D  D� D  D�fD  D� D  D� D  D� D��D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� DfD�fDfD� D  D� D  D� D  D� D��D� D  D� D  D�fD  D� D  D� D  D� DfD� D  D�fD fD � D!  D!y�D"  D"� D#  D#� D$  D$�fD%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2�fD3  D3�fD4fD4� D4��D5y�D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=�fD>  D>� D?  D?� D@  D@� DAfDA� DBfDB� DB��DC� DDfDD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DMfDM� DM��DN� DOfDO�fDP  DP� DQ  DQ� DR  DR� DS  DSy�DT  DT� DT��DUy�DU��DVy�DW  DW� DX  DX�fDYfDY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^�fD_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� DdfDd� De  De� De��Dfy�Df��Dg� Dh  Dh�fDifDi� Dj  Dj�fDk  Dk� Dl  Dl� Dl��Dm� Dn  Dn� Do  Doy�Do��Dp� Dp��Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Du� DvfDv�fDw  Dw� Dw�fDy�fD�9�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @A�@�\)@�\)AG�A#�ABzAc�A��
A��
A���A���A��
A��
A�
=A��
B �B�B�B�B �B(�B0�B9Q�B@�BH�BP�BYQ�B`�Bh�Bp�Bx�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�B�B�B�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B���BĨ�BȨ�B�u�B�B�B�B�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�C :�CT{C:�C:�C:�C
:�C:�C:�C:�C:�C:�C:�C:�C:�C:�C:�C :�C":�C$!GC&:�C(:�C*:�C,:�C.:�C0:�C2:�C4T{C6T{C8!GC::�C<:�C>!GC@!GCB:�CD:�CF:�CH!GCJ:�CL:�CN:�CP:�CR:�CT:�CV:�CX:�CZ:�C\:�C^:�C`:�Cb:�Cd:�Cf:�Ch:�Cj:�Cl:�Cn:�Cp:�CrT{Ct:�Cv:�Cx:�Cz:�C|:�C~:�C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC��C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�*>C�qC�qC�qC�qC�qC�qC��C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC��C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�*>C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�*>C�qC�qC�qC�qC�qC�qC�qC�qC�qC�*>C�*>C�qC�qC�qC�*>C�*>C�*>C��C��C��C�qC��C��C�qC�qC��C�qC��D �D ��D�D�RDRD�RD�D��D�D�D�D��D�D��D�D��DRD��D	�D	��D
�D
��D�D��D�D��D�D��D�D��DRD��D�D��D�D��D�D��DD�DD��D�D��D�D��D�D��DRD��D�D��D�D�D�D��D�D��D�D��DD��D�D�D D ��D!�D!�RD"�D"��D#�D#��D$�D$�D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2�D3�D3�D4D4��D5RD5�RD6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=�D>�D>��D?�D?��D@�D@��DADA��DBDB��DCRDC��DDDD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DMDM��DNRDN��DODO�DP�DP��DQ�DQ��DR�DR��DS�DS�RDT�DT��DURDU�RDVRDV�RDW�DW��DX�DX�DYDY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^�D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��DdDd��De�De��DfRDf�RDgRDg��Dh�Dh�DiDi��Dj�Dj�Dk�Dk��Dl�Dl��DmRDm��Dn�Dn��Do�Do�RDpRDp��DqRDq��Dr�Dr��Ds�Ds��Dt�Dt��DuRDu��DvDv�Dw�Dw��Dw�Dy�D�AH111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A��
A��#A��#A���A��A��A��#A��/A��;A��/A��/A��;A��;A��HA��HA��HA��HA��TA��`A��`A��TA��`A��`A��`A��HA��/A���Aգ�A�-AԺ^A�I�A� �A��A�VA���AӮAӏ\A�v�A�dZA�S�A�33A���A�^5A��
Aѥ�A�S�A���A�dZAϡ�A�5?A���A���A�5?A�jA�dZA�^5A�\)A�A���AȅA��A�/A��yAƮAƅA�G�A�S�A��mA�ƨA�K�A��PA��A�bNA�dZA�JA��-A�C�A���A��
A�A�XA��A��RA�Q�A��9A�\)A��;A���A�Q�A�XA��A�  A��wA���A��9A�bA�ffA��A�l�A��wA��A�;dA�I�A���A�|�A��9A��HA��A�l�A�33A~jAy�Au�;As��An�Aj�!Ai/AgS�AfA�AdVAa�;A`�HA_�;A^JA\ȴA\JAY�TAW�wAT1APr�AO&�AN$�AL-AK&�AH��AG�mAF=qADbNAC+ABZAA��AAA@bA>(�A=�A;��A:ffA9�A9�PA7�
A6I�A3��A1�7A0�A0��A0�DA/&�A-�A-
=A,��A, �A*�9A)�
A)7LA(r�A'�A&{A%�-A#�
A"1A -A��A~�At�A��A~�AZA-Al�A{A�RA�A�
A=qA"�AVAn�A�A(�A��A~�A��A
��A
(�A	�FA��A��A`BA��AM�A�`A�HAZA�A�hA�A|�A�A$�A�wA�A��A jA ��@�ȴ@��@���@�I�@��@�I�@�Z@���@��@�^5@���@���@�7@�j@�Q�@���@�P@���@�Ĝ@�\@���@���@�t�@�n�@��@���@�M�@�@���@���@�O�@�ff@��@�!@��@���@�j@�ȴ@ᙚ@�G�@߾w@ާ�@ݩ�@���@�/@�  @ج@�"�@�"�@�%@�7L@�1@ָR@�7L@�p�@Չ7@ա�@պ^@�?}@�M�@�@̴9@��@�M�@�r�@���@�ff@��#@š�@��@�A�@¸R@��#@��h@�@�G�@��@�Q�@��@�~�@��@���@��@�M�@�-@�$�@���@��T@���@��@�X@�7L@�j@���@���@��\@�J@���@��@�j@�  @���@�t�@�C�@�@���@��\@��+@�ff@��@��@��^@�`B@��u@�bN@�(�@��P@���@���@�n�@�^5@�=q@�J@�@��j@��u@��@��+@�@�p�@���@���@��D@���@��D@�C�@��
@�z�@��u@�r�@�I�@��@�ƨ@��@���@���@��@���@�\)@�+@�o@��!@�$�@���@�G�@��u@�I�@�;d@�E�@�E�@�5?@��T@���@�^5@�^5@�-@�$�@���@��T@��@��@�X@�X@��@���@��@�|�@���@�ȴ@��+@��@�|�@���@��@�"�@��@�@��!@��\@�-@��^@�bN@��@��@�ƨ@�ƨ@��w@���@�|�@�\)@�;d@�@���@���@�X@��@��
@�dZ@�;d@�o@��@�ȴ@��\@�^5@�5?@��^@�hs@�?}@���@�z�@�(�@���@�C�@��\@�^5@�M�@�=q@���@�&�@��@�r�@� �@���@���@�|�@�K�@���@��!@��+@�5?@�hs@��@���@�1@�|�@�33@���@�$�@�{@�{@���@���@���@�`B@�7L@��@��@��@�r�@�1'@��
@���@�t�@�;d@�ȴ@�~�@��^@�hs@�%@��j@�j@�A�@� �@�  @��P@�;d@�"�@�
=@��y@��!@��+@�ƨ@|�I111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A��
A��#A��#A���A��A��A��#A��/A��;A��/A��/A��;A��;A��HA��HA��HA��HA��TA��`A��`A��TA��`A��`A��`A��HA��/A���Aգ�A�-AԺ^A�I�A� �A��A�VA���AӮAӏ\A�v�A�dZA�S�A�33A���A�^5A��
Aѥ�A�S�A���A�dZAϡ�A�5?A���A���A�5?A�jA�dZA�^5A�\)A�A���AȅA��A�/A��yAƮAƅA�G�A�S�A��mA�ƨA�K�A��PA��A�bNA�dZA�JA��-A�C�A���A��
A�A�XA��A��RA�Q�A��9A�\)A��;A���A�Q�A�XA��A�  A��wA���A��9A�bA�ffA��A�l�A��wA��A�;dA�I�A���A�|�A��9A��HA��A�l�A�33A~jAy�Au�;As��An�Aj�!Ai/AgS�AfA�AdVAa�;A`�HA_�;A^JA\ȴA\JAY�TAW�wAT1APr�AO&�AN$�AL-AK&�AH��AG�mAF=qADbNAC+ABZAA��AAA@bA>(�A=�A;��A:ffA9�A9�PA7�
A6I�A3��A1�7A0�A0��A0�DA/&�A-�A-
=A,��A, �A*�9A)�
A)7LA(r�A'�A&{A%�-A#�
A"1A -A��A~�At�A��A~�AZA-Al�A{A�RA�A�
A=qA"�AVAn�A�A(�A��A~�A��A
��A
(�A	�FA��A��A`BA��AM�A�`A�HAZA�A�hA�A|�A�A$�A�wA�A��A jA ��@�ȴ@��@���@�I�@��@�I�@�Z@���@��@�^5@���@���@�7@�j@�Q�@���@�P@���@�Ĝ@�\@���@���@�t�@�n�@��@���@�M�@�@���@���@�O�@�ff@��@�!@��@���@�j@�ȴ@ᙚ@�G�@߾w@ާ�@ݩ�@���@�/@�  @ج@�"�@�"�@�%@�7L@�1@ָR@�7L@�p�@Չ7@ա�@պ^@�?}@�M�@�@̴9@��@�M�@�r�@���@�ff@��#@š�@��@�A�@¸R@��#@��h@�@�G�@��@�Q�@��@�~�@��@���@��@�M�@�-@�$�@���@��T@���@��@�X@�7L@�j@���@���@��\@�J@���@��@�j@�  @���@�t�@�C�@�@���@��\@��+@�ff@��@��@��^@�`B@��u@�bN@�(�@��P@���@���@�n�@�^5@�=q@�J@�@��j@��u@��@��+@�@�p�@���@���@��D@���@��D@�C�@��
@�z�@��u@�r�@�I�@��@�ƨ@��@���@���@��@���@�\)@�+@�o@��!@�$�@���@�G�@��u@�I�@�;d@�E�@�E�@�5?@��T@���@�^5@�^5@�-@�$�@���@��T@��@��@�X@�X@��@���@��@�|�@���@�ȴ@��+@��@�|�@���@��@�"�@��@�@��!@��\@�-@��^@�bN@��@��@�ƨ@�ƨ@��w@���@�|�@�\)@�;d@�@���@���@�X@��@��
@�dZ@�;d@�o@��@�ȴ@��\@�^5@�5?@��^@�hs@�?}@���@�z�@�(�@���@�C�@��\@�^5@�M�@�=q@���@�&�@��@�r�@� �@���@���@�|�@�K�@���@��!@��+@�5?@�hs@��@���@�1@�|�@�33@���@�$�@�{@�{@���@���@���@�`B@�7L@��@��@��@�r�@�1'@��
@���@�t�@�;d@�ȴ@�~�@��^@�hs@�%@��j@�j@�A�@� �@�  @��P@�;d@�"�@�
=@��y@��!@��+@�ƨ@|�I111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
K�B
K�B
L�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
M�B
L�B
K�B
N�B
VB
dZB
{�B
��B
�!B
�-B
ȴB
��B
��B
�/B
�yB
�B
�B
�B
�B
�B
�B
�BB%B%B
��B
�B
�)B
�B
�mB
�B
��B
��B
��BB�B0!BaHBw�B�JB��B��BĜB�BBPB�B+BcTBy�B�+B�{B��B�?B�RB�XB�qBĜBɺB��BŢB�XB��B�uB�=B�B�B}�BjB�B�DB_;B9XB�BoB%B  B
�B
��B
�}B
�!B
�oB
|�B
dZB
G�B
(�B
oB	��B	�;B	�
B	��B	�9B	��B	��B	�B	o�B	bNB	]/B	W
B	N�B	L�B	I�B	B�B	=qB	9XB	,B	�B	DB��B��B��B�B�B�fB�NB�5B�#B�B�
B�B��B��B��B��B��B��BɺBǮBǮBŢBƨBƨBƨBŢBÖBƨB��B��B��B��BȴBƨBȴB��B�B�BB�5B��B��B��B�dB�XB�?B�-B�!B�B�B�B�B�B��B��B��B��B��B��B�B�B�B�B��B��B��B��B��B��B��B��B��B�-B��B�jB�}BŢBǮBƨBŢBÖBŢBƨB��BÖB��B��B��B��B��B�XB�dB�wB�LB�?B�FB�^B�wB�}B�wBȴB��B��B��B	B	oB	�B	�B	�B	�B	�B	hB	JB	VB	�B	+B	,B	(�B	+B	49B	8RB	5?B	0!B	-B	+B	,B	(�B	'�B	.B	/B	33B	1'B	+B	%�B	'�B	8RB	A�B	C�B	F�B	F�B	J�B	K�B	L�B	M�B	K�B	B�B	8RB	8RB	;dB	8RB	6FB	5?B	:^B	:^B	=qB	=qB	?}B	C�B	F�B	H�B	O�B	Q�B	R�B	S�B	W
B	YB	ZB	^5B	k�B	m�B	q�B	t�B	s�B	t�B	t�B	w�B	z�B	z�B	x�B	w�B	w�B	{�B	|�B	�B	�B	�B	�+B	�7B	�PB	�PB	�\B	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�9B	�RB	�?B	�XB	��B	ŢB	ŢB	ŢB	ĜB	ƨB	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�#B	�)B	�)B	�/B	�/B	�/B	�/B	�/B	�BB	�mB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
1B
1B
	7B
	7B
	7B
	7B
	7B

=B
DB
DB
DB
DB
DB
DB
JB
JB
PB
PB
PB
PB
VB
�B
!-222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B
K�B
K�B
L�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
M�B
L�B
K�B
N�B
VB
dZB
{�B
��B
�!B
�-B
ȴB
��B
��B
�/B
�yB
�B
�B
�B
�B
�B
�B
�BB%B%B
��B
�B
�)B
�B
�mB
�B
��B
��B
��BB�B0!BaHBw�B�JB��B��BĜB�BBPB�B+BcTBy�B�+B�{B��B�?B�RB�XB�qBĜBɺB��BŢB�XB��B�uB�=B�B�B}�BjB�B�DB_;B9XB�BoB%B  B
�B
��B
�}B
�!B
�oB
|�B
dZB
G�B
(�B
oB	��B	�;B	�
B	��B	�9B	��B	��B	�B	o�B	bNB	]/B	W
B	N�B	L�B	I�B	B�B	=qB	9XB	,B	�B	DB��B��B��B�B�B�fB�NB�5B�#B�B�
B�B��B��B��B��B��B��BɺBǮBǮBŢBƨBƨBƨBŢBÖBƨB��B��B��B��BȴBƨBȴB��B�B�BB�5B��B��B��B�dB�XB�?B�-B�!B�B�B�B�B�B��B��B��B��B��B��B�B�B�B�B��B��B��B��B��B��B��B��B��B�-B��B�jB�}BŢBǮBƨBŢBÖBŢBƨB��BÖB��B��B��B��B��B�XB�dB�wB�LB�?B�FB�^B�wB�}B�wBȴB��B��B��B	B	oB	�B	�B	�B	�B	�B	hB	JB	VB	�B	+B	,B	(�B	+B	49B	8RB	5?B	0!B	-B	+B	,B	(�B	'�B	.B	/B	33B	1'B	+B	%�B	'�B	8RB	A�B	C�B	F�B	F�B	J�B	K�B	L�B	M�B	K�B	B�B	8RB	8RB	;dB	8RB	6FB	5?B	:^B	:^B	=qB	=qB	?}B	C�B	F�B	H�B	O�B	Q�B	R�B	S�B	W
B	YB	ZB	^5B	k�B	m�B	q�B	t�B	s�B	t�B	t�B	w�B	z�B	z�B	x�B	w�B	w�B	{�B	|�B	�B	�B	�B	�+B	�7B	�PB	�PB	�\B	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�9B	�RB	�?B	�XB	��B	ŢB	ŢB	ŢB	ĜB	ƨB	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�#B	�)B	�)B	�/B	�/B	�/B	�/B	�/B	�BB	�mB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
1B
1B
	7B
	7B
	7B
	7B
	7B

=B
DB
DB
DB
DB
DB
DB
JB
JB
PB
PB
PB
PB
VB
�B
!-222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.23 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190515                              AO  ARCAADJP                                                                    20181005190515    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190515  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190515  QCF$                G�O�G�O�G�O�8000            