CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:06:18Z creation      
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
resolution        =���   axis      Z        $  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     $  B`   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     $  KP   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     $  Rt   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Y�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     $  [d   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  b�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     $  dT   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     $  kx   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  r�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     $  th   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  {�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     $  }X   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �|   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �(   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �8   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �<   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �L   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �P   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �T   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �XArgo profile    3.1 1.2 19500101000000  20181005190618  20181005190618  5904953 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               
A   AO  6432                            2B  A   APEX                            7467                            062512                          846 @ף��U�1   @ף�}'�@3��1'�c�(�\1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      
A   B   B   B��B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  C   C  C�fC  C  C
  C  C  C  C  C  C  C  C  C�C  C�fC"  C#�fC%�fC'��C)��C,  C.33C0�C1�fC3��C6  C8�C:  C<  C=�fC?�fCB�CD�CF�CH33CJ33CL33CN�CO�fCR  CT33CV  CW�fCZ�C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C��C�  C�  C�  C��3C��C��fC��C��3C�  C�  C��C��fC�  C��C��3C��C��C��fC��3C��C��C��C�ٚC��fC��3C�  C�  C��C��C�  C��3C��C�  C��C��3C��fC��3C�  C��C��3C�  C��C��3C��3C��3C��3C��fC��C��C��3C��3C�  C��C��C�  C��3C��3C�  C�  C��C��3C��fC��3C�  C�  C��C��C��3C��fC��fC��3C�  C��C�  C�  C�  C��C�  C��3C�  C��C��C�  C��fC��3C��3C�  C�  C��C��C��C��C��C�  C��3C�  C��fC�  C��C��3C�  C��C�  C�  C��C��C�  C�  C��C�  C��3D y�D ��D� DfD�fD  Dy�D  D� D  D�fDfD� D  D�fDfD�fD	fD	� D
  D
�fD  Dy�D  D��DfD�fDfD�fD  D� D  Dy�D  D� D  D� D  D� D��D� DfD� D  Dy�DfD�fDfD�fDfD�fD��D� D  D� D��D� DfD�fDfD� D  D� D��D � D!fD!�fD"fD"y�D#  D#� D#��D$y�D%  D%�fD&fD&y�D'  D'� D'��D(y�D)  D)�fD*fD*� D+  D+�fD,  D,y�D-  D-�fD.  D.� D/  D/y�D0  D0�fD1fD1�fD2  D2� D3  D3y�D4  D4� D5  D5� D6  D6� D7  D7�fD8fD8� D9  D9�fD:fD:�fD;fD;�fD<  D<� D=  D=�fD>  D>y�D?  D?�fD@  D@�fDAfDA�fDB  DB�fDCfDC� DDfDDy�DEfDE�fDE��DFy�DGfDG� DH  DH� DI  DI� DJ  DJ� DK  DK�fDLfDL� DMfDM� DM��DN�fDO�DO�fDPfDP�fDQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DXy�DX��DY� DZ  DZ�fD[fD[�fD\fD\� D]  D]� D^  D^y�D^��D_s3D_�3D`y�Da  Da� Db  Dby�Dc  Dc� Dc��Dd� DefDe� Df  Df� Df��Dg� Dh  Dh�fDifDi�fDj  Dj� Dk�Dk� Dk��Dl� Dm  Dm� Dn  Dny�Do  Do�fDp�Dp�fDqfDq�fDr  Dr�fDs�Ds�fDtfDt�fDu  Duy�Dv  Dv�fDw  Dw� Dw�3Dy��D�5qD��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B Q�B(�RB0�RB8�RB@�RBH�RBP�RBX�RB`�RBh�RBp�RBy�B�\)B�\)B�\)B�\)B�(�B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�(�B�\)B�\)B�\)B�\)B�\)B�\)B�\)C .C.CzC.C.C
.C.C.C.C.C.C.C.C.CG�C.C zC".C$zC&zC'��C)��C,.C.aGC0G�C2zC3��C6.C8G�C:.C<.C>zC@zCBG�CDG�CFG�CHaGCJaGCLaGCNG�CPzCR.CTaGCV.CXzCZG�C�#�C�
C�
C�
C�
C�
=C�
C�
C�
C�
C�#�C�
C�#�C�
C�
C�
C�
=C�#�C��pC�#�C�
=C�
C�
C�#�C��pC�
C�#�C�
=C�#�C�#�C��pC�
=C�#�C�0�C�#�C��C��pC�
=C�
C�
C�#�C�#�C�
C�
=C�#�C�
C�#�C�
=C��pC�
=C�
C�#�C�
=C�
C�#�C�
=C�
=C�
=C�
=C��pC�#�C�#�C�
=C�
=C�
C�#�C�0�C�
C�
=C�
=C�
C�
C�#�C�
=C��pC�
=C�
C�
C�#�C�#�C�
=C��pC��pC�
=C�
C�#�C�
C�
C�
C�0�C�
C�
=C�
C�#�C�0�C�
C��pC�
=C�
=C�
C�
C�#�C�#�C�#�C�#�C�#�C�
C�
=C�
C��pC�
C�#�C�
=C�
C�#�C�
C�
C�#�C�#�C�
C�
C�#�C�
D D �DD��D�D��D�D�D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D�D�D�RD�D��D�D��D�D��D�D�D�D��D�D��D�D��DD��D�D��D�D�D�D��D�D��D�D��DD��D�D��DD��D�D��D�D��D�D��D D ��D!�D!��D"�D"�D#�D#��D$D$�D%�D%��D&�D&�D'�D'��D(D(�D)�D)��D*�D*��D+�D+��D,�D,�D-�D-��D.�D.��D/�D/�D0�D0��D1�D1��D2�D2��D3�D3�D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>�D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD�DE�DE��DFDF�DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DNDN��DORDO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX�DYDY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^�D_D_~�D_��D`�Da�Da��Db�Db�Dc�Dc��DdDd��De�De��Df�Df��DgDg��Dh�Dh��Di�Di��Dj�Dj��DkRDk��DlDl��Dm�Dm��Dn�Dn�Do�Do��DpRDp��Dq�Dq��Dr�Dr��DsRDs��Dt�Dt��Du�Du�Dv�Dv��Dw�Dw��Dw޸Dy�)D�;4D��R1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�jA�^5A�+A�hsA�$�AÉ7A�S�A��A���A\A���A�n�A���A��A���A�p�A���A���A�^5A�
=A�
=A���A��wA�"�A���A���A���A���A���A�hsA��A�C�A��A��A�r�A���A�JA�+A���A���A�9XA���A���A�S�A��yA�VA��HA�
=A�ffA��hA�ȴA�$�A���A��uA�ĜA���A�ĜA���A�&�A�ĜA��wA}��Ay��AuK�Aq�PAnffAm��AmdZAln�Aj��Ai�Ah�Ad�`Ac��AbVAa�7A`A�A_�A\=qAU��AS/AQ&�AP �AOS�AM�TAK�AH^5AG�hAF�!AE�A'A'�hA%�
A$�A$�A#��A#|�A#"�A"ĜA"��A!�-A 1'A�A�jAz�A�A`BAA�yA�A��A�\A�wA��A�AG�A1A��A5?A�A��At�A\)A
ĜA
��A
jA	ƨA	33A-AXA��A��AG�A��A%AA�A(�A1A ��@��R@��@��9@�~�@��@���@��m@�;d@��\@�5?@�7L@� �@�^5@�-@�p�@�D@�t�@�bN@��@�=q@�1@���@���@�33@��#@��
@��@��@��@ف@��@���@ش9@؋D@�r�@�Q�@���@ם�@�{@�1'@��@�{@���@���@�x�@��@���@Ϯ@ύP@�33@��@ͩ�@�O�@�1@�M�@Ѓ@У�@�j@϶F@ύP@��y@���@͑h@��@���@��@�O�@ȴ9@�C�@Ĵ9@�1@�+@�V@��9@��@�ƨ@�@�v�@�n�@�V@���@�x�@�p�@�O�@�/@���@� �@�;d@�ȴ@�n�@�~�@�{@��@��P@�dZ@���@�ȴ@�5?@��T@�G�@��`@���@���@�Ĝ@�Q�@�ƨ@�o@��!@���@�n�@�$�@���@�?}@�&�@���@��u@�z�@�j@�A�@�1'@� �@�b@��m@���@�l�@�;d@��@���@�5?@��@���@�G�@���@���@��j@��@��m@���@��@��!@�~�@�n�@�V@���@�?}@���@���@��j@��9@��j@��@���@�r�@���@���@��P@�o@��!@�V@�^5@�$�@��T@�7L@�V@��/@�r�@��F@�@��@�\)@���@��P@�;d@��@���@�=q@��h@�X@�&�@���@��/@��u@��@���@�^5@�M�@�5?@�{@�x�@���@��D@�I�@��w@�\)@���@��@�@���@�x�@�p�@�p�@�`B@�/@��`@�Z@�1'@�j@�1@���@��P@�S�@�;d@�;d@�+@�@���@��@��!@�E�@�-@�-@�-@�-@���@��#@���@�x�@���@�Ĝ@��@��u@�r�@�I�@� �@���@���@��F@��w@��F@���@�dZ@�C�@�o@�M�@���@��T@��#@��#@���@��^@���@��7@�x�@�p�@�`B@�G�@���@�bN@�(�@��
@�K�@�o@�
=@���@�ȴ@�E�@���@�`B@�%@��@��u@��@�bN@�I�@��@�ƨ@���@���@��@�K�@�;d@��@��R@�n�@�n�@�v�@�~�@��+@�~�@�V@�V@�=q@���@�hs@�G�@���@��@�bN@���@�\)@�;d@�+@�
=@�ȴ@���@���@���@��@��R@��\@���@�O�@���@��/@�Ĝ@�e�@nȴ@^�F1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�jA�^5A�+A�hsA�$�AÉ7A�S�A��A���A\A���A�n�A���A��A���A�p�A���A���A�^5A�
=A�
=A���A��wA�"�A���A���A���A���A���A�hsA��A�C�A��A��A�r�A���A�JA�+A���A���A�9XA���A���A�S�A��yA�VA��HA�
=A�ffA��hA�ȴA�$�A���A��uA�ĜA���A�ĜA���A�&�A�ĜA��wA}��Ay��AuK�Aq�PAnffAm��AmdZAln�Aj��Ai�Ah�Ad�`Ac��AbVAa�7A`A�A_�A\=qAU��AS/AQ&�AP �AOS�AM�TAK�AH^5AG�hAF�!AE�A'A'�hA%�
A$�A$�A#��A#|�A#"�A"ĜA"��A!�-A 1'A�A�jAz�A�A`BAA�yA�A��A�\A�wA��A�AG�A1A��A5?A�A��At�A\)A
ĜA
��A
jA	ƨA	33A-AXA��A��AG�A��A%AA�A(�A1A ��@��R@��@��9@�~�@��@���@��m@�;d@��\@�5?@�7L@� �@�^5@�-@�p�@�D@�t�@�bN@��@�=q@�1@���@���@�33@��#@��
@��@��@��@ف@��@���@ش9@؋D@�r�@�Q�@���@ם�@�{@�1'@��@�{@���@���@�x�@��@���@Ϯ@ύP@�33@��@ͩ�@�O�@�1@�M�@Ѓ@У�@�j@϶F@ύP@��y@���@͑h@��@���@��@�O�@ȴ9@�C�@Ĵ9@�1@�+@�V@��9@��@�ƨ@�@�v�@�n�@�V@���@�x�@�p�@�O�@�/@���@� �@�;d@�ȴ@�n�@�~�@�{@��@��P@�dZ@���@�ȴ@�5?@��T@�G�@��`@���@���@�Ĝ@�Q�@�ƨ@�o@��!@���@�n�@�$�@���@�?}@�&�@���@��u@�z�@�j@�A�@�1'@� �@�b@��m@���@�l�@�;d@��@���@�5?@��@���@�G�@���@���@��j@��@��m@���@��@��!@�~�@�n�@�V@���@�?}@���@���@��j@��9@��j@��@���@�r�@���@���@��P@�o@��!@�V@�^5@�$�@��T@�7L@�V@��/@�r�@��F@�@��@�\)@���@��P@�;d@��@���@�=q@��h@�X@�&�@���@��/@��u@��@���@�^5@�M�@�5?@�{@�x�@���@��D@�I�@��w@�\)@���@��@�@���@�x�@�p�@�p�@�`B@�/@��`@�Z@�1'@�j@�1@���@��P@�S�@�;d@�;d@�+@�@���@��@��!@�E�@�-@�-@�-@�-@���@��#@���@�x�@���@�Ĝ@��@��u@�r�@�I�@� �@���@���@��F@��w@��F@���@�dZ@�C�@�o@�M�@���@��T@��#@��#@���@��^@���@��7@�x�@�p�@�`B@�G�@���@�bN@�(�@��
@�K�@�o@�
=@���@�ȴ@�E�@���@�`B@�%@��@��u@��@�bN@�I�@��@�ƨ@���@���@��@�K�@�;d@��@��R@�n�@�n�@�v�@�~�@��+@�~�@�V@�V@�=q@���@�hs@�G�@���@��@�bN@���@�\)@�;d@�+@�
=@�ȴ@���@���@���@��@��R@��\@���@�O�@���@��/@�Ĝ@�e�@nȴ@^�F1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B&�B&�B)�B>wBI�BVBffBp�Bx�B~�B�B�^BɺB��B��B��BɺBÖBĜB�;B�)B��B�3B��B�%Bl�BO�B=qB0!B'�B�B&�B1'BM�BM�BI�B?}B49B!�BDBB  B��B��B�B�TB��B��B��B�9B��B��B�7BiyBJ�B8RB�B+B
��B
��B
;dB
%B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�oB	�1B	�B	w�B	n�B	VB	,B	�B	uB	JB	1B��B�B�)B�
B��B[#B�FB�^B��B��B��B�B�;B�TB�`B�yB�yB�NB�/B�5B�
B��B�B�#B�#B�B�B��BƨB��B�dB�FB�'B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�'B�'B�'B�'B�'B�3B�9B�9B�?B�XB�^B�}B��B�mB��B	B	%B	1B	1B		7B		7B	DB	{B	�B	uB	uB	�B	bB		7B	B	B	%B	JB	\B	bB	�B	�B	�B	�B	�B	�B	�B	�B	!�B	#�B	&�B	&�B	'�B	)�B	0!B	49B	:^B	>wB	>wB	?}B	@�B	C�B	I�B	O�B	R�B	R�B	R�B	R�B	T�B	W
B	YB	[#B	\)B	\)B	]/B	_;B	aHB	aHB	cTB	ffB	hsB	hsB	jB	k�B	k�B	l�B	m�B	o�B	p�B	r�B	s�B	u�B	x�B	z�B	|�B	� B	�B	�B	�B	�B	�B	�+B	�1B	�1B	�1B	�1B	�+B	�7B	�7B	�=B	�PB	�bB	�hB	�oB	�oB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�3B	�9B	�3B	�-B	�FB	�jB	�}B	B	ÖB	ÖB	B	ƨB	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�;B	�5B	�5B	�)B	�#B	�#B	�#B	�#B	�#B	�B	�B	�B	�B	�B	�)B	�B	�B	�B	�B	�B	�B	�B	�/B	�/B	�/B	�5B	�ZB	�`B	�`B	�fB	�fB	�fB	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
1B
	7B

=B
DB
PB
VB
\B
bB
bB
bB
bB
oB
vB
%zB
332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222442222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B&�B&�B)�B>wBI�BVBffBp�Bx�B~�B�B�^BɺB��B��B��BɺBÖBĜB�;B�)B��B�3B��B�%Bl�BO�B=qB0!B'�B�B&�B1'BM�BM�BI�B?}B49B!�BDBB  B��B��B�B�TB��B��B��B�9B��B��B�7BiyBJ�B8RB�B+B
��B
��B
;dB
%B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�oB	�1B	�B	w�B	n�B	VB	,B	�B	uB	JB	1B��B�B�)B�
B��B[#B�FB�^B��B��B��B�B�;B�TB�`B�yB�yB�NB�/B�5B�
B��B�B�#B�#B�B�B��BƨB��B�dB�FB�'B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�'B�'B�'B�'B�'B�3B�9B�9B�?B�XB�^B�}B��B�mB��B	B	%B	1B	1B		7B		7B	DB	{B	�B	uB	uB	�B	bB		7B	B	B	%B	JB	\B	bB	�B	�B	�B	�B	�B	�B	�B	�B	!�B	#�B	&�B	&�B	'�B	)�B	0!B	49B	:^B	>wB	>wB	?}B	@�B	C�B	I�B	O�B	R�B	R�B	R�B	R�B	T�B	W
B	YB	[#B	\)B	\)B	]/B	_;B	aHB	aHB	cTB	ffB	hsB	hsB	jB	k�B	k�B	l�B	m�B	o�B	p�B	r�B	s�B	u�B	x�B	z�B	|�B	� B	�B	�B	�B	�B	�B	�+B	�1B	�1B	�1B	�1B	�+B	�7B	�7B	�=B	�PB	�bB	�hB	�oB	�oB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�3B	�9B	�3B	�-B	�FB	�jB	�}B	B	ÖB	ÖB	B	ƨB	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�;B	�5B	�5B	�)B	�#B	�#B	�#B	�#B	�#B	�B	�B	�B	�B	�B	�)B	�B	�B	�B	�B	�B	�B	�B	�/B	�/B	�/B	�5B	�ZB	�`B	�`B	�fB	�fB	�fB	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
1B
	7B

=B
DB
PB
VB
\B
bB
bB
bB
bB
oB
vB
%zB
332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222442222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.18 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190618                              AO  ARCAADJP                                                                    20181005190618    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190618  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190618  QCF$                G�O�G�O�G�O�C000            