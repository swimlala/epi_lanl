CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:52Z creation      
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
resolution        =���   axis      Z        h  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  K�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  S`   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  mP   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  }�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �p   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �p   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �p   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �p   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005191752  20181005191752  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��$Pg:�1   @��$��c�@5PbM���d|�\)1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     A   A   B   @���@�  A   A   A@  A`  A~ffA�33A�  A�  A�  A���A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C  C  C   C"  C$  C&  C(  C*  C,  C-�fC0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD�CF�CH�CJ  CK�fCM�fCP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cc�fCe�fCh  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C{�fC}�fC�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��C��C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C��3C�  C��C��C�  C��C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C��C��C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C��C��C�  C��3C��3C�  C�  C��3C�  C�  C��3C�  C��C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  D fD � D  D�fDfDy�D  D� D��D� D  Dy�D��Dy�D  D� D  D� D	fD	�fD
  D
y�D  D� D  D�fDfD�fD  D�fDfD�fDfD�fDfD�fD  Dy�D  D�fD  Dy�D��D� D��Dy�D  D� DfDy�D  D�fD  D�fDfD��DfD� D��Dy�D��Dy�D��Dy�D   D � D!  D!� D"  D"y�D"��D#y�D#��D$� D%fD%� D%��D&� D'fD'� D'��D(y�D(��D)y�D)��D*�fD*��D+y�D+��D,� D-  D-� D.  D.� D/  D/y�D/��D0�fD1fD1y�D2  D2�fD3  D3� D4fD4� D4��D5�fD6  D6� D7  D7y�D7��D8� D9  D9y�D:  D:��D;  D;� D<  D<�fD=fD=� D>fD>�fD?  D?�fD@fD@� DA  DA�fDBfDB�fDCfDCy�DDfDD� DEfDE� DE��DF�fDG  DGy�DG��DH�fDI  DI� DJfDJy�DK  DK� DL  DL� DMfDMy�DN  DN� DO�DO� DPfDPy�DQfDQ�fDQ��DR�fDSfDS� DTfDT� DT��DU�fDV  DV�fDW  DWy�DXfDXy�DY  DY�fDZfDZy�D[fD[�fD\  D\�fD]  D]�fD^  D^y�D_fD_�fD_��D`y�D`��Da� Da��Db� Dc  Dc� Dc��Ddy�DefDe� Df  Df� Dg  Dg� Dg��Dh�fDh��Di� Di��Dj� Dj��Dk�fDl  Dl�fDl��Dm�fDm��Dn�fDo  Doy�Dp  Dp� Dq  Dq� DrfDry�DsfDs� DtfDt�fDt��Du� DvfDv� Dw  DwffDy�RD�4�D��q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�z�@��HAp�A%p�AEp�Aep�A��A��A��RA��RA¸RAӅA�RA�RB\)B	\)B\)B\)B!\)B)\)B1\)B9\)BA\)BI\)BQ\)BY\)Ba\)Bi\)Bq\)By\)B��B��B��B��B��B��B��B��B��B��B�z�B��B��B��B��B��B��BĮBȮB̮BЮBԮBخCW
CW
C W
C"W
C$W
C&W
C(W
C*W
C,W
C.=pC0W
C2W
C4W
C6W
C8W
C:W
C<W
C>W
C@W
CBW
CDp�CFp�CHp�CJW
CL=pCN=pCPW
CRW
CTW
CVW
CXW
CZW
C\W
C^W
C`W
CbW
Cd=pCf=pChW
CjW
ClW
CnW
CpW
CrW
CtW
CvW
CxW
CzW
C|=pC~=pC�+�C�8RC�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C��C��C�+�C�8RC�8RC�+�C�+�C�+�C�+�C�8RC�+�C�+�C�+�C�+�C�8RC�+�C�+�C�8RC�8RC�+�C�+�C�+�C�+�C�+�C�+�C�+�C�8RC�8RC�8RC�+�C��C�+�C�8RC�8RC�+�C�8RC�+�C�+�C�+�C�+�C�+�C�+�C�8RC�8RC�+�C�+�C�+�C�+�C�+�C�+�C�8RC�8RC�+�C�+�C�8RC�+�C�+�C�+�C�+�C�+�C�8RC�+�C�+�C�+�C�8RC�8RC�+�C��C��C�+�C�+�C��C�+�C�+�C��C�+�C�8RC�8RC�+�C�+�C�+�C�+�C�+�C�+�C�+�C��C�+�C�+�C�+�C�+�C�8RC�+�C�+�D )D ��D�D�)D)D�]D�D��D]D��D�D�]D]D�]D�D��D�D��D	)D	�)D
�D
�]D�D��D�D�)D)D�)D�D�)D)D�)D)D�)D)D�)D�D�]D�D�)D�D�]D]D��D]D�]D�D��D)D�]D�D�)D�D�)D)D��D)D��D]D�]D]D�]D]D�]D �D ��D!�D!��D"�D"�]D#]D#�]D$]D$��D%)D%��D&]D&��D')D'��D(]D(�]D)]D)�]D*]D*�)D+]D+�]D,]D,��D-�D-��D.�D.��D/�D/�]D0]D0�)D1)D1�]D2�D2�)D3�D3��D4)D4��D5]D5�)D6�D6��D7�D7�]D8]D8��D9�D9�]D:�D:��D;�D;��D<�D<�)D=)D=��D>)D>�)D?�D?�)D@)D@��DA�DA�)DB)DB�)DC)DC�]DD)DD��DE)DE��DF]DF�)DG�DG�]DH]DH�)DI�DI��DJ)DJ�]DK�DK��DL�DL��DM)DM�]DN�DN��DO"�DO��DP)DP�]DQ)DQ�)DR]DR�)DS)DS��DT)DT��DU]DU�)DV�DV�)DW�DW�]DX)DX�]DY�DY�)DZ)DZ�]D[)D[�)D\�D\�)D]�D]�)D^�D^�]D_)D_�)D`]D`�]Da]Da��Db]Db��Dc�Dc��Dd]Dd�]De)De��Df�Df��Dg�Dg��Dh]Dh�)Di]Di��Dj]Dj��Dk]Dk�)Dl�Dl�)Dm]Dm�)Dn]Dn�)Do�Do�]Dp�Dp��Dq�Dq��Dr)Dr�]Ds)Ds��Dt)Dt�)Du]Du��Dv)Dv��Dw�Dw|)Dy�D�?�D��R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A���A���A���A���A���A��hA�hsA���A���A��PA�M�A��/A���A�Q�A��TA��
A�ƨA��uA�n�A�O�A�;dA�7LA�/A�$�A�oA�%A��mA��!A��A�p�A�ffA�VA�I�A�E�A�A�A��HA��mA���A�ffA���A�VA�33A�1A�t�A��PA�XA�A�A�7LA�+A��A�Q�A��9A�v�A��jA���A��A��RA���A��A��9A�K�A�/A��`A��TA��A�?}A��FA�n�A��A��A��9A�(�A��A��A��HA��^A��A��A�`BA�dZA���A�-A���A�n�A�^5A���A�n�A�A}A{��Ay�AxbAtjAq��Aq|�Aq�Ap��Ap5?Am�Al��Al �AkS�AidZAg�TAe�7Ab��Aa�
A`��A`=qA]ƨA[AZA�AX�/AVVAUdZAT{AQ��APZAOS�AN�+AMS�ALJAI��AHJAG|�AF��AC�mAC33ABr�AA�hA?��A>�A=�A<��A<=qA;oA: �A9�PA8$�A5A4n�A4 �A3|�A2-A0��A0v�A0bNA0 �A/p�A.�yA.E�A-p�A,�RA+��A*ĜA)ƨA'��A&^5A%O�A#�mA#A"  A!��A��Ap�A��A��A�A\)A&�A�A��AM�A��A�^AG�A^5A+A��A�AVAoA�AbNA(�A1AƨA�-A�A�!A9XA��A��A{A|�AVA
��A
�A	��A	"�AG�AM�A=qA�
A+A��A��A bN@���@�{@�Z@�{@�E�@�?}@�ȴ@�@�\@�9@�V@��/@�l�@�;d@�
=@�5?@���@�ȴ@���@ߍP@�=q@�7L@܃@� �@�;d@�p�@ם�@���@֏\@և+@�M�@�x�@�7L@�I�@�b@��@��@�b@���@ӥ�@�ȴ@��@с@�X@��@���@��@�n�@�&�@��m@�t�@ʧ�@�?}@��;@�K�@�o@���@Ƈ+@�V@�x�@��m@+@�O�@���@��@�~�@�7L@�Q�@���@�
=@�~�@�x�@�&�@�V@���@��@��w@��@��-@�`B@���@��j@�z�@� �@�l�@��y@��@�o@�o@�+@�
=@���@�ȴ@�M�@�p�@�&�@��@��j@��u@�I�@��w@��P@��H@���@�V@�Ĝ@�z�@��@�
=@���@�=q@���@��^@��7@�%@�j@���@�|�@�@��+@�ff@��T@��-@��@��/@��/@��/@���@�1'@���@���@��+@�E�@�J@���@�p�@�hs@�X@�%@�Z@�(�@��;@�ƨ@��;@��@���@��@�K�@�C�@��@�
=@��@���@�^5@�=q@�$�@�V@�v�@��+@��\@�~�@�E�@���@�@���@��h@�p�@�%@���@��@�A�@���@�dZ@��!@��@���@��@�%@���@�Ĝ@��9@���@���@���@���@��u@��D@�j@�1'@�1@��@�"�@��@��@��+@���@��h@�x�@�G�@��@�%@��9@�A�@��@�1@��@�S�@�@�ff@�@��@��@��@��T@��-@��7@��7@��-@���@��#@���@��7@�/@�%@���@���@�Ĝ@�  @�;d@��y@���@�=q@���@���@��h@�p�@�hs@�O�@�?}@�&�@�%@���@�r�@�I�@�b@��;@���@�S�@�;d@�;d@�;d@�o@�
=@�o@��!@��\@��R@���@�v�@�V@�=q@�L�@}��@m��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A���A���A���A���A���A���A��hA�hsA���A���A��PA�M�A��/A���A�Q�A��TA��
A�ƨA��uA�n�A�O�A�;dA�7LA�/A�$�A�oA�%A��mA��!A��A�p�A�ffA�VA�I�A�E�A�A�A��HA��mA���A�ffA���A�VA�33A�1A�t�A��PA�XA�A�A�7LA�+A��A�Q�A��9A�v�A��jA���A��A��RA���A��A��9A�K�A�/A��`A��TA��A�?}A��FA�n�A��A��A��9A�(�A��A��A��HA��^A��A��A�`BA�dZA���A�-A���A�n�A�^5A���A�n�A�A}A{��Ay�AxbAtjAq��Aq|�Aq�Ap��Ap5?Am�Al��Al �AkS�AidZAg�TAe�7Ab��Aa�
A`��A`=qA]ƨA[AZA�AX�/AVVAUdZAT{AQ��APZAOS�AN�+AMS�ALJAI��AHJAG|�AF��AC�mAC33ABr�AA�hA?��A>�A=�A<��A<=qA;oA: �A9�PA8$�A5A4n�A4 �A3|�A2-A0��A0v�A0bNA0 �A/p�A.�yA.E�A-p�A,�RA+��A*ĜA)ƨA'��A&^5A%O�A#�mA#A"  A!��A��Ap�A��A��A�A\)A&�A�A��AM�A��A�^AG�A^5A+A��A�AVAoA�AbNA(�A1AƨA�-A�A�!A9XA��A��A{A|�AVA
��A
�A	��A	"�AG�AM�A=qA�
A+A��A��A bN@���@�{@�Z@�{@�E�@�?}@�ȴ@�@�\@�9@�V@��/@�l�@�;d@�
=@�5?@���@�ȴ@���@ߍP@�=q@�7L@܃@� �@�;d@�p�@ם�@���@֏\@և+@�M�@�x�@�7L@�I�@�b@��@��@�b@���@ӥ�@�ȴ@��@с@�X@��@���@��@�n�@�&�@��m@�t�@ʧ�@�?}@��;@�K�@�o@���@Ƈ+@�V@�x�@��m@+@�O�@���@��@�~�@�7L@�Q�@���@�
=@�~�@�x�@�&�@�V@���@��@��w@��@��-@�`B@���@��j@�z�@� �@�l�@��y@��@�o@�o@�+@�
=@���@�ȴ@�M�@�p�@�&�@��@��j@��u@�I�@��w@��P@��H@���@�V@�Ĝ@�z�@��@�
=@���@�=q@���@��^@��7@�%@�j@���@�|�@�@��+@�ff@��T@��-@��@��/@��/@��/@���@�1'@���@���@��+@�E�@�J@���@�p�@�hs@�X@�%@�Z@�(�@��;@�ƨ@��;@��@���@��@�K�@�C�@��@�
=@��@���@�^5@�=q@�$�@�V@�v�@��+@��\@�~�@�E�@���@�@���@��h@�p�@�%@���@��@�A�@���@�dZ@��!@��@���@��@�%@���@�Ĝ@��9@���@���@���@���@��u@��D@�j@�1'@�1@��@�"�@��@��@��+@���@��h@�x�@�G�@��@�%@��9@�A�@��@�1@��@�S�@�@�ff@�@��@��@��@��T@��-@��7@��7@��-@���@��#@���@��7@�/@�%@���@���@�Ĝ@�  @�;d@��y@���@�=q@���@���@��h@�p�@�hs@�O�@�?}@�&�@�%@���@�r�@�I�@�b@��;@���@�S�@�;d@�;d@�;d@�o@�
=@�o@��!@��\@��R@���@�v�@�V@�=q@�L�@}��@m��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B$�B$�B$�B$�B$�B$�B$�B&�B/BaHB�B�7B�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�uB�{B�{B��B��B�!B�9B�jB�/B�yB�B�BBJBVBVB\B\B\?��B�%BbNBT�BP�BD�B7LB+B$�B�BPB��B�yB�/B��BƨB�wB�B��B�oB�+B|�Bk�BXB7LB$�B�B�B\BB
�B
�mB
�ZB
�)B
�FB
��B
��B
�DB
� B
p�B
_;B
T�B
?}B
-B
)�B
%�B
!�B
�B
%B	��B	��B	�B	�TB	�B	ɺB	�jB	�?B	�B	��B	��B	�\B	�%B	}�B	q�B	iyB	aHB	YB	S�B	T�B	O�B	H�B	B�B	-B	&�B	%�B	!�B	�B	�B	�B	�B	uB	\B		7B	B	B��B��B��B�B�fB�NB�BB�5B�B��B��B��B��B��B��BǮBĜBB�wB�XB�?B�B��B��B��B��B��B��B�hB�PB�JB�=B�=B�=B�=B�7B�1B�+B�%B�B�B�B�B�B}�B{�B{�B{�B{�B{�Bz�Bz�Bz�By�Bx�Bw�Bv�Bt�Br�Bq�Bp�Bo�Bn�Bm�Bk�BiyBffBgmBffBffBe`BdZBe`BdZBdZBe`BcTBdZBdZBffBffBiyBjBjBk�Bl�Bl�Bk�Bk�Bn�Bo�Br�Br�Bt�Bu�Bv�Bw�By�B|�B�B�B�B�B�B�B�%B�7B�=B�=B�=B�=B�=B�DB�VB�hB�uB�{B��B��B��B��B��B��B��B��B�B�-B�3B�3B�9B�9B�9B�FB�XB�dB�wB�}BBɺB��B��B�
B�)B�;B�`B�mB�yB�yB�B�B�B�B��B��B��B��B��B��B	B	B	1B	DB	VB	\B	bB	bB	hB	�B	�B	�B	�B	�B	�B	"�B	"�B	"�B	#�B	&�B	(�B	)�B	+B	.B	/B	1'B	1'B	33B	33B	5?B	:^B	?}B	D�B	I�B	N�B	O�B	T�B	ZB	\)B	_;B	e`B	ffB	gmB	gmB	hsB	jB	l�B	n�B	p�B	s�B	u�B	u�B	u�B	w�B	}�B	� B	�B	�B	�%B	�=B	�VB	�bB	�hB	�hB	�oB	�oB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�-B	�9B	�9B	�LB	�RB	�XB	�XB	�^B	�^B	�^B	�^B	�^B	�^B	�dB	�qB	�wB	��B	��B	��B	��B	��B	B	B	��B	��B	��B	B	ŢB	ƨB	ǮB	ɺB	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�)B	�/B	�BB	�HB	�NB	�`B	�`B	�fB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
�B
B
-C222222222222222222222222222222222222222222222222222242222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B$�B$�B$�B$�B$�B$�B$�B&�B/BaHB�B�7B�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�uB�{B�{B��B��B�!B�9B�jB�/B�yB�B�BBJBVBVB\B\B\?��B�%BbNBT�BP�BD�B7LB+B$�B�BPB��B�yB�/B��BƨB�wB�B��B�oB�+B|�Bk�BXB7LB$�B�B�B\BB
�B
�mB
�ZB
�)B
�FB
��B
��B
�DB
� B
p�B
_;B
T�B
?}B
-B
)�B
%�B
!�B
�B
%B	��B	��B	�B	�TB	�B	ɺB	�jB	�?B	�B	��B	��B	�\B	�%B	}�B	q�B	iyB	aHB	YB	S�B	T�B	O�B	H�B	B�B	-B	&�B	%�B	!�B	�B	�B	�B	�B	uB	\B		7B	B	B��B��B��B�B�fB�NB�BB�5B�B��B��B��B��B��B��BǮBĜBB�wB�XB�?B�B��B��B��B��B��B��B�hB�PB�JB�=B�=B�=B�=B�7B�1B�+B�%B�B�B�B�B�B}�B{�B{�B{�B{�B{�Bz�Bz�Bz�By�Bx�Bw�Bv�Bt�Br�Bq�Bp�Bo�Bn�Bm�Bk�BiyBffBgmBffBffBe`BdZBe`BdZBdZBe`BcTBdZBdZBffBffBiyBjBjBk�Bl�Bl�Bk�Bk�Bn�Bo�Br�Br�Bt�Bu�Bv�Bw�By�B|�B�B�B�B�B�B�B�%B�7B�=B�=B�=B�=B�=B�DB�VB�hB�uB�{B��B��B��B��B��B��B��B��B�B�-B�3B�3B�9B�9B�9B�FB�XB�dB�wB�}BBɺB��B��B�
B�)B�;B�`B�mB�yB�yB�B�B�B�B��B��B��B��B��B��B	B	B	1B	DB	VB	\B	bB	bB	hB	�B	�B	�B	�B	�B	�B	"�B	"�B	"�B	#�B	&�B	(�B	)�B	+B	.B	/B	1'B	1'B	33B	33B	5?B	:^B	?}B	D�B	I�B	N�B	O�B	T�B	ZB	\)B	_;B	e`B	ffB	gmB	gmB	hsB	jB	l�B	n�B	p�B	s�B	u�B	u�B	u�B	w�B	}�B	� B	�B	�B	�%B	�=B	�VB	�bB	�hB	�hB	�oB	�oB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�-B	�9B	�9B	�LB	�RB	�XB	�XB	�^B	�^B	�^B	�^B	�^B	�^B	�dB	�qB	�wB	��B	��B	��B	��B	��B	B	B	��B	��B	��B	B	ŢB	ƨB	ǮB	ɺB	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�)B	�/B	�BB	�HB	�NB	�`B	�`B	�fB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
�B
B
-C222222222222222222222222222222222222222222222222222242222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.34 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191752                              AO  ARCAADJP                                                                    20181005191752    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191752  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191752  QCF$                G�O�G�O�G�O�8000            