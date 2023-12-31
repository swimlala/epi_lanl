CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:16:48Z creation      
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
resolution        =���   axis      Z          9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @x   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       B<   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ID   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       K   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       R   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Y   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       Z�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  a�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       c�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       j�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  q�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       s|   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  z�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       |H   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �$   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20181005191648  20181005191648  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @ןY(���1   @ןY�@�@2Ձ$�/�c�$�/�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B��B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B���B�  B�33B�33B�  B�  B�  B�  B�  B�  B���B�  B�33C   C  C  C  C  C
  C  C  C  C�C  C  C  C�C  C�fC�fC!�fC$  C&  C(  C*�C,�C.  C0  C2  C4  C6�C8  C:  C<  C>  C?�fCB  CD  CF�CH  CJ  CL  CN  CP  CR  CT  CV  CW�fCZ  C\  C^  C`  Cb  Cc�fCe�fCh  Cj�Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz�C|  C~  C�  C�  C�  C��3C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C��3C��3C�  C��C��C�  C�  C��C�  C��3C�  C��C�  C��3C�  C��C��C��3C��3C�  C��C��C��C��C�  C�  C��C�  C�  C�  C��C�  C��3C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C��C��C��C��C��C�  C�  C��3C��3C�  C�  C�  C��C��C��3D fD � D ��Dy�D��D� D  D� DfDy�D  D��DfDy�D��D� D  D� D	  D	y�D
  D
� D  D� DfD�fD  Dy�D  D� D  D�fD��D� D��D�fD  D� D  Dy�DfD� DfDy�DfD� DfD� D  D� DfDy�D  D� D  D� D  D�fD��D�fD  D� D  D� D fD y�D!fD!y�D"fD"� D"��D#� D$  D$� D%  D%� D&  D&� D&��D'� D(  D(� D(��D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7fD7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?�fD@  D@� DAfDA�fDB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DHfDH� DI  DI�fDI��DJ�fDJ��DK� DL  DL�fDL��DM� DN  DN� DOfDO� DP  DP� DQ  DQ� DR  DRy�DS  DS� DS��DT�fDT��DU�fDV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ�fD[  D[� D\  D\�fD]  D]�fD^  D^� D_�D_� D_��D`�fDa  Day�Db  Db�fDb��Dcy�Dd  Dd�fDefDe� Df  Df�fDgfDg�fDh�Dh�fDifDi�fDj  Dj� Dj��Dky�Dl  Dl� Dl��Dms3Dm�3Dny�Do  Do� DpfDp��DqfDq� Dq�3Dry�Ds  Ds�fDtfDty�Dt��Du� Du��Dvy�Dv��Dw� Dyz=D�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�33A��A!��AA��Aa��A���A���A���A���A���A���A���A���B ffBffBffBffB   B(ffB0ffB8ffB@ffBHffBPffBXffB`ffBhffBpffBxffB�33B�33B�33B�33B�33B�33B�  B�33B�33B�33B�33B�  B�33B�33B�33B�33B�33B�33B�33B�  B�33B�ffB�ffB�33B�33B�33B�33B�33B�33B�  B�33B�ffC �C�C�C�C�C
�C�C�C�C34C�C�C�C34C�C  C   C"  C$�C&�C(�C*34C,34C.�C0�C2�C4�C634C8�C:�C<�C>�C@  CB�CD�CF34CH�CJ�CL�CN�CP�CR�CT�CV�CX  CZ�C\�C^�C`�Cb�Cd  Cf  Ch�Cj34Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz34C|�C~�C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C�  C��C��C��C��C��C��C��C�  C��C��C��C�  C��C��C��C�  C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C�  C��C��C��C��C��C�  D �D �fD  D� D  D�fDfD�fD�D� DfD�3D�D� D  D�fDfD�fD	fD	� D
fD
�fDfD�fD�D��DfD� DfD�fDfD��D  D�fD  D��DfD�fDfD� D�D�fD�D� D�D�fD�D�fDfD�fD�D� DfD�fDfD�fDfD��D  D��DfD�fDfD�fD �D � D!�D!� D"�D"�fD#  D#�fD$fD$�fD%fD%�fD&fD&�fD'  D'�fD(fD(�fD)  D)�fD*fD*�fD+fD+�fD,fD,�fD-fD-�fD.fD.�fD/fD/�fD0fD0�fD1fD1�fD2fD2�fD3fD3�fD4fD4�fD5fD5�fD6fD6�fD7�D7�fD8fD8�fD9fD9�fD:fD:�fD;fD;�fD<fD<�fD=fD=�fD>fD>�fD?fD?��D@fD@�fDA�DA��DBfDB�fDCfDC�fDDfDD�fDEfDE�fDFfDF�fDGfDG�fDH�DH�fDIfDI��DJ  DJ��DK  DK�fDLfDL��DM  DM�fDNfDN�fDO�DO�fDPfDP�fDQfDQ�fDRfDR� DSfDS�fDT  DT��DU  DU��DVfDV�fDWfDW�fDXfDX�fDYfDY�fDZfDZ��D[fD[�fD\fD\��D]fD]��D^fD^�fD_3D_�fD`  D`��DafDa� DbfDb��Dc  Dc� DdfDd��De�De�fDffDf��Dg�Dg��Dh3Dh��Di�Di��DjfDj�fDk  Dk� DlfDl�fDm  Dmy�Dm��Dn� DofDo�fDp�Dp�3Dq�Dq�fDq��Dr� DsfDs��Dt�Dt� Du  Du�fDv  Dv� Dw  Dw�fDy��D�C�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AȁA�~�AȃAȇ+Aȉ7Aȉ7Aȇ+Aȇ+AȃA�Q�A�A�9XA�A��A��HA��#A���A�|�A�7LA��/A��/Aţ�A�ZA�5?A���A��mAĺ^AēuA�r�A�A�A��TAþwA�n�A�p�A�\)A�?}A�$�A�JA��A¶FA�n�A�\)A�"�A���A�^5A�;dA�(�A�A�  A��A���A��A��\A�z�A�bA���A�;dA���A��^A�ĜA���A�O�A���A��\A�A���A��A�C�A�5?A�JA�C�A���A��PA�bNA�;dA��mA��A���A�9XA�A���A�ZA��A���A���A�/A� �A���A��TA��uA��jA�ȴA��^A�1A�M�A���A���A�t�A��A�v�A�/A�p�A�?}A�1'A���A�ȴA�XA��RA��mA�33A�l�A��Ay�
AuXAsAr��Ap�RAljAf1'A_�TA]O�AZ~�AX��AWC�AU��AS�wANbAL1'AK"�AG�-AD1ABA@z�A?O�A=��A<I�A;�A:�9A6~�A49XA2�/A/��A,��A*�/A*VA)x�A($�A$jA"ZA!�mA!�FA!�hA"1A ��A��A�/AM�AC�A �`A v�A  �@��;@��@��@�J@��@�x�@�Z@���@��y@�ƨ@�z�@�ƨ@���@�M�@�7@��m@��@��`@�1'@�C�@�M�@���@ߍP@�J@���@�ƨ@�@�r�@�S�@���@��@�(�@ӝ�@�O�@Ϯ@�x�@��@͙�@�V@��@�j@�|�@�v�@�=q@��@���@�O�@��/@�j@�Q�@�9X@�(�@���@��w@���@�^5@��^@�%@��/@�Ĝ@��@�9X@��w@���@���@��T@��#@��j@�K�@���@���@���@���@��+@�$�@��j@�=q@��@�&�@�p�@���@��-@��^@���@���@���@�@�?}@��@��D@�z�@�j@�j@�bN@�bN@�bN@�I�@�b@���@��@��
@��P@�33@�ȴ@�-@���@��@��7@�V@�z�@�(�@���@�t�@�C�@�@��@��@��@��!@�=q@�-@��@��@��^@��@��D@�Z@�A�@�1'@��@��w@�K�@�~�@�E�@�{@��h@�X@�r�@��F@�  @�9X@�j@�1'@�  @�K�@���@�{@���@��@���@�@��@���@�b@�-@��T@��-@��h@�G�@��@���@�1'@��m@��@���@�S�@�"�@�~�@�5?@�J@���@���@�&�@���@�Q�@�A�@�A�@�A�@�9X@�1'@�(�@� �@��@��@��@�b@�1@�  @�1@�Z@�r�@��@�Z@�9X@�b@��m@��F@���@���@�l�@�~�@�{@��#@���@��h@�`B@�G�@�&�@��/@���@�Ĝ@���@�I�@��
@��y@��R@��!@��\@�M�@�{@���@�hs@��/@���@��@�Z@�A�@��@���@�t�@�"�@�ȴ@�ȴ@���@��+@�V@�=q@��#@���@���@��@�hs@���@���@��;@�ƨ@���@�t�@�o@���@�v�@���@�X@��@�V@���@��j@���@�z�@�j@�bN@�I�@��@�t�@�+@��@���@��\@�v�@�ff@�V@�V@�=q@��@��@��@�x�@���@��@�z�@�9X@��@�1@�w@�@�@�@4�@k{J111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AȁA�~�AȃAȇ+Aȉ7Aȉ7Aȇ+Aȇ+AȃA�Q�A�A�9XA�A��A��HA��#A���A�|�A�7LA��/A��/Aţ�A�ZA�5?A���A��mAĺ^AēuA�r�A�A�A��TAþwA�n�A�p�A�\)A�?}A�$�A�JA��A¶FA�n�A�\)A�"�A���A�^5A�;dA�(�A�A�  A��A���A��A��\A�z�A�bA���A�;dA���A��^A�ĜA���A�O�A���A��\A�A���A��A�C�A�5?A�JA�C�A���A��PA�bNA�;dA��mA��A���A�9XA�A���A�ZA��A���A���A�/A� �A���A��TA��uA��jA�ȴA��^A�1A�M�A���A���A�t�A��A�v�A�/A�p�A�?}A�1'A���A�ȴA�XA��RA��mA�33A�l�A��Ay�
AuXAsAr��Ap�RAljAf1'A_�TA]O�AZ~�AX��AWC�AU��AS�wANbAL1'AK"�AG�-AD1ABA@z�A?O�A=��A<I�A;�A:�9A6~�A49XA2�/A/��A,��A*�/A*VA)x�A($�A$jA"ZA!�mA!�FA!�hA"1A ��A��A�/AM�AC�A �`A v�A  �@��;@��@��@�J@��@�x�@�Z@���@��y@�ƨ@�z�@�ƨ@���@�M�@�7@��m@��@��`@�1'@�C�@�M�@���@ߍP@�J@���@�ƨ@�@�r�@�S�@���@��@�(�@ӝ�@�O�@Ϯ@�x�@��@͙�@�V@��@�j@�|�@�v�@�=q@��@���@�O�@��/@�j@�Q�@�9X@�(�@���@��w@���@�^5@��^@�%@��/@�Ĝ@��@�9X@��w@���@���@��T@��#@��j@�K�@���@���@���@���@��+@�$�@��j@�=q@��@�&�@�p�@���@��-@��^@���@���@���@�@�?}@��@��D@�z�@�j@�j@�bN@�bN@�bN@�I�@�b@���@��@��
@��P@�33@�ȴ@�-@���@��@��7@�V@�z�@�(�@���@�t�@�C�@�@��@��@��@��!@�=q@�-@��@��@��^@��@��D@�Z@�A�@�1'@��@��w@�K�@�~�@�E�@�{@��h@�X@�r�@��F@�  @�9X@�j@�1'@�  @�K�@���@�{@���@��@���@�@��@���@�b@�-@��T@��-@��h@�G�@��@���@�1'@��m@��@���@�S�@�"�@�~�@�5?@�J@���@���@�&�@���@�Q�@�A�@�A�@�A�@�9X@�1'@�(�@� �@��@��@��@�b@�1@�  @�1@�Z@�r�@��@�Z@�9X@�b@��m@��F@���@���@�l�@�~�@�{@��#@���@��h@�`B@�G�@�&�@��/@���@�Ĝ@���@�I�@��
@��y@��R@��!@��\@�M�@�{@���@�hs@��/@���@��@�Z@�A�@��@���@�t�@�"�@�ȴ@�ȴ@���@��+@�V@�=q@��#@���@���@��@�hs@���@���@��;@�ƨ@���@�t�@�o@���@�v�@���@�X@��@�V@���@��j@���@�z�@�j@�bN@�I�@��@�t�@�+@��@���@��\@�v�@�ff@�V@�V@�=q@��@��@��@�x�@���@��@�z�@�9X@��@�1@�w@�@�@�@4�@k{J111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
=B
=B
=B
=B
=B
=B
=B
=B
=BVB�B)�B6FB>wBA�BA�BC�BL�BXBq�Bx�B�B�\B�{B�uB��B��B�B�^BƨB��B��B�#B�;B�mB�yB�sB�B��B\B0!B7LBN�B[#BdZBjBr�Bw�Bw�Bw�By�B}�B� B�B�7B�oB��B��B��B�B�B�B��B��B�B��B��B�VB�DB�B� B|�Bz�By�Bx�Bv�Bt�Bq�Bn�Bl�BiyBhsBe`BbNB]/BW
BL�BG�B@�B5?B,B#�B1B�`B��B~�Bw�Bo�BffB[#B"�B
��B
�/B
ÖB
�-B
x�B
S�B
<jB
/B
$�B
�B
B	�B	�^B	�B	��B	�uB	q�B	E�B	!�B	�B	JB	B��B�B�mB��B��B��B�B��BŢB�}B�LB�B�B�B�B�B��B��B��B�uB�bB�JB�B{�Bu�Bs�Bv�Bz�B��B��BȴB��Bo�Bn�Bo�Bo�Bp�Bq�Bq�Br�Bs�Bs�Br�Bq�Bp�Bp�Bq�Bo�Bn�Bn�Bo�Bo�Bo�Bp�Bs�Bu�Bw�Bx�Bz�B{�B{�By�By�Bw�Bv�Bv�Bv�Bv�Bs�Bq�Bo�Bn�Bs�Bz�B�B�1A���B��B��B	  B	%B	1B	
=B	JB	\B	oB	�B	�B	�B	�B	�B	�B	�B	�B	"�B	%�B	&�B	&�B	'�B	(�B	)�B	)�B	)�B	-B	-B	1'B	2-B	33B	8RB	>wB	A�B	A�B	@�B	=qB	8RB	7LB	:^B	?}B	A�B	A�B	B�B	B�B	B�B	C�B	D�B	K�B	VB	ZB	[#B	]/B	_;B	aHB	cTB	dZB	gmB	jB	k�B	l�B	n�B	r�B	w�B	{�B	~�B	� B	� B	�B	�B	�VB	�hB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�9B	�9B	�LB	�XB	�dB	�^B	�LB	�9B	�FB	�RB	�XB	�^B	�dB	�dB	�wB	�}B	�}B	�}B	B	B	B	ÖB	ÖB	ĜB	ŢB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�)B	�)B	�/B	�5B	�5B	�;B	�5B	�;B	�BB	�HB	�NB	�ZB	�ZB	�`B	�fB	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
+B
%B
	7B
DB
DB
JB
JB
JB
JB
JB
PB
VB
\B
bB
bB
hB
hB
hB
oB
oB
uB
uB
uB
uB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
)�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222242222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B
=B
=B
=B
=B
=B
=B
=B
=B
=BVB�B)�B6FB>wBA�BA�BC�BL�BXBq�Bx�B�B�\B�{B�uB��B��B�B�^BƨB��B��B�#B�;B�mB�yB�sB�B��B\B0!B7LBN�B[#BdZBjBr�Bw�Bw�Bw�By�B}�B� B�B�7B�oB��B��B��B�B�B�B��B��B�B��B��B�VB�DB�B� B|�Bz�By�Bx�Bv�Bt�Bq�Bn�Bl�BiyBhsBe`BbNB]/BW
BL�BG�B@�B5?B,B#�B1B�`B��B~�Bw�Bo�BffB[#B"�B
��B
�/B
ÖB
�-B
x�B
S�B
<jB
/B
$�B
�B
B	�B	�^B	�B	��B	�uB	q�B	E�B	!�B	�B	JB	B��B�B�mB��B��B��B�B��BŢB�}B�LB�B�B�B�B�B��B��B��B�uB�bB�JB�B{�Bu�Bs�Bv�Bz�B��B��BȴB��Bo�Bn�Bo�Bo�Bp�Bq�Bq�Br�Bs�Bs�Br�Bq�Bp�Bp�Bq�Bo�Bn�Bn�Bo�Bo�Bo�Bp�Bs�Bu�Bw�Bx�Bz�B{�B{�By�By�Bw�Bv�Bv�Bv�Bv�Bs�Bq�Bo�Bn�Bs�Bz�B�B�1A���B��B��B	  B	%B	1B	
=B	JB	\B	oB	�B	�B	�B	�B	�B	�B	�B	�B	"�B	%�B	&�B	&�B	'�B	(�B	)�B	)�B	)�B	-B	-B	1'B	2-B	33B	8RB	>wB	A�B	A�B	@�B	=qB	8RB	7LB	:^B	?}B	A�B	A�B	B�B	B�B	B�B	C�B	D�B	K�B	VB	ZB	[#B	]/B	_;B	aHB	cTB	dZB	gmB	jB	k�B	l�B	n�B	r�B	w�B	{�B	~�B	� B	� B	�B	�B	�VB	�hB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�9B	�9B	�LB	�XB	�dB	�^B	�LB	�9B	�FB	�RB	�XB	�^B	�dB	�dB	�wB	�}B	�}B	�}B	B	B	B	ÖB	ÖB	ĜB	ŢB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�)B	�)B	�/B	�5B	�5B	�;B	�5B	�;B	�BB	�HB	�NB	�ZB	�ZB	�`B	�fB	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
+B
%B
	7B
DB
DB
JB
JB
JB
JB
JB
PB
VB
\B
bB
bB
hB
hB
hB
oB
oB
uB
uB
uB
uB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
)�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222242222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.10 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191648                              AO  ARCAADJP                                                                    20181005191648    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191648  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191648  QCF$                G�O�G�O�G�O�8000            