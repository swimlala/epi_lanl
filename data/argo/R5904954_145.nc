CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:22Z creation      
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
_FillValue                 �  A4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \h   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �H   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �h   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �l   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �p   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �t   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005191722  20181005191722  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @�Ӥ����1   @�ӥq�p@5vȴ9X�dy�7Kƨ1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A���A�  A�  A�  B   B  BffBffB   B(  B0  B8  B@��BG��BP  BX  B`  Bh  Bp  Bw��B��B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B���B���B���B���B�  B�33B�  B�  B�  B�  C �C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C%�fC(  C*  C,  C.  C0  C1�fC3�fC5�fC8  C:  C<  C>  C@  CB�CD  CF  CH  CJ  CL�CN  CP�CR  CS�fCV  CX  CZ  C\  C^  C`�Cb  Cc�fCe�fCh  Cj  Cl  Cn  Cp  Cr�Ct�Cv�Cx�Cz�C|  C~  C��C�  C�  C��C�  C�  C�  C��3C��3C�  C��C��C�  C��3C�  C��3C�  C��C��C��C�  C�  C��C��C��C�  C�  C��C��C��C��C�  C��C�  C�  C�  C�  C�  C��3C��3C��3C��3C�  C��C��C��3C�  C�  C��3C��C�  C�  C��3C�  C�  C��3C�  C�  C��3C��C�  C��C�  C�  C�  C��C��3C��3C�  C�  C�  C�  C��C��3C��3C��3C�  C��C��3C��3C�  C�  C�  C�  C��3C��3C�  C��3C�  C�  C�  C��C�  C��C��C��3C�  C��C��3C��3C��C�  C�  C�  C�  C��C��C�  C��C��C��3C��C��C�  C��3C��C�  C�  C��C�  C��3C��C�  C�  C�  C�  C�  C�  D   D �fDfD� DfD�fDfD� D��Dy�D  D�fDfD� D  D� D��Dy�D��D	�fD
  D
�fD  Dy�DfDy�D  D� DfD�fDfD� D��D� D  D�fD  D�fDfD� D�3Dy�D  Dy�D��Dy�D��D� D  D� DfDy�D��D� DfDy�D��D�fDfDy�D�3D� D  D� D fD � D ��D!� D"  D"�fD#  D#�fD$  D$�fD%fD%� D&  D&� D'  D'� D'��D(�fD)fD)� D*  D*� D*��D+� D,fD,�fD-fD-� D.  D.� D/  D/�fD0fD0� D1  D1� D2fD2� D2��D3y�D4  D4�fD5  D5� D5��D6y�D7fD7� D7��D8� D9  D9� D9��D:y�D:��D;� D<  D<�fD=  D=�fD>  D>y�D>��D?y�D@  D@� D@��DA� DBfDB�fDCfDC�fDDfDDy�DD��DEy�DE��DF� DGfDG� DH  DH� DI  DI� DJfDJ�fDKfDKy�DL  DL� DM  DM�fDN  DNy�DO  DO�fDPfDPs3DP��DQy�DR  DR�fDS�DS�fDTfDT�fDU  DU�fDV  DVy�DW  DW��DXfDXy�DX��DY� DY��DZy�D[  D[�fD\fD\��D]fD]�fD^fD^� D_fD_�fD`fD`y�D`�3Da� DbfDb� Dc  Dcy�Dd  Dd�fDe  De� Df  Df� Dg  Dg� Dh  Dhy�Dh��Di� Dj  Dj�fDk  Dk� Dl  Dl� Dl��Dm� Dn  Dny�Dn��Doy�DpfDp� Dq  Dq� Dr  Dr�fDs  Ds�fDt  Dt� Dt��Du� Dv  Dv�fDwfDw�fDx  Dxl�D�0�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��R@�Q�A(�A$(�AD(�Ad(�A�{A�{A�{A�{A��HA�{A�{A�{B
=B	
=Bp�Bp�B!
=B)
=B1
=B9
=BA�
BH��BQ
=BY
=Ba
=Bi
=Bq
=Bx��B�Q�B��B��B�Q�B�Q�B��B��B��B��B��B��B��B��B��B��B��RB��RBąBȅB̅BЅBԅB�Q�B�Q�B�Q�B�Q�B�B�RB��B�B��B��C \)CB�CB�CB�CB�C
B�CB�CB�CB�CB�CB�CB�CB�CB�CB�CB�C B�C"B�C$B�C&(�C(B�C*B�C,B�C.B�C0B�C2(�C4(�C6(�C8B�C:B�C<B�C>B�C@B�CB\)CDB�CFB�CHB�CJB�CL\)CNB�CP\)CRB�CT(�CVB�CXB�CZB�C\B�C^B�C`\)CbB�Cd(�Cf(�ChB�CjB�ClB�CnB�CpB�Cr\)Ct\)Cv\)Cx\)Cz\)C|B�C~B�C�.C�!HC�!HC�.C�!HC�!HC�!HC�{C�{C�!HC�.C�.C�!HC�{C�!HC�{C�!HC�.C�.C�.C�!HC�!HC�.C�:�C�.C�!HC�!HC�.C�.C�.C�.C�!HC�.C�!HC�!HC�!HC�!HC�!HC�{C�{C�{C�{C�!HC�.C�.C�{C�!HC�!HC�{C�.C�!HC�!HC�{C�!HC�!HC�{C�!HC�!HC�{C�.C�!HC�.C�!HC�!HC�!HC�.C�{C�{C�!HC�!HC�!HC�!HC�.C�{C�{C�{C�!HC�.C�{C�{C�!HC�!HC�!HC�!HC�{C�{C�!HC�{C�!HC�!HC�!HC�.C�!HC�.C�:�C�{C�!HC�.C�{C�{C�.C�!HC�!HC�!HC�!HC�.C�.C�!HC�.C�.C�{C�.C�.C�!HC�{C�.C�!HC�!HC�.C�!HC�{C�.C�!HC�!HC�!HC�!HC�!HC�!HD �D �
D
D��D
D�
D
D��D
>D�>D�D�
D
D��D�D��D
>D�>D	
>D	�
D
�D
�
D�D�>D
D�>D�D��D
D�
D
D��D
>D��D�D�
D�D�
D
D��D�D�>D�D�>D
>D�>D
>D��D�D��D
D�>D
>D��D
D�>D
>D�
D
D�>D�D��D�D��D 
D ��D!
>D!��D"�D"�
D#�D#�
D$�D$�
D%
D%��D&�D&��D'�D'��D(
>D(�
D)
D)��D*�D*��D+
>D+��D,
D,�
D-
D-��D.�D.��D/�D/�
D0
D0��D1�D1��D2
D2��D3
>D3�>D4�D4�
D5�D5��D6
>D6�>D7
D7��D8
>D8��D9�D9��D:
>D:�>D;
>D;��D<�D<�
D=�D=�
D>�D>�>D?
>D?�>D@�D@��DA
>DA��DB
DB�
DC
DC�
DD
DD�>DE
>DE�>DF
>DF��DG
DG��DH�DH��DI�DI��DJ
DJ�
DK
DK�>DL�DL��DM�DM�
DN�DN�>DO�DO�
DP
DP��DQ
>DQ�>DR�DR�
DSqDS�
DT
DT�
DU�DU�
DV�DV�>DW�DW�qDX
DX�>DY
>DY��DZ
>DZ�>D[�D[�
D\
D\�qD]
D]�
D^
D^��D_
D_�
D`
D`�>Da�Da��Db
Db��Dc�Dc�>Dd�Dd�
De�De��Df�Df��Dg�Dg��Dh�Dh�>Di
>Di��Dj�Dj�
Dk�Dk��Dl�Dl��Dm
>Dm��Dn�Dn�>Do
>Do�>Dp
Dp��Dq�Dq��Dr�Dr�
Ds�Ds�
Dt�Dt��Du
>Du��Dv�Dv�
Dw
Dw�
Dx�Dx}qD�9H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A�ƨA�ƨA���A���A�ȴA���A���A���A���A���A���A���A���A���A���A���A���A��
A���Aش9A�ȴA�\)AɸRA�/A�G�A�ZA��wA�Q�A��yA���A�$�A�\)A���A�l�A�ZA���A��`A���A�|�A���A��A�O�A�33A���A�dZA� �A���A�p�A�=qA�ƨA�VA�dZA��HA��A��A�(�A���A�
=A���A���A��9A���A��A���A��A�G�A���A��A��A��A�O�A�E�A��A�ZA�ȴA��A�XA���A�hsA�M�A��A��TA��RA�  A�(�A�A���A�XA��A�A~��A~�DA}�wA{C�Ay��AxZAwoAux�Ar~�Ap  AnjAjȴAg&�A`�RA^r�A]ƨA[��AY`BAW��AU
=AS��AR�HAQ�^AOO�AL=qAH1AD��ABJA@ĜA?��A>�/A<��A:��A9�A933A8�DA5��A3&�A1�A0ZA/��A/O�A.�!A.bA,�yA*�A(A'
=A&��A&bA$�A#�A"��A!�
A ��AS�AZA|�A �A�^Ax�A�A��A�-A�+AȴA��AbA�A�A�#Ax�A�\A�-A%A5?AXAn�A�
AC�A
�A
JA	?}A�`A��A��A/A �A;dA��A5?At�A�HA$�A�;A�-Ax�A �9@�\)@��-@���@���@�v�@���@�I�@��P@�V@�X@�A�@�ff@��@��@�~�@�J@�G�@�|�@�M�@�-@陚@�X@��/@�@�@�$�@�D@◍@�bN@޸R@�p�@�r�@��@��
@۝�@���@�`B@׍P@�V@���@ԃ@��@с@�(�@��
@Ͼw@�|�@�@̴9@� �@�l�@��@ɲ-@�/@��@Ɵ�@�~�@Ł@���@ģ�@�bN@���@���@�{@��-@���@��D@�hs@�9X@���@��D@�K�@���@�v�@��h@��;@�E�@���@���@���@�
=@���@��T@�Z@�t�@�n�@�7L@���@���@�I�@��D@���@�1'@�1@��;@��F@���@��@��@�@��@��9@�r�@�j@��@�j@�r�@�A�@�A�@�Z@��D@��@��/@��@�Ĝ@��u@��D@���@���@�1@��P@�l�@�"�@��y@���@�~�@�M�@�-@�@���@�&�@���@��D@�9X@�1@�t�@��P@���@�t�@�K�@�+@�o@��R@�~�@���@�C�@��@���@���@�o@�ȴ@��@�/@��#@��-@�V@��D@�J@�E�@��^@��@���@���@��@���@�@�-@���@��@�G�@�%@�ƨ@���@�@�p�@��@���@��@��@��@��@�1'@��w@�(�@�Q�@�bN@��9@���@�O�@���@��u@���@��@�(�@��@�/@�&�@�r�@��@��@�l�@��@��H@�S�@��w@�ƨ@��w@��@�o@��\@�@��^@��7@��7@��7@�x�@�x�@��h@�X@�O�@�O�@�O�@�?}@�/@��@��@��@���@�9X@���@�t�@�K�@�"�@�@��H@�ȴ@�J@��-@���@�`B@��/@���@��@��;@��m@��;@��
@���@��@�|�@��@�+@�v�@�~�@�^5@�$�@�J@��@�@���@�p�@�?}@�X@�%@�Ĝ@�1@���@��m@��@��@�l�@��@��R@��\@��+@�V@�J@��@���@��h@�`B@�?}@�&�@���@��/@��9@��u@�r�@�bN@�j@��9@�?}@�O�@���@�x�@�p�@���@�?}@��7@�/@�O�@�%@�9X@�b@�I�@�Ĝ@��u@l�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A�ƨA�ƨA���A���A�ȴA���A���A���A���A���A���A���A���A���A���A���A���A��
A���Aش9A�ȴA�\)AɸRA�/A�G�A�ZA��wA�Q�A��yA���A�$�A�\)A���A�l�A�ZA���A��`A���A�|�A���A��A�O�A�33A���A�dZA� �A���A�p�A�=qA�ƨA�VA�dZA��HA��A��A�(�A���A�
=A���A���A��9A���A��A���A��A�G�A���A��A��A��A�O�A�E�A��A�ZA�ȴA��A�XA���A�hsA�M�A��A��TA��RA�  A�(�A�A���A�XA��A�A~��A~�DA}�wA{C�Ay��AxZAwoAux�Ar~�Ap  AnjAjȴAg&�A`�RA^r�A]ƨA[��AY`BAW��AU
=AS��AR�HAQ�^AOO�AL=qAH1AD��ABJA@ĜA?��A>�/A<��A:��A9�A933A8�DA5��A3&�A1�A0ZA/��A/O�A.�!A.bA,�yA*�A(A'
=A&��A&bA$�A#�A"��A!�
A ��AS�AZA|�A �A�^Ax�A�A��A�-A�+AȴA��AbA�A�A�#Ax�A�\A�-A%A5?AXAn�A�
AC�A
�A
JA	?}A�`A��A��A/A �A;dA��A5?At�A�HA$�A�;A�-Ax�A �9@�\)@��-@���@���@�v�@���@�I�@��P@�V@�X@�A�@�ff@��@��@�~�@�J@�G�@�|�@�M�@�-@陚@�X@��/@�@�@�$�@�D@◍@�bN@޸R@�p�@�r�@��@��
@۝�@���@�`B@׍P@�V@���@ԃ@��@с@�(�@��
@Ͼw@�|�@�@̴9@� �@�l�@��@ɲ-@�/@��@Ɵ�@�~�@Ł@���@ģ�@�bN@���@���@�{@��-@���@��D@�hs@�9X@���@��D@�K�@���@�v�@��h@��;@�E�@���@���@���@�
=@���@��T@�Z@�t�@�n�@�7L@���@���@�I�@��D@���@�1'@�1@��;@��F@���@��@��@�@��@��9@�r�@�j@��@�j@�r�@�A�@�A�@�Z@��D@��@��/@��@�Ĝ@��u@��D@���@���@�1@��P@�l�@�"�@��y@���@�~�@�M�@�-@�@���@�&�@���@��D@�9X@�1@�t�@��P@���@�t�@�K�@�+@�o@��R@�~�@���@�C�@��@���@���@�o@�ȴ@��@�/@��#@��-@�V@��D@�J@�E�@��^@��@���@���@��@���@�@�-@���@��@�G�@�%@�ƨ@���@�@�p�@��@���@��@��@��@��@�1'@��w@�(�@�Q�@�bN@��9@���@�O�@���@��u@���@��@�(�@��@�/@�&�@�r�@��@��@�l�@��@��H@�S�@��w@�ƨ@��w@��@�o@��\@�@��^@��7@��7@��7@�x�@�x�@��h@�X@�O�@�O�@�O�@�?}@�/@��@��@��@���@�9X@���@�t�@�K�@�"�@�@��H@�ȴ@�J@��-@���@�`B@��/@���@��@��;@��m@��;@��
@���@��@�|�@��@�+@�v�@�~�@�^5@�$�@�J@��@�@���@�p�@�?}@�X@�%@�Ĝ@�1@���@��m@��@��@�l�@��@��R@��\@��+@�V@�J@��@���@��h@�`B@�?}@�&�@���@��/@��9@��u@�r�@�bN@�j@��9@�?}@�O�@���@�x�@�p�@���@�?}@��7@�/@�O�@�%@�9X@�b@�I�@�Ĝ@��u@l�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bv�BbNBw�B~�B�uB��B�%B�B�%B�B�=B�hB��B��B��B��B��B��B��B��B��B�!B�B��B��B��B��B��B�1Bw�BiyBdZBT�BH�B@�B,B�B�BB�B�/B��B�jB��B�PB{�Bt�Bo�BbNBP�B<jB�BVBB  B
��B
�B
�TB
�B
ƨB
�B
��B
�{B
�hB
�B
p�B
aHB
\)B
VB
E�B
>wB
<jB
8RB
2-B
"�B
�B
PB
B	��B	�`B	��B	ƨB	�B	��B	u�B	ffB	`BB	VB	H�B	@�B	33B	+B	%�B	�B	oB	B�B�mB�5B�B��B��BɺBĜB��B�qB�^B�9B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�oB�hB�uB��B�PB�=B�DB�DB�DB�DB�=B�7B�B~�Bw�B� B�+B�Bu�BffBgmBe`BcTBaHB_;B[#BVBS�BS�BS�BQ�BO�BO�BP�BO�BL�BL�BI�BG�BE�BB�B>wB=qB<jB;dB:^B9XB7LB5?B49B33B2-B2-B1'B1'B1'B0!B0!B/B/B.B.B.B-B-B-B.B-B-B-B-B-B-B-B-B-B.B.B/B/B0!B2-B2-B1'B1'B2-B2-B1'B2-B49B5?B49B33B2-B33B49B49B5?B9XB;dB<jB?}BB�BB�BG�BK�BL�BM�BO�BP�BP�BZBbNBbNB`BB_;B`BBdZBcTBdZBe`BiyBk�Bp�Br�Bt�Bt�Bw�B~�B�B�%B�%B�=B�\B�uB�{B��B��B��B��B��B�3B�?B�LB�RB�^B�wBÖBȴBɺB��B��B�B�HB�yB�B�B�B�B�B��B��B��B��B	  B	B	1B	VB	VB	hB	{B	�B	�B	�B	�B	�B	"�B	'�B	'�B	)�B	-B	0!B	49B	9XB	>wB	A�B	B�B	A�B	A�B	@�B	?}B	A�B	F�B	O�B	XB	ZB	_;B	^5B	\)B	[#B	bNB	dZB	bNB	aHB	m�B	p�B	p�B	u�B	w�B	v�B	u�B	s�B	y�B	|�B	{�B	�B	�B	�B	~�B	�B	~�B	z�B	{�B	|�B	}�B	}�B	�B	�B	�B	�B	�JB	�VB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	�B	�!B	�9B	�FB	�LB	�RB	�XB	�RB	�LB	�LB	�LB	�RB	�dB	�jB	�qB	�}B	��B	��B	��B	��B	��B	B	B	B	ĜB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�B	�)B	�5B	�;B	�;B	�5B	�5B	�BB	�HB	�HB	�NB	�TB	�ZB	�ZB	�ZB	�ZB	�`B	�fB	�`B	�`B	�ZB	�`B	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B
B
  B
B
B
  B
B
B
1B

=B
h22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bv�BbNBw�B~�B�uB��B�%B�B�%B�B�=B�hB��B��B��B��B��B��B��B��B��B�!B�B��B��B��B��B��B�1Bw�BiyBdZBT�BH�B@�B,B�B�BB�B�/B��B�jB��B�PB{�Bt�Bo�BbNBP�B<jB�BVBB  B
��B
�B
�TB
�B
ƨB
�B
��B
�{B
�hB
�B
p�B
aHB
\)B
VB
E�B
>wB
<jB
8RB
2-B
"�B
�B
PB
B	��B	�`B	��B	ƨB	�B	��B	u�B	ffB	`BB	VB	H�B	@�B	33B	+B	%�B	�B	oB	B�B�mB�5B�B��B��BɺBĜB��B�qB�^B�9B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�oB�hB�uB��B�PB�=B�DB�DB�DB�DB�=B�7B�B~�Bw�B� B�+B�Bu�BffBgmBe`BcTBaHB_;B[#BVBS�BS�BS�BQ�BO�BO�BP�BO�BL�BL�BI�BG�BE�BB�B>wB=qB<jB;dB:^B9XB7LB5?B49B33B2-B2-B1'B1'B1'B0!B0!B/B/B.B.B.B-B-B-B.B-B-B-B-B-B-B-B-B-B.B.B/B/B0!B2-B2-B1'B1'B2-B2-B1'B2-B49B5?B49B33B2-B33B49B49B5?B9XB;dB<jB?}BB�BB�BG�BK�BL�BM�BO�BP�BP�BZBbNBbNB`BB_;B`BBdZBcTBdZBe`BiyBk�Bp�Br�Bt�Bt�Bw�B~�B�B�%B�%B�=B�\B�uB�{B��B��B��B��B��B�3B�?B�LB�RB�^B�wBÖBȴBɺB��B��B�B�HB�yB�B�B�B�B�B��B��B��B��B	  B	B	1B	VB	VB	hB	{B	�B	�B	�B	�B	�B	"�B	'�B	'�B	)�B	-B	0!B	49B	9XB	>wB	A�B	B�B	A�B	A�B	@�B	?}B	A�B	F�B	O�B	XB	ZB	_;B	^5B	\)B	[#B	bNB	dZB	bNB	aHB	m�B	p�B	p�B	u�B	w�B	v�B	u�B	s�B	y�B	|�B	{�B	�B	�B	�B	~�B	�B	~�B	z�B	{�B	|�B	}�B	}�B	�B	�B	�B	�B	�JB	�VB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	�B	�!B	�9B	�FB	�LB	�RB	�XB	�RB	�LB	�LB	�LB	�RB	�dB	�jB	�qB	�}B	��B	��B	��B	��B	��B	B	B	B	ĜB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�B	�)B	�5B	�;B	�;B	�5B	�5B	�BB	�HB	�HB	�NB	�TB	�ZB	�ZB	�ZB	�ZB	�`B	�fB	�`B	�`B	�ZB	�`B	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B
B
  B
B
B
  B
B
B
1B

=B
h22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.26 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191722                              AO  ARCAADJP                                                                    20181005191722    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191722  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191722  QCF$                G�O�G�O�G�O�8000            