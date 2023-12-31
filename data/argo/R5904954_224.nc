CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:40Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005191740  20181005191740  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��d�s]�1   @��e��@@@5��E��d��t�j1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�ff@���A   A   A@  A`  A�  A�33A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bw��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B���B�  B�  C   C  C  C  C  C
  C  C�fC�fC  C  C  C�C�C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CS�fCV  CX  CZ  C\  C^  C`  Cb  Cd�Cf�Ch�Cj  Cl  Cn  Cp  Cr  Ct�Cv  Cw�fCz  C|  C~  C�  C�  C��3C��3C�  C��C��C�  C�  C��3C��3C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C��C��C��C��C�  C��C��C�  C��C�  C��3C�  C�  C��C��C�  C��3C�  C�  C�  C��C�  C��3C��3C�  C�  C��C��C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C��3C�  C��3C�  C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C��C��3C��3C��3C��C�  C��3C��3C�  C��C�  C��C��C��C��C��C�  C��3C�  C�  C�  C�  C��C��3C�  C�  C�  C��3C�  C�  C��3C��3C�  C��C�  C��3C��3C��3C��C�  C��C�  C�  C��C�  D   D � D� D��Dy�D  D� D��Dy�D��Dy�D��D� D��D� D  Dy�D  D�fD�D�fD��D� DfD� D��Ds3D��Dy�D  D� DfD� D��Dy�D  D�fD�D�fDfD�fD   D y�D ��D!�fD"fD"�fD#fD#�fD$  D$� D%  D%y�D&fD&�fD'  D'y�D'��D(� D)fD)y�D)��D*y�D+  D+� D,fD,�fD-�D-�fD-��D.y�D/  D/� D/��D0� D1  D1�fD2  D2y�D3  D3�fD3��D4� D5  D5y�D6  D6y�D6��D7� D7��D8y�D9  D9� D:  D:� D;  D;y�D<  D<� D=  D=��D>fD>�fD?  D?y�D@  D@�fDA  DAy�DA��DBs3DB��DCy�DC��DD� DE  DE� DE��DFy�DG  DG� DG��DHy�DI  DI�fDJfDJ�fDKfDK� DL  DL�fDMfDM�fDNfDN�fDO  DO� DPfDP�fDQfDQ� DR  DR�fDSfDS� DT  DT� DU  DU� DVfDV� DW  DW� DXfDX�fDYfDY� DZ  DZ� D[  D[� D\  D\� D]  D]�fD^  D^� D_  D_y�D_��D`y�Da  Da� Da��Db� Dc  Dc� Dd  Ddy�Dd��De� Df  Df� Dg  Dg� Dh  Dh� Dh��Di� DjfDj�fDkfDky�DlfDly�Dl��Dm� Dm��Dn�fDo  Do� Dp  Dpy�Dq  Dq�fDrfDr��DsfDs� Dt  Dt� Du  Du� Dv  Dvy�DwfDws3Dy~D�B�D�|)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�33@�ffA33A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��BwfgB��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��3B��3B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB�3B��fB��fB��fB�3B��fB��fB��fC�3C�3C�3C�3C	�3C�3CٙCٙC�3C�3C�3C�C�C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CSٙCU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cd�Cf�Ch�Ci�3Ck�3Cm�3Co�3Cq�3Ct�Cu�3CwٙCy�3C{�3C}�3C�3C���C���C���C���C�gC�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC�gC�gC���C���C�gC�gC�gC�gC���C�gC�gC���C�gC���C���C���C���C�gC�gC���C���C���C���C���C�gC���C���C���C���C���C�gC�gC���C���C���C�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C���C���C���C���C���C�gC���C���C���C�gC���C���C���C���C�gC���C�gC�gC�4C�gC�gC���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���C���C���C�gC���C���C���C���C�gC���C�gC���C���C�gC���C���D |�D|�D�gDvgD��D|�D�gDvgD�gDvgD�gD|�D�gD|�D��DvgD��D�3D	�D�3D�gD|�D3D|�D�gDp D�gDvgD��D|�D3D|�D�gDvgD��D�3D	�D�3D3D�3D��D vgD �gD!�3D"3D"�3D#3D#�3D#��D$|�D$��D%vgD&3D&�3D&��D'vgD'�gD(|�D)3D)vgD)�gD*vgD*��D+|�D,3D,�3D-	�D-�3D-�gD.vgD.��D/|�D/�gD0|�D0��D1�3D1��D2vgD2��D3�3D3�gD4|�D4��D5vgD5��D6vgD6�gD7|�D7�gD8vgD8��D9|�D9��D:|�D:��D;vgD;��D<|�D<��D=��D>3D>�3D>��D?vgD?��D@�3D@��DAvgDA�gDBp DB�gDCvgDC�gDD|�DD��DE|�DE�gDFvgDF��DG|�DG�gDHvgDH��DI�3DJ3DJ�3DK3DK|�DK��DL�3DM3DM�3DN3DN�3DN��DO|�DP3DP�3DQ3DQ|�DQ��DR�3DS3DS|�DS��DT|�DT��DU|�DV3DV|�DV��DW|�DX3DX�3DY3DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]�3D]��D^|�D^��D_vgD_�gD`vgD`��Da|�Da�gDb|�Db��Dc|�Dc��DdvgDd�gDe|�De��Df|�Df��Dg|�Dg��Dh|�Dh�gDi|�Dj3Dj�3Dk3DkvgDl3DlvgDl�gDm|�Dm�gDn�3Dn��Do|�Do��DpvgDp��Dq�3Dr3Dr��Ds3Ds|�Ds��Dt|�Dt��Du|�Du��DvvgDw3Dwp Dyz�D�AGD�z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aʉ7AʋDAʏ\Aʏ\Aʕ�Aʝ�Aʡ�Aʥ�AʬAʮAʲ-Aʲ-Aʲ-AʶFAʸRAʶFAʶFAʸRAʸRAʺ^Aʺ^Aʺ^AʸRAʸRAʸRA��yA�$�A˾wA�bA��A��yA˾wA�~�A�(�A�{A�=qAɲ-A�bNA��mA��A�&�AƟ�A�jA��Aá�A�oA�bNA�K�A��TA��A�ȴA�ĜA���A�dZA�r�A�bNA�\)A���A�5?A�bA�\)A�C�A���A���A�/A�~�A�G�A��;A���A�E�A�$�A��A�VA��;A�l�A�r�A�G�A�Q�A���A�VA���A��TA�VA��\A��9A�ZA�S�A�C�A�+A�9XA�l�A�=qA�O�A�G�A�ȴA�t�A���A�S�A��wA�|�A��!A���A�bNA��;A��hA��A�(�A�ĜA��A�~�A�XA��hA��DA��\A�G�A��A�
=A�;A~�DA{O�Azr�Ay��Ayt�Ax�yAw|�Av�jAudZAt  AsK�Ar5?Aq;dAohsAn�Al�AjȴAh��Ah^5Ah=qAg�AgAfr�AfI�Aex�Ac�Aa?}A`JA^A]&�A\=qA\{A[�
AY�AX�yAWt�AQ�#APAN��AM��ALffAK�-AK�AJ��AI��AH�AFr�AC�hAA��A@�RA?��A>�A<�DA:ȴA9�A8�\A6Q�A57LA3��A2n�A0A�A/VA-�
A,r�A+��A*r�A)��A'�7A%��A$�!A"��A"Q�A!�#A �9A�HA�-A�`AQ�A�A`BAƨA"�AbAS�A-A�A�A�A�A5?A�#A�7AVA�A�jA��A
�9A
$�A	\)AJA�A7LA�`A�A�PA ȴ@��T@�;d@�E�@���@�&�@��;@�&�@���@�1'@��H@陚@�u@�Z@�1'@�@��y@�~�@�V@�b@�+@���@�+@���@ڟ�@��@ش9@�  @׍P@�33@�E�@�hs@с@�J@���@�G�@���@�z�@� �@��@���@�
=@�V@��@�hs@�`B@�x�@�p�@�G�@��9@�ƨ@���@�n�@�{@�hs@���@�bN@�  @��@�|�@���@�dZ@���@�l�@�dZ@���@���@���@���@���@���@��\@�~�@�n�@��@��-@���@�x�@�7L@��@�Ĝ@���@��D@�j@�Z@�Q�@�b@��@�C�@���@�5?@��@��j@��u@�z�@��@�|�@�;d@���@�n�@�$�@���@��h@�j@�S�@��@��@���@�;d@�5?@��7@���@�9X@��@�M�@�-@�-@��@��@�%@�Ĝ@��u@�bN@�b@��
@���@�S�@�K�@�"�@��@��@���@�
=@�
=@�"�@�S�@�S�@�S�@�\)@�dZ@�l�@�l�@�l�@�\)@�
=@�ȴ@�ȴ@���@�$�@��T@��7@�V@��@��@��u@�1@��F@��@��!@�^5@�{@��@��^@�bN@� �@�b@�1@�1@���@��
@�t�@�+@�ȴ@���@�ff@���@��T@�@���@�x�@�`B@�O�@�G�@�?}@��@��`@��9@���@���@��u@�r�@�A�@�1@�ƨ@�l�@���@�$�@��T@��#@��7@�G�@�/@��@���@��`@��/@��@�Ĝ@�9X@���@�C�@�"�@��@���@�5?@��-@�@���@��T@��@��@���@��@���@�1'@�  @�ƨ@���@��;@�b@��
@���@�K�@��y@���@�ff@�-@��@��@��@��@��@���@��^@��^@��-@�X@��9@�� @w�k@gn/111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aʉ7AʋDAʏ\Aʏ\Aʕ�Aʝ�Aʡ�Aʥ�AʬAʮAʲ-Aʲ-Aʲ-AʶFAʸRAʶFAʶFAʸRAʸRAʺ^Aʺ^Aʺ^AʸRAʸRAʸRA��yA�$�A˾wA�bA��A��yA˾wA�~�A�(�A�{A�=qAɲ-A�bNA��mA��A�&�AƟ�A�jA��Aá�A�oA�bNA�K�A��TA��A�ȴA�ĜA���A�dZA�r�A�bNA�\)A���A�5?A�bA�\)A�C�A���A���A�/A�~�A�G�A��;A���A�E�A�$�A��A�VA��;A�l�A�r�A�G�A�Q�A���A�VA���A��TA�VA��\A��9A�ZA�S�A�C�A�+A�9XA�l�A�=qA�O�A�G�A�ȴA�t�A���A�S�A��wA�|�A��!A���A�bNA��;A��hA��A�(�A�ĜA��A�~�A�XA��hA��DA��\A�G�A��A�
=A�;A~�DA{O�Azr�Ay��Ayt�Ax�yAw|�Av�jAudZAt  AsK�Ar5?Aq;dAohsAn�Al�AjȴAh��Ah^5Ah=qAg�AgAfr�AfI�Aex�Ac�Aa?}A`JA^A]&�A\=qA\{A[�
AY�AX�yAWt�AQ�#APAN��AM��ALffAK�-AK�AJ��AI��AH�AFr�AC�hAA��A@�RA?��A>�A<�DA:ȴA9�A8�\A6Q�A57LA3��A2n�A0A�A/VA-�
A,r�A+��A*r�A)��A'�7A%��A$�!A"��A"Q�A!�#A �9A�HA�-A�`AQ�A�A`BAƨA"�AbAS�A-A�A�A�A�A5?A�#A�7AVA�A�jA��A
�9A
$�A	\)AJA�A7LA�`A�A�PA ȴ@��T@�;d@�E�@���@�&�@��;@�&�@���@�1'@��H@陚@�u@�Z@�1'@�@��y@�~�@�V@�b@�+@���@�+@���@ڟ�@��@ش9@�  @׍P@�33@�E�@�hs@с@�J@���@�G�@���@�z�@� �@��@���@�
=@�V@��@�hs@�`B@�x�@�p�@�G�@��9@�ƨ@���@�n�@�{@�hs@���@�bN@�  @��@�|�@���@�dZ@���@�l�@�dZ@���@���@���@���@���@���@��\@�~�@�n�@��@��-@���@�x�@�7L@��@�Ĝ@���@��D@�j@�Z@�Q�@�b@��@�C�@���@�5?@��@��j@��u@�z�@��@�|�@�;d@���@�n�@�$�@���@��h@�j@�S�@��@��@���@�;d@�5?@��7@���@�9X@��@�M�@�-@�-@��@��@�%@�Ĝ@��u@�bN@�b@��
@���@�S�@�K�@�"�@��@��@���@�
=@�
=@�"�@�S�@�S�@�S�@�\)@�dZ@�l�@�l�@�l�@�\)@�
=@�ȴ@�ȴ@���@�$�@��T@��7@�V@��@��@��u@�1@��F@��@��!@�^5@�{@��@��^@�bN@� �@�b@�1@�1@���@��
@�t�@�+@�ȴ@���@�ff@���@��T@�@���@�x�@�`B@�O�@�G�@�?}@��@��`@��9@���@���@��u@�r�@�A�@�1@�ƨ@�l�@���@�$�@��T@��#@��7@�G�@�/@��@���@��`@��/@��@�Ĝ@�9X@���@�C�@�"�@��@���@�5?@��-@�@���@��T@��@��@���@��@���@�1'@�  @�ƨ@���@��;@�b@��
@���@�K�@��y@���@�ff@�-@��@��@��@��@��@���@��^@��^@��-@�X@��9@�� @w�k@gn/111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�7B�7B�7B�7B�7B�7B�7B�7B�7B�7B�7B�7B�7B�=B�=B�7B�1B�1B�1B�1B�1B�1B�1B�=B�=B��B�?BBA�BXBdZBk�Bo�Bz�B��B��B��B��B��B��B�?B�}B��B��B��B�B�`B�B�B�B�B��B��B�jB�FB�?B�9B�3B�!B�B��B��B�RB�RB�^B�qB�FB�9B��B��B��B��B��B��B��B�%B|�Bs�BjBdZBS�BJ�B:^B/B�B�BDB��B��B�uBffBR�BG�BB�BE�BF�BF�BB�B<jB-B#�B�B  B
��B
��B
�B
�TB
�)B
��B
�wB
�^B
�!B
��B
��B
�oB
�\B
�VB
�DB
�B
l�B
ffB
aHB
_;B
ZB
Q�B
J�B
B�B
9XB
33B
,B
$�B
�B
\B
B	��B	�B	�B	�yB	�mB	�NB	�/B	�)B	��B	ɺB	�XB	�'B	��B	��B	��B	��B	��B	�+B	}�B	p�B	I�B	=qB	7LB	.B	(�B	%�B	$�B	 �B	�B	uB	DB��B��B��B��B�B�yB�HB�/B�B��B��BƨBÖB�}B�^B�?B�B�'B�B�B��B��B��B��B��B��B�uB�bB�VB�DB�DB�=B�7B�B�B~�B� Bz�Bw�Bu�Br�Bp�Bq�Bp�Bo�Bn�Bm�Bk�BjBiyBhsBe`BdZBaHB]/BZBYBXBZBXBS�BQ�BR�BQ�BR�BR�BP�BO�BN�BM�BN�BO�BO�BO�BO�BN�BN�BN�BN�BO�BQ�BXBZBZB[#B[#B[#BZBYBW
B^5B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�-B�LB�^B�wB�}BƨB��B�#B�BB�fB�B�B��B��B	  B	B	%B	1B	DB	oB	uB	{B	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	"�B	$�B	)�B	-B	1'B	5?B	:^B	=qB	>wB	B�B	D�B	D�B	E�B	I�B	L�B	L�B	J�B	G�B	B�B	<jB	9XB	8RB	;dB	;dB	<jB	@�B	D�B	H�B	K�B	K�B	J�B	K�B	O�B	Q�B	VB	XB	[#B	]/B	]/B	^5B	`BB	aHB	cTB	n�B	t�B	u�B	x�B	|�B	}�B	~�B	~�B	�B	�B	�B	�B	�B	�%B	�7B	�7B	�1B	�+B	�+B	�%B	�+B	�1B	�7B	�DB	�JB	�JB	�PB	�VB	�\B	�\B	�VB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�-B	�-B	�9B	�LB	�LB	�LB	�XB	�dB	�jB	�qB	�qB	�qB	�wB	B	ÖB	ĜB	ĜB	ŢB	ƨB	ƨB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�#B	�)B	�/B	�/B	�;B	�5B	�)B	�#B	�/B	�5B	�;B	�HB	�ZB	�fB	�mB	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B
~B
#T222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B�7B�7B�7B�7B�7B�7B�7B�7B�7B�7B�7B�7B�7B�=B�=B�7B�1B�1B�1B�1B�1B�1B�1B�=B�=B��B�?BBA�BXBdZBk�Bo�Bz�B��B��B��B��B��B��B�?B�}B��B��B��B�B�`B�B�B�B�B��B��B�jB�FB�?B�9B�3B�!B�B��B��B�RB�RB�^B�qB�FB�9B��B��B��B��B��B��B��B�%B|�Bs�BjBdZBS�BJ�B:^B/B�B�BDB��B��B�uBffBR�BG�BB�BE�BF�BF�BB�B<jB-B#�B�B  B
��B
��B
�B
�TB
�)B
��B
�wB
�^B
�!B
��B
��B
�oB
�\B
�VB
�DB
�B
l�B
ffB
aHB
_;B
ZB
Q�B
J�B
B�B
9XB
33B
,B
$�B
�B
\B
B	��B	�B	�B	�yB	�mB	�NB	�/B	�)B	��B	ɺB	�XB	�'B	��B	��B	��B	��B	��B	�+B	}�B	p�B	I�B	=qB	7LB	.B	(�B	%�B	$�B	 �B	�B	uB	DB��B��B��B��B�B�yB�HB�/B�B��B��BƨBÖB�}B�^B�?B�B�'B�B�B��B��B��B��B��B��B�uB�bB�VB�DB�DB�=B�7B�B�B~�B� Bz�Bw�Bu�Br�Bp�Bq�Bp�Bo�Bn�Bm�Bk�BjBiyBhsBe`BdZBaHB]/BZBYBXBZBXBS�BQ�BR�BQ�BR�BR�BP�BO�BN�BM�BN�BO�BO�BO�BO�BN�BN�BN�BN�BO�BQ�BXBZBZB[#B[#B[#BZBYBW
B^5B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�-B�LB�^B�wB�}BƨB��B�#B�BB�fB�B�B��B��B	  B	B	%B	1B	DB	oB	uB	{B	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	"�B	$�B	)�B	-B	1'B	5?B	:^B	=qB	>wB	B�B	D�B	D�B	E�B	I�B	L�B	L�B	J�B	G�B	B�B	<jB	9XB	8RB	;dB	;dB	<jB	@�B	D�B	H�B	K�B	K�B	J�B	K�B	O�B	Q�B	VB	XB	[#B	]/B	]/B	^5B	`BB	aHB	cTB	n�B	t�B	u�B	x�B	|�B	}�B	~�B	~�B	�B	�B	�B	�B	�B	�%B	�7B	�7B	�1B	�+B	�+B	�%B	�+B	�1B	�7B	�DB	�JB	�JB	�PB	�VB	�\B	�\B	�VB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�-B	�-B	�9B	�LB	�LB	�LB	�XB	�dB	�jB	�qB	�qB	�qB	�wB	B	ÖB	ĜB	ĜB	ŢB	ƨB	ƨB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�#B	�)B	�/B	�/B	�;B	�5B	�)B	�#B	�/B	�5B	�;B	�HB	�ZB	�fB	�mB	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B
~B
#T222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.05 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191740                              AO  ARCAADJP                                                                    20181005191740    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191740  QCP$                G�O�G�O�G�O�F03E            AO  ARGQQCPL                                                                    20181005191740  QCF$                G�O�G�O�G�O�8000            