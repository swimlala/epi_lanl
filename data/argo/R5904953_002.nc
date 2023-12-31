CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:06:16Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190616  20181005190616  5904953 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6432                            2B  A   APEX                            7467                            062512                          846 @ל�c�Dc1   @ל���nR@3�=p��
�c�S���1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   A   @�33@�  A   AffA@  A`  A�  A�  A���A�  A�  A�  A�  A�  B   B��B  B  B   B(ffB0  B8  B@  BH  BP  BXffB`  Bh  Bp  Bx  B�  B���B���B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33C �C  C  C  C  C
  C  C  C�fC�fC�fC  C  C  C  C  C�fC!�fC#�fC&  C(�C*�C,�C.�C0  C1�fC3�fC5�fC8  C:�C<  C>  C@  CB  CD  CF  CH�CJ�CL  CN  CP33CR�CT  CV�CX33CZ  C[�fC^  C`  Cb  Cd  Cf�Ch�Cj�Cl  Cm�fCo��Cq��Cs��Cu�fCx�Cy�fC|  C~33C��C��3C��3C��3C��fC�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C��3C�  C��C��C�  C�  C��C�  C��3C��3C�  C�  C�  C�  C��C��3C�  C�  C�  C�  C�  C��C��3C��C��fC��C��3C��C��3C��C��3C��C�  C��C��C��fC��3C�  C�  C��3C��3C��fC��fC��fC��fC��fC��3C��fC��fC�  C��C��C��C�  C�  C��3C�  C��C�  C��3C��3C�  C�  C�  C�  C��C��C�  C�  C��C��3C��3C�  C�  C��3C��3C�  C�  C�  C��C�  C�  C�  C��3C��fC�  C��C�  C��fC�  C��C��C��3C��fC�  C��C�  C��3C��fC��3C��C��C��3C�  C��C��C�  C�  D   D � D �3D� DfDy�D  D�fD��D� DfDy�D  D� D  D�fDfD�fD��D	y�D
fD
��D�D�fDfD�fDfD��D�D��D�D� D�3Ds3D�3Ds3D��Ds3D�3Dy�D  D�fDfD��D�D� D�3Dy�D��D�fDfDy�D��D� DfD�fD�3Dy�D  D��D�D��D  Dy�D   D �fD!  D!� D"  D"� D#  D#� D#��D$y�D%fD%��D&�D&�fD'fD'�fD(fD(� D(��D)y�D*  D*�fD+�D+��D,�D,��D-fD-�fD.  D.� D.��D/s3D/��D0� D1  D1� D1��D2y�D3  D3��D4  D4y�D4�3D5� D6fD6y�D7  D7�fD8  D8y�D9  D9� D:fD:� D;  D;� D<fD<� D=  D=�fD>  D>� D?fD?� D?��D@y�DAfDA� DA��DB� DCfDC� DC��DD� DE�DE��DF  DFy�DG  DG�fDG��DH� DIfDI� DI��DJ� DKfDK� DL  DL�fDMfDMy�DM��DNy�DN��DO� DPfDP�fDQfDQ� DR  DR� DR��DS� DT�DT�fDU  DUy�DU�3DV� DW�DW�fDX  DX� DX�3DY� DZfDZ�fD[fD[y�D[��D\� D]fD]� D^fD^�fD_  D_y�D`  D`��DafDa� Db  Db� Db��Dc� DdfDdy�De  De�fDf  Df� Dg  Dgy�Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dly�Dm  Dm�fDn  Dny�Dn��Doy�Dp  Dp�fDqfDq�fDr  Dry�Ds  Ds�fDt  Dty�Du  Du�fDvfDv� Dv��Dw� Dw�3Dy��D�F�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�ff@�ffA��A?33A_33A33A���A�fgA���A���Aϙ�Aߙ�AA���BfgB��B��B��B(33B/��B7��B?��BG��BO��BX33B_��Bg��Bo��Bw��B��B��3B��3B��fB��fB��fB��fB��B��fB��fB��fB��fB��fB��3B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��C �C�3C�3C�3C�3C	�3C�3C�3CٙCٙCٙC�3C�3C�3C�3C�3CٙC!ٙC#ٙC%�3C(�C*�C,�C.�C/�3C1ٙC3ٙC5ٙC7�3C:�C;�3C=�3C?�3CA�3CC�3CE�3CH�CJ�CK�3CM�3CP&fCR�CS�3CV�CX&fCY�3C[ٙC]�3C_�3Ca�3Cc�3Cf�Ch�Cj�Ck�3CmٙCo� Cq� Cs� CuٙCx�CyٙC{�3C~&fC�gC���C���C���C�� C���C���C���C���C���C���C���C���C���C�gC�gC���C���C���C���C���C���C�gC�gC���C���C�gC���C���C���C���C���C���C���C�gC���C���C���C���C���C���C�gC���C�4C�� C�gC���C�gC���C�gC���C�4C���C�gC�gC�� C���C���C���C���C���C�� C�� C�� C�� C�� C���C�� C�� C���C�4C�4C�gC���C���C���C���C�gC���C���C���C���C���C���C���C�gC�4C���C���C�gC���C���C���C���C���C���C���C���C���C�gC���C���C���C���C�� C���C�gC���C�� C���C�gC�gC���C�� C���C�gC���C���C�� C���C�4C�gC���C���C�gC�gC���C���C���D |�D � D|�D3DvgD��D�3D�gD|�D3DvgD��D|�D��D�3D3D�3D�gD	vgD
3D
��D	�D�3D3D�3D3D��D	�D��D	�D|�D� Dp D� Dp D�gDp D� DvgD��D�3D3D��D	�D|�D� DvgD�gD�3D3DvgD�gD|�D3D�3D� DvgD��D��D	�D��D��DvgD��D �3D ��D!|�D!��D"|�D"��D#|�D#�gD$vgD%3D%��D&	�D&�3D'3D'�3D(3D(|�D(�gD)vgD)��D*�3D+	�D+��D,	�D,��D-3D-�3D-��D.|�D.�gD/p D/�gD0|�D0��D1|�D1�gD2vgD2��D3��D3��D4vgD4� D5|�D63D6vgD6��D7�3D7��D8vgD8��D9|�D:3D:|�D:��D;|�D<3D<|�D<��D=�3D=��D>|�D?3D?|�D?�gD@vgDA3DA|�DA�gDB|�DC3DC|�DC�gDD|�DE	�DE��DE��DFvgDF��DG�3DG�gDH|�DI3DI|�DI�gDJ|�DK3DK|�DK��DL�3DM3DMvgDM�gDNvgDN�gDO|�DP3DP�3DQ3DQ|�DQ��DR|�DR�gDS|�DT	�DT�3DT��DUvgDU� DV|�DW	�DW�3DW��DX|�DX� DY|�DZ3DZ�3D[3D[vgD[�gD\|�D]3D]|�D^3D^�3D^��D_vgD_��D`��Da3Da|�Da��Db|�Db�gDc|�Dd3DdvgDd��De�3De��Df|�Df��DgvgDg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��DlvgDl��Dm�3Dm��DnvgDn�gDovgDo��Dp�3Dq3Dq�3Dq��DrvgDr��Ds�3Ds��DtvgDt��Du�3Dv3Dv|�Dv�gDw|�Dw� Dy��D�ED��)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AȑhAȝ�Aȟ�Aȡ�Aȡ�Aȣ�Aȥ�Aȧ�Aȩ�Aȩ�Aȩ�AȬAȮAȮAȰ!AȰ!AȰ!Aȩ�Aȩ�AȮAȰ!AȰ!AȮAȰ!AȰ!AȮAȮAȧ�Aȣ�Aȉ7A�hsA�7LAǬAƮA�jA�oA�ƨA�|�A��AĶFA�XA���A�r�AÕ�AÝ�A�p�A�S�A��AA�7LA��-A�^5A�oA�%A��yA�t�A�l�A�9XA�^5A��DA�K�A�A�A���A��A�  A�ȴA�C�A���A��A���A�`BA���A��A�t�A���A���A�1A�C�A�A��/A�-A�ffA��/A��A��hA���A�
=A�VA�VA�jA�?}A��A���A��A���A�+A��A�Q�A��#A�Q�A��hA�7LA�XA���A���A��;A��+AdZA}l�Az�`As&�Aj�HAg�FAfr�Ad�9Aa�^A`1'A_dZA[
=AVVATbAP��AL��AL��AK�-AJȴAJ1'AHZAEK�AC;dAB�HAB^5A@�9A=��A9��A8n�A7��A6��A5�wA5�A41A37LA2��A2�A/��A-33A+�A)�#A)+A(��A'S�A%��A$��A#�PA"��A"VA!x�A jA�A�AJA�!A��A��A�!A�PA��A^5AE�A(�A�-AS�A&�AVA��A�`AȴA�A�mA7LA1A\)A�At�A
�9A	��A�uA��A�jA�A�^AȴA�jAS�A%AC�AG�A�@�\)@�O�@�ƨ@���@�Z@�bN@��j@�j@�V@�Z@�@�ƨ@�l�@�p�@�M�@�@�V@�9X@�$�@�+@�7L@��
@׍P@�t�@�O�@��@��@�E�@��@�r�@ߕ�@��@��y@ؓu@�dZ@��H@֧�@Ձ@�b@�@�{@�hs@��@�l�@��`@˾w@�33@�
=@�ȴ@�^5@��H@�33@���@�n�@ȼj@�o@���@���@���@�(�@�1@�G�@�I�@�1@��;@���@��R@�V@�&�@���@�j@���@�z�@�v�@�`B@�1'@�v�@��7@�V@���@��@�&�@�X@��j@��9@�Ĝ@���@��/@���@���@��9@�Z@�\)@�
=@��@���@�ff@�5?@���@��@�G�@�G�@�7L@�/@�%@���@���@�?}@�7L@���@�A�@�b@��
@�o@���@�v�@�E�@�5?@��@���@�/@�%@��/@��@��u@�r�@�Q�@��w@�
=@��!@�-@��#@��^@���@��@�p�@�G�@�/@��/@�(�@��m@�l�@���@��@��@��@��@��y@���@��R@���@��\@�~�@�n�@�v�@�^5@�J@�`B@��j@��@�A�@���@��@��H@��+@�v�@�J@��#@���@���@���@�hs@�7L@�%@���@�  @���@��P@��@�dZ@�"�@�@��@���@��+@�n�@�=q@��@���@���@��h@�O�@���@��9@��9@��@�r�@��@�ƨ@��@�S�@���@��!@�$�@���@���@�hs@�G�@��@���@�r�@�Q�@�I�@�(�@�1@���@�t�@�;d@��@�n�@��@��@��@��#@���@��-@���@�X@�?}@��@��D@�Z@�1'@��@�|�@�;d@��@���@���@���@�n�@�-@��@�J@�@�x�@�?}@���@�Ĝ@�z�@�Q�@�I�@�1'@�b@���@���@�dZ@�33@���@���@��\@��+@�M�@��@��@���@�x�@�&�@��9@�Q�@�(�@���@��w@��P@�;d@��y@��@��R@���@�ff@�5?@�$�@�J@��#@��-@��7@��@��D@��w@�S�@�;d@�o@��@�~�@�J@��T@��^@��7@n?@^111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AȑhAȝ�Aȟ�Aȡ�Aȡ�Aȣ�Aȥ�Aȧ�Aȩ�Aȩ�Aȩ�AȬAȮAȮAȰ!AȰ!AȰ!Aȩ�Aȩ�AȮAȰ!AȰ!AȮAȰ!AȰ!AȮAȮAȧ�Aȣ�Aȉ7A�hsA�7LAǬAƮA�jA�oA�ƨA�|�A��AĶFA�XA���A�r�AÕ�AÝ�A�p�A�S�A��AA�7LA��-A�^5A�oA�%A��yA�t�A�l�A�9XA�^5A��DA�K�A�A�A���A��A�  A�ȴA�C�A���A��A���A�`BA���A��A�t�A���A���A�1A�C�A�A��/A�-A�ffA��/A��A��hA���A�
=A�VA�VA�jA�?}A��A���A��A���A�+A��A�Q�A��#A�Q�A��hA�7LA�XA���A���A��;A��+AdZA}l�Az�`As&�Aj�HAg�FAfr�Ad�9Aa�^A`1'A_dZA[
=AVVATbAP��AL��AL��AK�-AJȴAJ1'AHZAEK�AC;dAB�HAB^5A@�9A=��A9��A8n�A7��A6��A5�wA5�A41A37LA2��A2�A/��A-33A+�A)�#A)+A(��A'S�A%��A$��A#�PA"��A"VA!x�A jA�A�AJA�!A��A��A�!A�PA��A^5AE�A(�A�-AS�A&�AVA��A�`AȴA�A�mA7LA1A\)A�At�A
�9A	��A�uA��A�jA�A�^AȴA�jAS�A%AC�AG�A�@�\)@�O�@�ƨ@���@�Z@�bN@��j@�j@�V@�Z@�@�ƨ@�l�@�p�@�M�@�@�V@�9X@�$�@�+@�7L@��
@׍P@�t�@�O�@��@��@�E�@��@�r�@ߕ�@��@��y@ؓu@�dZ@��H@֧�@Ձ@�b@�@�{@�hs@��@�l�@��`@˾w@�33@�
=@�ȴ@�^5@��H@�33@���@�n�@ȼj@�o@���@���@���@�(�@�1@�G�@�I�@�1@��;@���@��R@�V@�&�@���@�j@���@�z�@�v�@�`B@�1'@�v�@��7@�V@���@��@�&�@�X@��j@��9@�Ĝ@���@��/@���@���@��9@�Z@�\)@�
=@��@���@�ff@�5?@���@��@�G�@�G�@�7L@�/@�%@���@���@�?}@�7L@���@�A�@�b@��
@�o@���@�v�@�E�@�5?@��@���@�/@�%@��/@��@��u@�r�@�Q�@��w@�
=@��!@�-@��#@��^@���@��@�p�@�G�@�/@��/@�(�@��m@�l�@���@��@��@��@��@��y@���@��R@���@��\@�~�@�n�@�v�@�^5@�J@�`B@��j@��@�A�@���@��@��H@��+@�v�@�J@��#@���@���@���@�hs@�7L@�%@���@�  @���@��P@��@�dZ@�"�@�@��@���@��+@�n�@�=q@��@���@���@��h@�O�@���@��9@��9@��@�r�@��@�ƨ@��@�S�@���@��!@�$�@���@���@�hs@�G�@��@���@�r�@�Q�@�I�@�(�@�1@���@�t�@�;d@��@�n�@��@��@��@��#@���@��-@���@�X@�?}@��@��D@�Z@�1'@��@�|�@�;d@��@���@���@���@�n�@�-@��@�J@�@�x�@�?}@���@�Ĝ@�z�@�Q�@�I�@�1'@�b@���@���@�dZ@�33@���@���@��\@��+@�M�@��@��@���@�x�@�&�@��9@�Q�@�(�@���@��w@��P@�;d@��y@��@��R@���@�ff@�5?@�$�@�J@��#@��-@��7@��@��D@��w@�S�@�;d@�o@��@�~�@�J@��T@��^@��7@n?@^111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�3B
�3B
�3B
�3B
�3B
�9B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�9B
�!B
�9B
�9B
�9B
�9B
�9B
�9B
�9B
�?B
�FB
�FB
�RB
�qB
ŢB
�BB#�B8RBN�B_;Bp�B�+B��B�BÖB��BoB)�B8RB9XB5?B@�BS�Bm�Bs�Bo�B_;Bl�Bv�Bn�BiyBaHBVB^5Bo�Bm�B_;BL�B6FB`BBhsBT�BN�BG�B;dB'�BB��B�B�yB�#BÖB�'B�?B��B��B��B��B�uB�DB~�Bu�BffBXBI�BD�B?}B33BbB
��B
�fB
��B
��B
�oB
n�B
_;B
W
B
R�B
G�B
33B
&�B
�B
B	��B	��B	�1B	}�B	s�B	[#B	O�B	K�B	+B	bB��B�B�fB�B��B��B��B�yB�5B��B�B�
B��B�XB��B�{B�\B�1B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�bB�VB�PB�VB�JB�+B�B�+B�1B�1B�1B�+B�1B�1B�7B�7B�7B�7B�1B�1B�VB�\B�{B��B��B��B��B��B��B��B��B��B��B��B�oB�=B�JB�bB�oB�oB�hB�VB�JB�7B�JB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�?BŢB��B�#B�B��B	  B	B��B��B�B�B�B�B�B�B�B�yB�sB�sB�ZB�5B�#B�)B�NB�mB�B�B��B��B��B	B	  B��B�B�B�B�B��B	  B��B��B��B��B�B�B��B	B	bB	oB	hB	\B	JB	JB	PB	\B	�B	�B	 �B	)�B	33B	6FB	7LB	8RB	9XB	<jB	A�B	B�B	D�B	J�B	L�B	N�B	P�B	Q�B	Q�B	R�B	T�B	XB	YB	ZB	[#B	^5B	_;B	`BB	e`B	l�B	n�B	o�B	o�B	o�B	r�B	u�B	v�B	x�B	y�B	{�B	}�B	�B	�B	�B	�B	�B	�B	�B	�B	�+B	�1B	�=B	�DB	�JB	�PB	�PB	�PB	�VB	�VB	�\B	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�3B	�LB	�RB	�RB	�RB	�RB	�XB	�dB	�jB	�jB	�}B	��B	��B	��B	��B	��B	B	ÖB	ŢB	ƨB	ǮB	ǮB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�#B	�#B	�#B	�;B	�BB	�NB	�TB	�ZB	�`B	�`B	�`B	�`B	�fB	�fB	�fB	�mB	�mB	�mB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
+B
%B
B
%B
+B
1B
1B
	7B

=B

=B
PB
B
VB
1[222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B
�3B
�3B
�3B
�3B
�3B
�9B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�9B
�!B
�9B
�9B
�9B
�9B
�9B
�9B
�9B
�?B
�FB
�FB
�RB
�qB
ŢB
�BB#�B8RBN�B_;Bp�B�+B��B�BÖB��BoB)�B8RB9XB5?B@�BS�Bm�Bs�Bo�B_;Bl�Bv�Bn�BiyBaHBVB^5Bo�Bm�B_;BL�B6FB`BBhsBT�BN�BG�B;dB'�BB��B�B�yB�#BÖB�'B�?B��B��B��B��B�uB�DB~�Bu�BffBXBI�BD�B?}B33BbB
��B
�fB
��B
��B
�oB
n�B
_;B
W
B
R�B
G�B
33B
&�B
�B
B	��B	��B	�1B	}�B	s�B	[#B	O�B	K�B	+B	bB��B�B�fB�B��B��B��B�yB�5B��B�B�
B��B�XB��B�{B�\B�1B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�bB�VB�PB�VB�JB�+B�B�+B�1B�1B�1B�+B�1B�1B�7B�7B�7B�7B�1B�1B�VB�\B�{B��B��B��B��B��B��B��B��B��B��B��B�oB�=B�JB�bB�oB�oB�hB�VB�JB�7B�JB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�?BŢB��B�#B�B��B	  B	B��B��B�B�B�B�B�B�B�B�yB�sB�sB�ZB�5B�#B�)B�NB�mB�B�B��B��B��B	B	  B��B�B�B�B�B��B	  B��B��B��B��B�B�B��B	B	bB	oB	hB	\B	JB	JB	PB	\B	�B	�B	 �B	)�B	33B	6FB	7LB	8RB	9XB	<jB	A�B	B�B	D�B	J�B	L�B	N�B	P�B	Q�B	Q�B	R�B	T�B	XB	YB	ZB	[#B	^5B	_;B	`BB	e`B	l�B	n�B	o�B	o�B	o�B	r�B	u�B	v�B	x�B	y�B	{�B	}�B	�B	�B	�B	�B	�B	�B	�B	�B	�+B	�1B	�=B	�DB	�JB	�PB	�PB	�PB	�VB	�VB	�\B	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�3B	�LB	�RB	�RB	�RB	�RB	�XB	�dB	�jB	�jB	�}B	��B	��B	��B	��B	��B	B	ÖB	ŢB	ƨB	ǮB	ǮB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�#B	�#B	�#B	�;B	�BB	�NB	�TB	�ZB	�`B	�`B	�`B	�`B	�fB	�fB	�fB	�mB	�mB	�mB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
+B
%B
B
%B
+B
1B
1B
	7B

=B

=B
PB
B
VB
1[222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.05 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190616                              AO  ARCAADJP                                                                    20181005190616    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190616  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190616  QCF$                G�O�G�O�G�O�8000            