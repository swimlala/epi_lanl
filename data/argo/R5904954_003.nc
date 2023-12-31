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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005191648  20181005191648  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @ם�1M�i1   @ם����@3�x����cɑhr�!1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   A   @333@�  @�  A   A   AA��A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�33B�  B���B�  B�33B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B���B�  B�  B�33C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C �C"  C$  C&  C(  C*  C,  C.  C0  C1�fC3�fC6  C8�C:�C<�C>  C?�fCA�fCC�fCE�fCG�fCJ  CL  CN  CP�CR  CT  CV  CX  CY�fC\  C^  C`  Cb  Cd  Cf�Ch�Cj  Cl  Cm�fCo�fCq�fCt  Cv  Cx  Cz  C{�fC}�fC�fC�  C��C�  C�  C�  C�  C��C�  C��3C�  C��C�  C��C��C��C��C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C��C��C��C��C�  C��3C��3C��C�  C��3C��3C��3C�  C��C�  C��3C�  C��C��3C�  C�  C�  C��3C�  C��C��C�  C��3C��3C��3C��3C��3C��3C��fC��3C��fC�  C�  C�  C��3C��3C�  C��C��3C��3C��C��C��C�  C�  C��3C��3C�  C��C��3C�  C��C��3C�  C�  C��C�  C��3C�  C��C��C��C��C�  C��3C�  C�  C��3C��C��C��C�  C��C��C��3C�  C�  C��3C��C�  C��3C��C��3C��C�  C��C�  C�  C��3C�  C�  C�  D   D � D  D� D  D� D��D� D  D� D��D� D  D� D  D� D  D� D	  D	� D
  D
y�DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D�fDfD� D  D� D  D� D  Dy�D   D � D!  D!y�D"  D"� D#  D#� D$  D$� D%  D%� D&  D&�fD'  D'� D'��D(� D)  D)y�D*  D*� D+  D+� D,  D,� D-  D-y�D.  D.� D.��D/� D0fD0� D1  D1�fD2  D2�fD3  D3� D4  D4� D5  D5� D5��D6y�D7  D7y�D8fD8�fD8�3D9y�D:fD:� D:��D;�fD;��D<� D=fD=��D>fD>� D>��D?y�D?��D@y�DAfDA�fDB  DBy�DB��DC�fDD�DD�fDE  DEy�DE��DF� DGfDG�fDHfDHy�DH�3DIy�DI�3DJy�DK  DK� DLfDL��DMfDMy�DM��DNy�DO  DO�fDPfDP�fDQ  DQy�DQ��DRy�DS  DS� DTfDT�fDUfDUy�DU��DV�fDWfDW�fDX  DX� DY�DY�fDZfDZ�fD[fD[�fD\fD\y�D]  D]� D^  D^� D^��D_� D_��D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  Dey�De��Df� Dg  Dg� Dh  Dh� Di  Di�fDj  Dj� Dk  Dk�fDlfDl� Dm  Dmy�Dn  Dn� Do  Do� Dp  Dpy�DqfDq� Dq��Dr� Ds  Ds� Dt  Dty�DufDu� Du��Dv�fDw  Dws3Dyt{D�?
D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @6ff@���@���A ��A ��ABfgA`��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB 33B33B33B33B 33B(33B033B833B@33BH33BP33BX33B`33Bh33Bp33Bx33B��B��B��B��B��B��B��B�L�B��B��B�L�B��B��gB��B�L�B��B��B��B��B��B��B��B��gB��B��B��B��B��B��gB��B��B�L�C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C &gC"�C$�C&�C(�C*�C,�C.�C0�C1�3C3�3C6�C8&gC:&gC<&gC>�C?�3CA�3CC�3CE�3CG�3CJ�CL�CN�CP&gCR�CT�CV�CX�CY�3C\�C^�C`�Cb�Cd�Cf&gCh&gCj�Cl�Cm�3Co�3Cq�3Ct�Cv�Cx�Cz�C{�3C}�3C�3C�fC�3C�fC�fC�fC�fC�3C�fC���C�fC�3C�fC�3C�3C�3C�  C�fC���C�fC�fC�fC�3C�fC�fC�fC�fC�3C�fC�fC�fC�3C�3C�3C�3C�fC���C���C�3C�fC���C���C���C�fC�3C�fC���C�fC�3C���C�fC�fC�fC���C�fC�3C�3C�fC���C���C���C���C���C���C���C���C���C�fC�fC�fC���C���C�fC�3C���C���C�3C�  C�3C�fC�fC���C���C�fC�3C���C�fC�3C���C�fC�fC�  C�fC���C�fC�3C�3C�  C�3C�fC���C�fC�fC���C�3C�3C�3C�fC�3C�3C���C�fC�fC���C�3C�fC���C�3C���C�3C�fC�3C�fC�fC���C�fC�fC�fD 3D �3D3D�3D3D�3D��D�3D3D�3D��D�3D3D�3D3D�3D3D�3D	3D	�3D
3D
|�D	�D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D��D3D�3D3D�3D	�D�3D3D�3D3D�3D3D�3D3D�3D3D��D	�D�3D3D�3D3D�3D3D|�D 3D �3D!3D!|�D"3D"�3D#3D#�3D$3D$�3D%3D%�3D&3D&��D'3D'�3D'��D(�3D)3D)|�D*3D*�3D+3D+�3D,3D,�3D-3D-|�D.3D.�3D.��D/�3D0	�D0�3D13D1��D23D2��D33D3�3D43D4�3D53D5�3D5��D6|�D73D7|�D8	�D8��D8�fD9|�D:	�D:�3D:��D;��D;��D<�3D=	�D=� D>	�D>�3D>��D?|�D?��D@|�DA	�DA��DB3DB|�DB��DC��DD DD��DE3DE|�DE��DF�3DG	�DG��DH	�DH|�DH�fDI|�DI�fDJ|�DK3DK�3DL	�DL� DM	�DM|�DM��DN|�DO3DO��DP	�DP��DQ3DQ|�DQ��DR|�DS3DS�3DT	�DT��DU	�DU|�DU��DV��DW	�DW��DX3DX�3DY DY��DZ	�DZ��D[	�D[��D\	�D\|�D]3D]�3D^3D^�3D^��D_�3D_��D`�3Da3Da�3Db3Db�3Dc3Dc�3Dd3Dd�3De3De|�De��Df�3Dg3Dg�3Dh3Dh�3Di3Di��Dj3Dj�3Dk3Dk��Dl	�Dl�3Dm3Dm|�Dn3Dn�3Do3Do�3Dp3Dp|�Dq	�Dq�3Dq��Dr�3Ds3Ds�3Dt3Dt|�Du	�Du�3Du��Dv��Dw3DwvfDyw�D�@�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�A�A�7LA�5?A�33A�5?A�/A��A�A��;Aț�A�K�AǾwA���A�x�A�hsA�^5A�M�A�A�A�5?A�(�A��A�{A��A��A��A�+A�ZA�VA�;dA�-A�&�A�oA��A���AœuA�z�A�hsA�^5A�XA�G�A�%Aě�A�=qA��A��AÏ\A�~�A�1'A�JA�S�A�~�A��PA���A�%A���A�7LA��A�E�A�33A��mA�"�A�"�A���A��A��DA�-A��A�=qA��A��/A���A���A�1'A�jA��yA���A�1A��A��/A�oA�VA�33A�\)A�hsA��A���A���A��A��DA���A�;dA�+A~��A~��A};dAu33Ar9XApI�An�9Am;dAl{AkVAhQ�Af9XAd�HAb��A^��A[�TAZz�AY`BAXJAV-AS;dAQC�AN�DAK`BAI;dAG�FAFM�AA�mA>�A=l�A<-A;hsA:ȴA8��A4�A3`BA2ĜA2�!A2~�A1��A1\)A1�A0ZA/�^A.ȴA,�A+|�A*JA)|�A(�A(��A(r�A($�A'��A'S�A&1A$ �A#�A#oA"��A ��A�`AVAA�A7LAA�AS�A33A��A=qA�jA�uAp�A�!A=qA
r�A�9AdZA%A�jAn�A  A�wA�A�At�A|�Ax�A?}A�uA�A�A�A&�A I�@��F@���@�9X@�ff@��@��T@��@�ȴ@��T@���@���@�&�@���@�S�@�"�@�^5@�@홚@��/@�P@�=q@�p�@���@�b@��
@�~�@�hs@�z�@�V@�bN@�7L@ە�@ڧ�@�E�@���@�?}@�1'@�33@�5?@Լj@���@�=q@щ7@с@���@�|�@Ώ\@�Z@�ƨ@�\)@���@�V@��@š�@őh@�7L@���@��/@ēu@��
@�@���@���@��
@�;d@��@�n�@��@��^@���@���@��h@�O�@��@���@��9@�bN@�A�@�1'@�K�@���@���@�=q@�-@�-@��@�G�@��9@��@�\)@���@�M�@��@�X@��P@���@��!@���@��\@��\@��\@���@�^5@��-@�/@��@��@�ƨ@�;d@���@���@�n�@�5?@�@��7@�Ĝ@�9X@�A�@�1@�\)@��y@��!@���@��!@��!@���@��\@�V@�E�@�5?@��@���@��T@���@��h@��9@�Q�@�1'@�1@��
@���@�K�@��@��H@�ȴ@�ff@��T@�p�@�?}@��@��`@��@�j@�Z@��@���@�t�@�+@��\@�-@���@�`B@�/@���@��u@���@�K�@��@���@�5?@�O�@�?}@�G�@�/@��`@�A�@�9X@��
@���@��P@��y@��R@�ff@��@�?}@��@�V@���@�bN@�1@���@��w@���@�C�@�@���@�~�@��^@���@�z�@���@���@�;d@�~�@�=q@�J@��@���@�p�@��/@��9@��@��D@�r�@���@���@��@�o@�ȴ@���@���@���@���@���@�~�@���@���@�Z@�1'@�  @�1@��
@�C�@���@��!@���@�{@���@���@��h@�7L@��`@��@�1@���@��m@��@��@��y@�ȴ@��\@���@��T@��@���@�{@�$�@��@��T@�@�`B@�?}@��@�Ĝ@��@�1'@��;@��@�l�@�"�@�@���@��H@���@�J@�x�@�p�@�p�@�hs@�X@�&�@�V@���@��D@�Z@�9X@��@�@�P@\)@K�@�@~�@~�R@~��@~V@}�@|��@|�@|��@|�j@|�j@|�@|��@|j@|(�@|�@{�@j�"@Z��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�A�A�7LA�5?A�33A�5?A�/A��A�A��;Aț�A�K�AǾwA���A�x�A�hsA�^5A�M�A�A�A�5?A�(�A��A�{A��A��A��A�+A�ZA�VA�;dA�-A�&�A�oA��A���AœuA�z�A�hsA�^5A�XA�G�A�%Aě�A�=qA��A��AÏ\A�~�A�1'A�JA�S�A�~�A��PA���A�%A���A�7LA��A�E�A�33A��mA�"�A�"�A���A��A��DA�-A��A�=qA��A��/A���A���A�1'A�jA��yA���A�1A��A��/A�oA�VA�33A�\)A�hsA��A���A���A��A��DA���A�;dA�+A~��A~��A};dAu33Ar9XApI�An�9Am;dAl{AkVAhQ�Af9XAd�HAb��A^��A[�TAZz�AY`BAXJAV-AS;dAQC�AN�DAK`BAI;dAG�FAFM�AA�mA>�A=l�A<-A;hsA:ȴA8��A4�A3`BA2ĜA2�!A2~�A1��A1\)A1�A0ZA/�^A.ȴA,�A+|�A*JA)|�A(�A(��A(r�A($�A'��A'S�A&1A$ �A#�A#oA"��A ��A�`AVAA�A7LAA�AS�A33A��A=qA�jA�uAp�A�!A=qA
r�A�9AdZA%A�jAn�A  A�wA�A�At�A|�Ax�A?}A�uA�A�A�A&�A I�@��F@���@�9X@�ff@��@��T@��@�ȴ@��T@���@���@�&�@���@�S�@�"�@�^5@�@홚@��/@�P@�=q@�p�@���@�b@��
@�~�@�hs@�z�@�V@�bN@�7L@ە�@ڧ�@�E�@���@�?}@�1'@�33@�5?@Լj@���@�=q@щ7@с@���@�|�@Ώ\@�Z@�ƨ@�\)@���@�V@��@š�@őh@�7L@���@��/@ēu@��
@�@���@���@��
@�;d@��@�n�@��@��^@���@���@��h@�O�@��@���@��9@�bN@�A�@�1'@�K�@���@���@�=q@�-@�-@��@�G�@��9@��@�\)@���@�M�@��@�X@��P@���@��!@���@��\@��\@��\@���@�^5@��-@�/@��@��@�ƨ@�;d@���@���@�n�@�5?@�@��7@�Ĝ@�9X@�A�@�1@�\)@��y@��!@���@��!@��!@���@��\@�V@�E�@�5?@��@���@��T@���@��h@��9@�Q�@�1'@�1@��
@���@�K�@��@��H@�ȴ@�ff@��T@�p�@�?}@��@��`@��@�j@�Z@��@���@�t�@�+@��\@�-@���@�`B@�/@���@��u@���@�K�@��@���@�5?@�O�@�?}@�G�@�/@��`@�A�@�9X@��
@���@��P@��y@��R@�ff@��@�?}@��@�V@���@�bN@�1@���@��w@���@�C�@�@���@�~�@��^@���@�z�@���@���@�;d@�~�@�=q@�J@��@���@�p�@��/@��9@��@��D@�r�@���@���@��@�o@�ȴ@���@���@���@���@���@�~�@���@���@�Z@�1'@�  @�1@��
@�C�@���@��!@���@�{@���@���@��h@�7L@��`@��@�1@���@��m@��@��@��y@�ȴ@��\@���@��T@��@���@�{@�$�@��@��T@�@�`B@�?}@��@�Ĝ@��@�1'@��;@��@�l�@�"�@�@���@��H@���@�J@�x�@�p�@�p�@�hs@�X@�&�@�V@���@��D@�Z@�9X@��@�@�P@\)@K�@�@~�@~�R@~��@~V@}�@|��@|�@|��@|�j@|�j@|�@|��@|j@|(�@|�@{�@j�"@Z��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�-B
�FB
�jB
��B
�#B
�5B
�BB
�B
��BB1BuB�B�B�B�B"�B5?BQ�Bn�Bx�B�bB��B�-B�/B$�B6FB=qBM�BXB^5BaHB��B�hB�B�B|�B��B�1BiyBL�BC�B/B�B�B�BB�B�TB�`B��B�TB��B��B��Bv�BO�BJ�B:^B�B
�B
�
B
�}B
�B
�\B
s�B
[#B
L�B
C�B
@�B
=qB
,B	�sB	��B	ƨB	�wB	�LB	�!B	��B	��B	�hB	�7B	|�B	jB	YB	P�B	H�B	?}B	49B	$�B	�B	DB��B�B�sB�BB��B��B�^B�FB�-B�B��B��B��B��B��B�{B�{B�uB�oB�oB�bB�JB�B�B�B�%B�=B�JB�PB�PB�PB�JB�DB�DB�DB�=B�+B�B� B|�Bz�Bu�Bq�Bp�Bn�Bm�Bk�BiyBffBcTBaHB]/B[#BW
BR�BO�BN�BM�BM�BO�BN�BP�BQ�BS�BVBW
BXB\)B[#B[#B[#BZBZBYB]/B\)B]/BbNBgmBe`B_;B]/BdZB�B�DB�=B�VB�oB�oB�PB�1B�B�B�%B�uB��B��B�B�3B�3B�-B�'B�'B�?B�XB�^B�dB�^B�^B�^B�^B�XB�RB�XB�dB�wB�qB�qB�qB�qB�}B�}B��B��BBɺB��B��B��B��B��B��B��B�#B�;B�TB�yB�B�B��B��B��B��B��B��B	  B	B	B	%B		7B	hB	�B	�B	�B	�B	�B	�B	 �B	#�B	%�B	%�B	'�B	(�B	)�B	+B	+B	-B	6FB	9XB	9XB	:^B	=qB	?}B	A�B	C�B	F�B	F�B	F�B	H�B	J�B	M�B	R�B	YB	[#B	\)B	]/B	]/B	_;B	aHB	cTB	e`B	e`B	gmB	hsB	hsB	hsB	iyB	iyB	iyB	iyB	jB	k�B	k�B	l�B	l�B	m�B	o�B	q�B	w�B	w�B	w�B	x�B	y�B	z�B	|�B	~�B	~�B	~�B	�B	�B	�B	�B	�%B	�1B	�=B	�DB	�DB	�JB	�VB	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�3B	�?B	�?B	�LB	�^B	�jB	�qB	�wB	�}B	��B	��B	ĜB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�)B	�)B	�/B	�NB	�`B	�mB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
%B
+B
+B
	7B
	7B
DB
DB
DB
JB
PB
VB
PB
VB
VB
bB
hB
hB
hB
hB
hB
oB
oB
oB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
xB
)�B
6�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�-B
�FB
�jB
��B
�#B
�5B
�BB
�B
��BB1BuB�B�B�B�B"�B5?BQ�Bn�Bx�B�bB��B�-B�/B$�B6FB=qBM�BXB^5BaHB��B�hB�B�B|�B��B�1BiyBL�BC�B/B�B�B�BB�B�TB�`B��B�TB��B��B��Bv�BO�BJ�B:^B�B
�B
�
B
�}B
�B
�\B
s�B
[#B
L�B
C�B
@�B
=qB
,B	�sB	��B	ƨB	�wB	�LB	�!B	��B	��B	�hB	�7B	|�B	jB	YB	P�B	H�B	?}B	49B	$�B	�B	DB��B�B�sB�BB��B��B�^B�FB�-B�B��B��B��B��B��B�{B�{B�uB�oB�oB�bB�JB�B�B�B�%B�=B�JB�PB�PB�PB�JB�DB�DB�DB�=B�+B�B� B|�Bz�Bu�Bq�Bp�Bn�Bm�Bk�BiyBffBcTBaHB]/B[#BW
BR�BO�BN�BM�BM�BO�BN�BP�BQ�BS�BVBW
BXB\)B[#B[#B[#BZBZBYB]/B\)B]/BbNBgmBe`B_;B]/BdZB�B�DB�=B�VB�oB�oB�PB�1B�B�B�%B�uB��B��B�B�3B�3B�-B�'B�'B�?B�XB�^B�dB�^B�^B�^B�^B�XB�RB�XB�dB�wB�qB�qB�qB�qB�}B�}B��B��BBɺB��B��B��B��B��B��B��B�#B�;B�TB�yB�B�B��B��B��B��B��B��B	  B	B	B	%B		7B	hB	�B	�B	�B	�B	�B	�B	 �B	#�B	%�B	%�B	'�B	(�B	)�B	+B	+B	-B	6FB	9XB	9XB	:^B	=qB	?}B	A�B	C�B	F�B	F�B	F�B	H�B	J�B	M�B	R�B	YB	[#B	\)B	]/B	]/B	_;B	aHB	cTB	e`B	e`B	gmB	hsB	hsB	hsB	iyB	iyB	iyB	iyB	jB	k�B	k�B	l�B	l�B	m�B	o�B	q�B	w�B	w�B	w�B	x�B	y�B	z�B	|�B	~�B	~�B	~�B	�B	�B	�B	�B	�%B	�1B	�=B	�DB	�DB	�JB	�VB	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�3B	�?B	�?B	�LB	�^B	�jB	�qB	�wB	�}B	��B	��B	ĜB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�)B	�)B	�/B	�NB	�`B	�mB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
%B
+B
+B
	7B
	7B
DB
DB
DB
JB
PB
VB
PB
VB
VB
bB
hB
hB
hB
hB
hB
oB
oB
oB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
xB
)�B
6�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.05 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191648                              AO  ARCAADJP                                                                    20181005191648    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191648  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191648  QCF$                G�O�G�O�G�O�8000            