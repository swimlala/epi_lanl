CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-12-05 AOML 2.2 creation; 2015-12-13T23:15:40Z UW 3.1 conversion   
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Kt   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  Mp   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  U\   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]H   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  _D   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  g0   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  i,   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  q   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  {    PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �0   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �4   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �8   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �<   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �@   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901337 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               =A   AO  20111205113414  20190522121836  1901_5055_061                   2C  D   APEX                            2140                            040306                          846 @���ax@1   @�����@-`ě��T�c����l�1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@�33A   A   A@  A`  A�  A�  A�  A���A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�33B�33B�33B�  B�  B�  B�  B�  B�33B�  B�  B�33B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B���C�fC  C�C  C
  C�C  C  C�C  C  C  C  C  C  C �C"�C$�C&�C(  C)�fC+�fC-�fC0  C2�C4  C5�fC8  C:  C<  C>�C@  CA�fCD�CF  CG�fCJ  CL  CN  CP�CR  CS�fCU�fCW�fCY�fC[�fC^  C`�Cb�Cd  Cf  Ch�Cj  Ck�fCm�fCp  Cr�Ct  Cu�fCx  Cz  C|  C~�C�  C�  C�  C��3C��3C��3C�  C�  C��C�  C�  C��3C��3C��3C��3C�  C�  C��C��C��C��C�  C��3C��3C��3C�  C�  C�  C��C�  C��3C�  C�  C��C�  C��3C�  C�  C�  C��C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C��C�  C��3C�  C�  C��3C�  C�  C��3C�  C��C�  C�  C��C��C�  C��C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C��3C�  C��C�  C�  C�  C��C�  C��3C�  C��C�  C��3C�  C��C�  C�  C��C�  C�  C��C�  C�  C��C��C�  C��3C��3C�  C�  C�  C��C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C��3C��3C��3C��3C��D   D � D  D� D  D� D  D� D  D�fDfD�fDfD�fD  D� D  D� D	fD	y�D
  D
� D  D�fD  D� D  D�fDfD� D  D�fD��D� D  Dy�D��D� D��D� D  Dy�D  D�fD  D� DfD� D  Dy�D��D� DfD� D��D� DfD� D  Dy�D��D� DfD� D��D y�D!  D!�fD"fD"�fD#  D#y�D#��D$� D%fD%�fD&fD&�fD'fD'�fD(fD(�fD)fD)�fD*fD*�fD+fD+��D,  D,y�D,��D-� D.  D.� D.��D/y�D/��D0y�D1  D1� D2  D2� D3  D3�fD4fD4� D4��D5y�D5��D6� D7  D7�fD8fD8� D8��D9� D:  D:�fD;fD;�fD<  D<� D=fD=�fD>fD>�fD?  D?� D@  D@y�D@��DAy�DA��DBy�DB��DCy�DD  DD��DEfDE�fDFfDF�fDG�DG��DH�DH�fDIfDI�fDJ�DJ� DJ�3DKs3DL  DL�fDM  DMy�DM��DNy�DO  DO� DP  DP� DQ  DQ� DQ��DRy�DS  DS�fDTfDT�fDUfDU�fDVfDV� DW  DWy�DW��DX� DYfDY�fDZ  DZ� D[  D[� D[��D\y�D\��D]y�D]��D^y�D^��D_s3D`  D`��Da  Das3Da�3Dby�Db��Dcy�Dc��Ddy�Dd��Dey�Df  Df�fDg  Dg� Dg��Dh� DifDi�fDjfDj� Dj��Dk� DlfDl� Dm  Dmy�Dn  Dn�fDofDo� Dp  Dpy�Dp��Dq� DrfDr�fDs  Dsy�Ds��D�0 D��3D��fD� D�� D�� D���D�FfD�C3D��3D�ɚD�0 D��3D�vfD��3D�C3D� D���D�ffD��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @l��@�ff@�33A��A9��AY��Ay��A���A���A���A���A���A���A���A���BffBffBffBffB&ffB.ffB6ffB>ffBFffBNffBVffB^ffBfffBnffBvffB~��B�ffB�ffB�ffB�33B�33B�33B�33B�33B�ffB�33B�33B�ffB�33B�33B�  B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�ffB�33B�  B�33B�33B�  C� C��C�3C��C	��C�3C��C��C�3C��C��C��C��C��C��C�3C!�3C#�3C%�3C'��C)� C+� C-� C/��C1�3C3��C5� C7��C9��C;��C=�3C?��CA� CC�3CE��CG� CI��CK��CM��CO�3CQ��CS� CU� CW� CY� C[� C]��C_�3Ca�3Cc��Ce��Cg�3Ci��Ck� Cm� Co��Cq�3Cs��Cu� Cw��Cy��C{��C}�3C��C���C���C�� C�� C�� C���C���C�ٚC���C���C�� C�� C�� C�� C���C���C�ٚC�ٚC�ٚC�ٚC���C�� C�� C�� C���C���C���C�ٚC���C�� C���C���C�ٚC���C�� C���C���C���C�ٚC���C���C���C�ٚC���C���C���C���C�� C���C���C�ٚC���C�� C���C���C�� C���C���C�� C���C�ٚC���C���C�ٚC�ٚC���C�ٚC���C���C���C���C�� C���C���C�� C���C���C�� C���C�ٚC���C���C���C�ٚC���C�� C���C�ٚC���C�� C���C�ٚC���C���C�ٚC���C���C�ٚC���C���C�ٚC�ٚC���C�� C�� C���C���C���C�ٚC���C�� C���C���C�� C���C���C���C���C���C���C���C�ٚC�� C�� C�� C�� C�ٚC���D ffD �fDffD�fDffD�fDffD�fDl�D��Dl�D��Dl�D�fDffD�fDffD��D	` D	�fD
ffD
�fDl�D�fDffD�fDl�D��DffD�fDl�D� DffD�fD` D� DffD� DffD�fD` D�fDl�D�fDffD��DffD�fD` D� DffD��DffD� DffD��DffD�fD` D� DffD��DffD� D ` D �fD!l�D!��D"l�D"�fD#` D#� D$ffD$��D%l�D%��D&l�D&��D'l�D'��D(l�D(��D)l�D)��D*l�D*��D+s3D+�fD,` D,� D-ffD-�fD.ffD.� D/` D/� D0` D0�fD1ffD1�fD2ffD2�fD3l�D3��D4ffD4� D5` D5� D6ffD6�fD7l�D7��D8ffD8� D9ffD9�fD:l�D:��D;l�D;�fD<ffD<��D=l�D=��D>l�D>�fD?ffD?�fD@` D@� DA` DA� DB` DB� DC` DC�fDDs3DD��DEl�DE��DFl�DF�3DGs3DG�3DHl�DH��DIl�DI�3DJffDJٚDKY�DK�fDLl�DL�fDM` DM� DN` DN�fDOffDO�fDPffDP�fDQffDQ� DR` DR�fDSl�DS��DTl�DT��DUl�DU��DVffDV�fDW` DW� DXffDX��DYl�DY�fDZffDZ�fD[ffD[� D\` D\� D]` D]� D^` D^� D_Y�D_�fD`s3D`�fDaY�DaٚDb` Db� Dc` Dc� Dd` Dd� De` De�fDfl�Df�fDgffDg� DhffDh��Dil�Di��DjffDj� DkffDk��DlffDl�fDm` Dm�fDnl�Dn��DoffDo�fDp` Dp� DqffDq��Drl�Dr�fDs` Ds� D�#3D�vfD�ɚD�3D�s3D��3D�� D�9�D�6fD��fD���D�#3D��fD�i�D��fD�6fD�3D�� D�Y�D�  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�9XA�;dA�1'A�;dA�;dA�=qA�?}A�C�A�A�A�C�A�A�A�A�ĜAÑhA�~�A�v�A�z�A�z�A�bNA� �A�  A��A��A��;A§�A�7LA��9A��#A�;dA�K�A�l�A�`BA�JA�dZA�{A��A��RA���A���A��^A��/A���A��!A�`BA�33A��A���A�|�A��+A�M�A�1A��`A��!A���A��/A��7A��A�;dA�JA�jA��7A�ƨA��TA�ZA��mA�hsA�JA���A�A�A���A�x�A�G�A���A��A�t�A���A�;dA��-A���A���A�z�A�9XA�oA��TA���A���A�9XA��A�r�A��A�ffA��mA�5?A���A��7A�XA�jA�v�A~�A|JAy��At�AmVAi�Ag�7AdJAbbA\�AX^5AU+ATn�ASG�AJ�`AH{AF��AE�AD�`AC`BAA�#A@�jA?��A<1A:�A7p�A5ƨA4{A2VA1��A0��A/�mA-p�A+33A*Q�A(��A'��A&��A&n�A$jA#��A#?}A#\)A"1A!ƨA"9XA!A jA?}AĜA��A�FA�yA%A�A�\AbNAQ�Ar�AQ�A�Ap�AhsAJA�A|�AQ�An�A��A�A�^A��A��AbNAhsA�;A$�A1'A  A�PAdZAA�AȴA�#A�A7LA�/A5?A|�A(�A�
A�-A&�A"�A?}A7LA�DA�hAO�A%A=qA��AC�A
�uA
1'A
  A	�-A	l�A�AffA�A��A��AdZAO�A�A�#A��A�AdZAp�A�At�A�A�A=qA�
A`BA�AĜAr�A=qAA�FA7LA�yA��AbA�TA��AO�A �HA ZA J@�K�@�O�@�(�@���@�ȴ@���@��@�  @��w@��!@��@��u@�ƨ@�@�"�@�@�M�@���@�hs@�?}@��@�Ĝ@�A�@�dZ@�@���@�ƨ@�;d@��@�ff@�-@���@�(�@�"�@���@�\@噚@�dZ@�^5@�{@��#@��u@���@�o@޸R@���@ޟ�@ݺ^@���@ܓu@ܬ@۝�@�ȴ@�x�@��/@�Ĝ@ج@ؓu@�z�@�Z@��;@��@֗�@�~�@�V@�@�7L@Ԭ@�Z@Ӿw@��@��H@ҟ�@�v�@�M�@�hs@���@��@��@ΰ!@�V@�@́@��/@� �@���@�v�@�J@ɲ-@�O�@���@��@���@ȴ9@ȃ@��
@�"�@Ə\@�=q@�@ź^@őh@�hs@���@��;@�|�@�S�@���@�ff@�J@���@��7@��@��/@���@�1'@�|�@��@��H@���@�V@�$�@���@��@�j@��@�C�@�"�@�"�@��H@��\@�{@�G�@���@�Ĝ@�Q�@�1@�@�~�@�5?@���@��-@�&�@��D@�Q�@��;@�"�@���@���@�ff@��@��@�&�@�9X@�  @��
@���@�l�@�"�@�E�@��#@���@���@�Z@�b@��F@�+@���@���@�E�@���@���@���@�hs@��@��`@��D@�z�@�9X@��w@�l�@�o@���@�V@�$�@��@���@���@�p�@�G�@�%@��j@��@�Q�@���@�|�@��y@��!@��\@�-@�@��T@���@��@�%@��@��D@�Z@�1'@��@���@�S�@��@�^5@�M�@�=q@��#@�p�@�7L@�V@���@��9@��@��@���@��P@�@���@�v�@�E�@�J@��#@��h@�`B@�/@�Ĝ@�r�@��@���@���@���@�\)@��@��@��\@�/@��@{�m@q7L@hQ�@^$�@T��@K��@D�@<9X@4�D@/+@*�\@$�/@+@hs@V@7L@9X111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�9XA�;dA�1'A�;dA�;dA�=qA�?}A�C�A�A�A�C�A�A�A�A�ĜAÑhA�~�A�v�A�z�A�z�A�bNA� �A�  A��A��A��;A§�A�7LA��9A��#A�;dA�K�A�l�A�`BA�JA�dZA�{A��A��RA���A���A��^A��/A���A��!A�`BA�33A��A���A�|�A��+A�M�A�1A��`A��!A���A��/A��7A��A�;dA�JA�jA��7A�ƨA��TA�ZA��mA�hsA�JA���A�A�A���A�x�A�G�A���A��A�t�A���A�;dA��-A���A���A�z�A�9XA�oA��TA���A���A�9XA��A�r�A��A�ffA��mA�5?A���A��7A�XA�jA�v�A~�A|JAy��At�AmVAi�Ag�7AdJAbbA\�AX^5AU+ATn�ASG�AJ�`AH{AF��AE�AD�`AC`BAA�#A@�jA?��A<1A:�A7p�A5ƨA4{A2VA1��A0��A/�mA-p�A+33A*Q�A(��A'��A&��A&n�A$jA#��A#?}A#\)A"1A!ƨA"9XA!A jA?}AĜA��A�FA�yA%A�A�\AbNAQ�Ar�AQ�A�Ap�AhsAJA�A|�AQ�An�A��A�A�^A��A��AbNAhsA�;A$�A1'A  A�PAdZAA�AȴA�#A�A7LA�/A5?A|�A(�A�
A�-A&�A"�A?}A7LA�DA�hAO�A%A=qA��AC�A
�uA
1'A
  A	�-A	l�A�AffA�A��A��AdZAO�A�A�#A��A�AdZAp�A�At�A�A�A=qA�
A`BA�AĜAr�A=qAA�FA7LA�yA��AbA�TA��AO�A �HA ZA J@�K�@�O�@�(�@���@�ȴ@���@��@�  @��w@��!@��@��u@�ƨ@�@�"�@�@�M�@���@�hs@�?}@��@�Ĝ@�A�@�dZ@�@���@�ƨ@�;d@��@�ff@�-@���@�(�@�"�@���@�\@噚@�dZ@�^5@�{@��#@��u@���@�o@޸R@���@ޟ�@ݺ^@���@ܓu@ܬ@۝�@�ȴ@�x�@��/@�Ĝ@ج@ؓu@�z�@�Z@��;@��@֗�@�~�@�V@�@�7L@Ԭ@�Z@Ӿw@��@��H@ҟ�@�v�@�M�@�hs@���@��@��@ΰ!@�V@�@́@��/@� �@���@�v�@�J@ɲ-@�O�@���@��@���@ȴ9@ȃ@��
@�"�@Ə\@�=q@�@ź^@őh@�hs@���@��;@�|�@�S�@���@�ff@�J@���@��7@��@��/@���@�1'@�|�@��@��H@���@�V@�$�@���@��@�j@��@�C�@�"�@�"�@��H@��\@�{@�G�@���@�Ĝ@�Q�@�1@�@�~�@�5?@���@��-@�&�@��D@�Q�@��;@�"�@���@���@�ff@��@��@�&�@�9X@�  @��
@���@�l�@�"�@�E�@��#@���@���@�Z@�b@��F@�+@���@���@�E�@���@���@���@�hs@��@��`@��D@�z�@�9X@��w@�l�@�o@���@�V@�$�@��@���@���@�p�@�G�@�%@��j@��@�Q�@���@�|�@��y@��!@��\@�-@�@��T@���@��@�%@��@��D@�Z@�1'@��@���@�S�@��@�^5@�M�@�=q@��#@�p�@�7L@�V@���@��9@��@��@���@��P@�@���@�v�@�E�@�J@��#@��h@�`B@�/@�Ĝ@�r�@��@���@���@���@�\)@��@��@��\@�/@��@{�m@q7L@hQ�@^$�@T��@K��@D�@<9X@4�D@/+@*�\@$�/@+@hs@V@7L@9X111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�?B	�?B	�FB	�?B	�?B	�?B	�?B	�LB	�XB	�^B	�jB	�B	�B
B
	7B
VB
uB
�B
�B
�B
{B
uB
uB
�B
E�B
�\B
�B@�Bz�B��B��B�'B�?B�B��B�B�-B�wB��B�#B�fB�fB�mB�BB�sB�B�B��BB�B�B"�B+B��B�'B�B�B6FB"�B1B�B��B��B�B  BBB��B��B��B�B�mB�ZB�/B�B��B��BĜB�XB�B��B��B��B�{B�7Bz�Bk�BW
B2-B�B
�B
�B
�^B
�uB
�B
ZB
+B
B	�B	�/B	ÖB	��B	n�B	L�B	=qB	+B	�B��B�sB�#B��BĜB�XB�-B�'B�!B�B��B�B�B�B�LB�-B�9B�-B�B��B��B��B��B��B��B�!BǮB�HB�`B�B�B		7B	{B	%�B	�B	�B	#�B	%�B	"�B	�B	uB	�B	�B	uB	DB��B	B	!�B	R�B	�{B	�oB	�PB	�%B	{�B	o�B	cTB	XB	R�B	[#B	w�B	��B	��B	��B	��B	�TB	��B
B
%B

=B
DB
JB
DB
1B
B
B	��B
B
%B
	7B

=B
%B
  B
  B
B
B
B
1B
\B
bB
hB
bB
\B
PB
DB
	7B
%B
B
B	��B	��B	��B	��B	��B
B
B
%B
%B
DB
hB
�B
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
hB
bB
\B
VB
PB
DB

=B
1B
1B
+B
%B
B
B
B
B
%B
B
B
B
%B
%B
+B
+B
%B
B
B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
%B
%B
1B
+B
+B
+B
1B
1B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B
	7B

=B
	7B

=B

=B

=B

=B

=B

=B
DB
JB
JB
PB
PB
PB
PB
PB
PB
PB
VB
VB
VB
PB
PB
VB
VB
VB
VB
VB
VB
\B
\B
\B
\B
\B
\B
bB
bB
bB
oB
oB
oB
oB
oB
oB
uB
uB
oB
uB
uB
{B
{B
{B
{B
{B
{B
{B
 �B
%�B
+B
2-B
:^B
@�B
E�B
L�B
Q�B
T�B
ZB
_;B
cTB
ffB
l�B
p�B
t�B
x�B
|�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	�?B	�?B	�FB	�?B	�?B	�?B	�?B	�LB	�XB	�dB	�wB	�#B	��B
B
	7B
VB
uB
�B
�B
�B
�B
{B
{B
�B
J�B
��B
�BBE�By�B��B�B�?B�dB�B��B�B�3B�wB��B�B�mB�mB�B�NB�yB�B��B  BhB%�B7LB2-B!�B�BŢB�3B�BD�B6FB�B��BB��B��B+BJB\B%B+BJB��B�B�B�fB�NB�B��B��BȴB�qB��B��B��B��B��B�7B�Bw�BK�B8RB1B
�B
��B
��B
��B
~�B
K�B
�B
B	�B	�fB	��B	�%B	\)B	S�B	?}B	<jB	�B��B�TB�fB�BɺB�dB�RB�RB�LB�9B�?B�XBƨBŢBÖB��B�qB�RB�'B�B�B�'B�B�B�^B��B�yB�B��B��B	JB	�B	.B	 �B	�B	)�B	1'B	.B	(�B	�B	#�B	!�B	%�B	!�B��B��B	\B	<jB	��B	��B	��B	��B	�=B	� B	r�B	]/B	M�B	M�B	hsB	��B	��B	��B	��B	�/B	��B
  B
+B
JB
\B
\B
hB
bB
PB
1B
B
%B
DB
\B
hB
VB
B
B
B
B
B
DB
�B
�B
{B
{B
�B
oB
\B
VB
	7B
B
B
B
B
  B	��B	��B
B
B
%B
B

=B
oB
�B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
hB
hB
\B
VB
JB
DB
JB
DB
	7B
%B
B
+B
1B
+B
+B
+B
+B
1B
+B
+B
%B
B
B
%B
B
B	��B	��B	��B
B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	�B	�B	��B	�B	��B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
%B
B
%B
%B
1B
	7B
1B
1B
	7B
	7B
1B
1B

=B
1B
	7B
	7B
	7B
	7B
	7B

=B
DB

=B
	7B
DB
	7B

=B
JB
JB
PB
JB

=B
DB
JB
JB
PB
PB
PB
PB
\B
PB
\B
VB
VB
VB
PB
PB
VB
VB
VB
VB
bB
VB
bB
bB
\B
bB
\B
\B
bB
oB
bB
{B
oB
uB
uB
oB
uB
uB
uB
{B
�B
uB
{B
�B
{B
{B
�B
{B
{B
 �B
%�B
+B
33B
;dB
@�B
E�B
L�B
Q�B
VB
ZB
_;B
cTB
ffB
l�B
p�B
t�B
x�B
|�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<e`B<���<u<���<�/<��
<#�
<#�
<e`B<���<�t�<#�
<#�
<#�
<#�
<#�
<#�
<e`B<#�
<#�
<�C�<T��<#�
<#�
<#�
<#�
<#�
<#�
<D��<e`B<u<#�
<#�
<#�
<T��<e`B<e`B<�1=o<���<��<��
<ě�<ě�<�o=+=\)=o<�t�<�C�<�1=C�=,1<�9X<u<�9X<���=C�<�9X<�C�<#�
<�t�=49X<�o<#�
<#�
<#�
<#�
<#�
<#�
<D��<�j<e`B<�o<D��<49X<#�
<#�
<#�
<#�
<�o<e`B<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<49X<e`B<#�
<#�
<#�
<�C�<�1<#�
<#�
<�t�<�9X<#�
<#�
<#�
<�o<e`B<�o<e`B<#�
<#�
<T��<u<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.4 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201131250292012011312502920120113125029  AO  ARGQ                                                                        20111205113414  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111205113414  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120113125029  IP                  G�O�G�O�G�O�                