CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:22Z UW 3.1 conversion   
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
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               oA   AO  20111130141706  20190522121826  1727_5046_111                   2C  D   APEX                            2143                            040306                          846 @ԥ('���1   @ԥ(��?�@6���+�c�7KƧ�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @@  @y��@�  A   A!��AA��A`  A�  A�33A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8ffB@  BH  BP  BXffB`  Bh  Bp  Bx  B�33B�33B�  B���B�  B�33B�  B�  B�  B�  B�  B�  B�33B�  B�  B�33B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�33B�  B���B�  B�  C �C  C�fC  C  C
  C  C  C  C  C  C  C  C�fC  C�C   C"  C$  C%�fC(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CW�fCZ  C\  C^  C`  Ca�fCd  Cf  Ch  Cj�Cl  Cn  Cp  Cr  Cs�fCv  Cx�Cz  C|�C~�C�  C�  C��C�  C�  C�  C��3C��3C�  C�  C��3C�  C�  C��3C�  C�  C��3C�  C��C�  C�  C�  C�  C��C�  C��3C�  C��C�  C�  C�  C��C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C��3C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C��C�  C��3C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C��3C�  C��3C��3C��3C��3C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3D   D � D  D� D  D� D  D� D��D� D  D� D  Dy�D  D� D��D� D	fD	� D	��D
� D  D�fD  Dy�D  D� D  D�fDfD� D��Dy�D  D� DfD� D  D� D��D� D  D� D  D�fDfD�fDfD� D  Dy�D��D� DfD� D  D� D��Dy�D  D�fD  D� D��D � D!  D!� D!��D"y�D"��D#y�D#��D$� D%fD%� D&  D&� D'fD'� D'��D(� D)  D)� D*  D*y�D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D/��D0� D1  D1� D2fD2� D3  D3� D3��D4y�D5  D5� D6  D6� D7fD7�fD8fD8�fD9fD9� D9��D:� D;  D;� D<fD<� D<��D=� D>  D>� D>��D?y�D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DD��DE� DF  DF� DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DP� DQ  DQy�DR  DR� DS  DS� DTfDT�fDU  DUy�DU��DV� DW  DW� DXfDX� DY  DY� DZfDZ� D[  D[y�D[��D\� D]  D]� D]��D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� DlfDl� Dl��Dmy�Dn  Dn� Do  Do� Do��Dp� Dq  Dq� DrfDr�fDs  Ds� Ds�3Dy�3D�0 D�l�D�� D��fD��D�` D���D�� D�  D�VfD��3D�� D��D�ffDڠ D���D�3D�L�D�3D�311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @@  @y��@�  A   A!��AA��A`  A�  A�33A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8ffB@  BH  BP  BXffB`  Bh  Bp  Bx  B�33B�33B�  B���B�  B�33B�  B�  B�  B�  B�  B�  B�33B�  B�  B�33B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�33B�  B���B�  B�  C �C  C�fC  C  C
  C  C  C  C  C  C  C  C�fC  C�C   C"  C$  C%�fC(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CW�fCZ  C\  C^  C`  Ca�fCd  Cf  Ch  Cj�Cl  Cn  Cp  Cr  Cs�fCv  Cx�Cz  C|�C~�C�  C�  C��C�  C�  C�  C��3C��3C�  C�  C��3C�  C�  C��3C�  C�  C��3C�  C��C�  C�  C�  C�  C��C�  C��3C�  C��C�  C�  C�  C��C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C��3C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C��C�  C��3C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C��3C�  C��3C��3C��3C��3C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3D   D � D  D� D  D� D  D� D��D� D  D� D  Dy�D  D� D��D� D	fD	� D	��D
� D  D�fD  Dy�D  D� D  D�fDfD� D��Dy�D  D� DfD� D  D� D��D� D  D� D  D�fDfD�fDfD� D  Dy�D��D� DfD� D  D� D��Dy�D  D�fD  D� D��D � D!  D!� D!��D"y�D"��D#y�D#��D$� D%fD%� D&  D&� D'fD'� D'��D(� D)  D)� D*  D*y�D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D/��D0� D1  D1� D2fD2� D3  D3� D3��D4y�D5  D5� D6  D6� D7fD7�fD8fD8�fD9fD9� D9��D:� D;  D;� D<fD<� D<��D=� D>  D>� D>��D?y�D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DD��DE� DF  DF� DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DP� DQ  DQy�DR  DR� DS  DS� DTfDT�fDU  DUy�DU��DV� DW  DW� DXfDX� DY  DY� DZfDZ� D[  D[y�D[��D\� D]  D]� D]��D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� DlfDl� Dl��Dmy�Dn  Dn� Do  Do� Do��Dp� Dq  Dq� DrfDr�fDs  Ds� Ds�3Dy�3D�0 D�l�D�� D��fD��D�` D���D�� D�  D�VfD��3D�� D��D�ffDڠ D���D�3D�L�D�3D�311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�jA�`BA�t�A�v�A�t�A�t�A�t�A�v�AʃAʃAʅAʅAʅAʅAʅAʉ7Aʇ+Aʇ+Aʉ7AʍPAʍPAʑhAʓuAʕ�Aʕ�Aʗ�Aʗ�Aʗ�Aʗ�Aʙ�Aʗ�AʑhA�^5A�E�A���A�33A���A���A���A�A��+A�A��PA�A�p�A�/A���A�ĜA�"�A���A�n�A��`A��`A���A���A�ffA�A��hA�^5A�$�A���A���A��DA�`BA���A��PA���A�JA���A�VA�ĜA�ffA���A�M�A��A�C�A���A���A�G�A��TA�r�A���A��A�`BA��A�oA���A�z�A�oA��DA�E�A�A�A�oA�JA���A��A�O�A��mA���A�jA���A�r�A�n�A�  A�K�A�;dA�ƨA�bNA��A�9XA��A�G�A�"�A���A��mA���A���A��hA�$�A}�FA|�\A{x�Ay�Ax5?Au��At~�Ap��Anz�Aj�!Af$�Ae+Ac��Aa�A_�
A\��AZ��AX~�AS��ARZAPr�ANĜAL�`AK�AIl�AH �AF�AE%ADbNADbAC33AB5?A@�HA@1A>ȴA>bA<��A<n�A<1'A;t�A9�-A8�A8��A8^5A8  A7�7A7/A6��A6  A5�^A5O�A2=qA1A0�uA/�FA/33A.�yA.{A,~�A*^5A(jA'�;A'33A&�!A&Q�A%��A$ �A"�/A"�A!p�A �A�FAffA��A��A�7AM�At�A+A�AĜAQ�A��AS�AjAG�A`BA��A^5A��A33AffA��A|�A"�Az�A��A�A�A{Ap�A
bNA	�wA�A5?A
=A�A�+A�-A�A�\A(�A�A\)A �+A  �@��;@��@�o@�Ĝ@�C�@��7@��@�;d@�(�@���@���@�"�@�@�r�@��@��
@�o@�!@�\@�V@�5?@�7L@�1@�v�@�Ĝ@���@�G�@�r�@��@ۥ�@ڗ�@؃@�+@�J@�@Չ7@��`@�(�@ӍP@ҏ\@��@�?}@�l�@�Z@���@˥�@�dZ@�E�@��`@�ƨ@�;d@�5?@��/@�1'@§�@��@�n�@��T@��#@��-@�&�@��/@���@���@��D@�r�@� �@�+@�Z@���@��;@�dZ@�dZ@�l�@�l�@�\)@�o@���@��\@�E�@��T@�hs@��j@��D@�1'@�K�@�-@�%@�r�@� �@��@���@�|�@�33@�^5@�X@�Ĝ@��@��R@�5?@���@���@���@�r�@���@���@��m@��@�
=@�V@��#@���@��^@�@��@��T@�^5@��@�33@�\)@�l�@�t�@�33@�@���@���@�%@�9X@�  @��F@�;d@��H@��R@���@��\@�M�@�@���@���@���@�X@�Ĝ@�z�@�bN@��D@��u@��D@��@�j@���@���@�n�@�J@�?}@��@�z�@�33@�\)@�S�@�t�@�|�@�C�@���@�t�@�
=@���@��@��H@���@���@�V@��
@�M�@��T@��^@��^@��-@���@���@�X@�Ĝ@�z�@�I�@�1@��
@���@�t�@�dZ@�\)@�dZ@�l�@�|�@�|�@��@��+@��@���@���@���@���@���@���@���@���@�$�@�^5@�n�@�^5@�E�@�E�@�M�@�5?@�J@��@��#@���@���@���@��^@��h@��`@�|�@���@��h@�`B@�O�@�/@��@�%@��D@�bN@��@��
@���@���@�dZ@�C�@�o@��+@�=q@�$�@��@�J@�@�-@�/@��9@�r�@�1'@���@��m@��
@��#@|Z@q�^@gK�@_�@T�/@KC�@Dz�@?�P@9�7@333@-p�@&�R@"-@�@�#@I�@&�@Z@r�@ƨ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�jA�`BA�t�A�v�A�t�A�t�A�t�A�v�AʃAʃAʅAʅAʅAʅAʅAʉ7Aʇ+Aʇ+Aʉ7AʍPAʍPAʑhAʓuAʕ�Aʕ�Aʗ�Aʗ�Aʗ�Aʗ�Aʙ�Aʗ�AʑhA�^5A�E�A���A�33A���A���A���A�A��+A�A��PA�A�p�A�/A���A�ĜA�"�A���A�n�A��`A��`A���A���A�ffA�A��hA�^5A�$�A���A���A��DA�`BA���A��PA���A�JA���A�VA�ĜA�ffA���A�M�A��A�C�A���A���A�G�A��TA�r�A���A��A�`BA��A�oA���A�z�A�oA��DA�E�A�A�A�oA�JA���A��A�O�A��mA���A�jA���A�r�A�n�A�  A�K�A�;dA�ƨA�bNA��A�9XA��A�G�A�"�A���A��mA���A���A��hA�$�A}�FA|�\A{x�Ay�Ax5?Au��At~�Ap��Anz�Aj�!Af$�Ae+Ac��Aa�A_�
A\��AZ��AX~�AS��ARZAPr�ANĜAL�`AK�AIl�AH �AF�AE%ADbNADbAC33AB5?A@�HA@1A>ȴA>bA<��A<n�A<1'A;t�A9�-A8�A8��A8^5A8  A7�7A7/A6��A6  A5�^A5O�A2=qA1A0�uA/�FA/33A.�yA.{A,~�A*^5A(jA'�;A'33A&�!A&Q�A%��A$ �A"�/A"�A!p�A �A�FAffA��A��A�7AM�At�A+A�AĜAQ�A��AS�AjAG�A`BA��A^5A��A33AffA��A|�A"�Az�A��A�A�A{Ap�A
bNA	�wA�A5?A
=A�A�+A�-A�A�\A(�A�A\)A �+A  �@��;@��@�o@�Ĝ@�C�@��7@��@�;d@�(�@���@���@�"�@�@�r�@��@��
@�o@�!@�\@�V@�5?@�7L@�1@�v�@�Ĝ@���@�G�@�r�@��@ۥ�@ڗ�@؃@�+@�J@�@Չ7@��`@�(�@ӍP@ҏ\@��@�?}@�l�@�Z@���@˥�@�dZ@�E�@��`@�ƨ@�;d@�5?@��/@�1'@§�@��@�n�@��T@��#@��-@�&�@��/@���@���@��D@�r�@� �@�+@�Z@���@��;@�dZ@�dZ@�l�@�l�@�\)@�o@���@��\@�E�@��T@�hs@��j@��D@�1'@�K�@�-@�%@�r�@� �@��@���@�|�@�33@�^5@�X@�Ĝ@��@��R@�5?@���@���@���@�r�@���@���@��m@��@�
=@�V@��#@���@��^@�@��@��T@�^5@��@�33@�\)@�l�@�t�@�33@�@���@���@�%@�9X@�  @��F@�;d@��H@��R@���@��\@�M�@�@���@���@���@�X@�Ĝ@�z�@�bN@��D@��u@��D@��@�j@���@���@�n�@�J@�?}@��@�z�@�33@�\)@�S�@�t�@�|�@�C�@���@�t�@�
=@���@��@��H@���@���@�V@��
@�M�@��T@��^@��^@��-@���@���@�X@�Ĝ@�z�@�I�@�1@��
@���@�t�@�dZ@�\)@�dZ@�l�@�|�@�|�@��@��+@��@���@���@���@���@���@���@���@���@�$�@�^5@�n�@�^5@�E�@�E�@�M�@�5?@�J@��@��#@���@���@���@��^@��h@��`@�|�@���@��h@�`B@�O�@�/@��@�%@��D@�bN@��@��
@���@���@�dZ@�C�@�o@��+@�=q@�$�@��@�J@�@�-@�/@��9@�r�@�1'@���@��m@��
@��#@|Z@q�^@gK�@_�@T�/@KC�@Dz�@?�P@9�7@333@-p�@&�R@"-@�@�#@I�@&�@Z@r�@ƨ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBÖBÖBÖBBBB��B�}B��B��B�NB�B��BDB\B+BB��B��B	7B%�B1'B7LB5?B0!B-B/B49BI�BT�BYB_;BgmBr�Bw�B{�B|�B� B�B�VB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�VB�1B�B|�Bx�Bu�Br�Bk�BffB^5BQ�BC�B5?B"�BoB	7BB�B�sB�BĜB��B��Bx�BbNBS�B>wB+B!�B�B
��B
ȴB
��B
|�B
ffB
Q�B
:^B
uB	�mB	�/B	��B	��B	B	�9B	��B	�hB	|�B	hsB	L�B	B�B	6FB	%�B	�B	+B��B�mB��BȴB�wB�FB�B��B��B��B�{B�hB�VB�JB�7B�B�B~�B|�Bz�Bz�Bx�Bv�Bt�Bu�Bt�Bt�Bt�Bs�Bt�Bt�Bs�Br�Bp�Bl�Bq�Bp�Bo�Bo�Bn�Bl�Bn�Bp�Bq�Br�Bq�Bp�Bn�Bm�BjBjBiyBjBhsBgmBgmBhsBgmBffBdZBffBffBhsBiyBiyBhsBgmBffBe`BcTBbNB`BB_;B^5B]/B[#BZBYBW
BT�BQ�BP�BO�BM�BK�BJ�BH�BF�BC�BB�B?}B@�B>wB=qB=qB<jB;dB;dB:^B:^B9XB8RB6FB5?B49B33B2-B/B0!B1'B/B/B.B-B.B/B/B.B.B-B+B+B(�B(�B(�B,B,B-B-B-B/B5?B;dBC�BD�BD�BH�BK�BK�BK�BK�BJ�BK�BM�BN�BP�BR�BXBYBXBVBS�BS�BQ�BR�B[#BdZBffBgmBgmBk�Bv�B}�B�B�B�%B�B�B�B�B�%B�DB�VB�\B�bB�oB��B��B��B��B��B��B�B��B��B�B�-B�?B�LB�XB�XB�^B�^B�^B�jB�}B�}B�}BÖBǮB��B��B��B��B��B��B�
B�)B�HB�B��B��B��B��B	  B	+B	DB	oB	�B	�B	�B	�B	�B	�B	�B	!�B	#�B	'�B	,B	,B	,B	-B	-B	-B	-B	0!B	8RB	:^B	:^B	:^B	=qB	C�B	K�B	R�B	W
B	[#B	[#B	[#B	[#B	_;B	cTB	e`B	e`B	jB	n�B	o�B	p�B	p�B	p�B	p�B	r�B	u�B	z�B	�+B	�JB	�DB	�DB	�=B	�7B	�+B	�B	~�B	� B	�B	�1B	�7B	�7B	�7B	�=B	�DB	�VB	�bB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�9B	�FB	�XB	�dB	�jB	�jB	�jB	�qB	�}B	�}B	��B	��B	��B	��B	��B	��B	�wB	�^B	�RB	�LB	�LB	�LB	�dB	�jB	�dB	�dB	�dB	�dB	�dB	�jB	�jB	�qB	�wB	��B	ŢB	ǮB	ǮB	ǮB	ȴB	ȴB	��B	ȴB	ǮB	ƨB	ǮB	��B	��B	��B	�HB	��B
B
bB
�B
&�B
/B
6FB
:^B
@�B
G�B
L�B
S�B
YB
]/B
aHB
gmB
jB
p�B
t�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBÖBÖBÖBBÖBĜBÖBŢB��B�BB�B��BPB.B�BPBJBB%B�B.B8RB<jB8RB7LB9XB;dB<jBN�BZB]/BbNBjBv�By�B}�B� B�%B�JB��B��B��B��B��B��B��B��B��B�B��B��B��B��B��B��B��B��B�bB�DB�B{�Bz�Bx�Bp�Bn�BhsB_;BP�BD�B/B�BbBPB��B��B�B�#B�B��B�Bl�BdZBH�B1'B,B7LB"�B
�`B
�?B
�JB
v�B
gmB
]/B
49B	�B	�fB	�;B	�B	��B	��B	�}B	��B	��B	�B	VB	O�B	E�B	6FB	/B	�B	PB	B�)B�
B��BÖB�jB�3B��B��B��B��B�oB�uB�hB�VB�7B�+B�B�B~�B|�B}�B� Bz�Bv�Bw�Bw�Bw�Bw�Bx�Bw�Bv�By�B~�Bx�Bt�Bt�Bs�Br�Bt�Bz�B� B|�Bw�Bv�Bt�Br�Bs�Bv�Br�Bo�Bo�Bo�Bm�Bn�Bk�BiyBjBl�Bk�BhsBjBk�Bl�Bl�Bm�Bn�Bn�Bn�BffBdZBcTBcTBbNB_;B]/B]/B]/B\)B]/BXBVBR�BR�BO�BO�BL�BK�BJ�BI�BF�BC�B@�B@�B?}B@�B@�B=qB<jB;dB<jB>wB;dB:^B8RB8RB9XB7LB5?B5?B49B49B6FB33B2-B0!B/B/B/B0!B0!B0!B0!B2-B.B/B-B/B2-B6FB:^B;dBD�BF�BD�BK�BK�BO�BK�BO�BQ�BK�BO�BN�BR�BW
B]/BYBXB[#BS�BS�BYB[#B[#BffBgmBhsBgmBl�Bv�B~�B�B�B�%B�B�B�=B�+B�%B�DB�VB�\B�hB�uB��B��B��B��B��B��B�B��B�B�'B�FB�?B�LB�XB�XB�^B�jB�^B��BBB�}BŢBǮB��B��B��B��B��B�B�B�5B�TB�B��B��B��B��B��B	+B		7B	hB	�B	�B	�B	�B	!�B	�B	�B	$�B	%�B	(�B	-B	,B	,B	-B	-B	-B	.B	0!B	8RB	:^B	:^B	:^B	?}B	C�B	K�B	R�B	W
B	[#B	[#B	\)B	]/B	bNB	e`B	gmB	gmB	k�B	q�B	r�B	p�B	p�B	p�B	p�B	r�B	u�B	z�B	�+B	�JB	�DB	�DB	�=B	�7B	�+B	�B	~�B	� B	�B	�1B	�7B	�7B	�7B	�=B	�DB	�\B	�hB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�9B	�FB	�XB	�dB	�jB	�jB	�jB	�qB	�}B	�}B	��B	��B	��B	��B	B	B	B	�qB	�XB	�RB	�LB	�LB	�dB	�jB	�jB	�jB	�jB	�jB	�dB	�qB	�qB	�qB	�}B	��B	ŢB	ǮB	ǮB	ǮB	ȴB	ȴB	��B	ɺB	ȴB	ƨB	ǮB	��B	��B	��B	�HB	��B
B
bB
�B
&�B
/B
6FB
:^B
@�B
G�B
L�B
S�B
YB
]/B
aHB
gmB
jB
p�B
t�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<D��<#�
<T��<�j=C�<T��<#�
<#�
<#�
<e`B<e`B<#�
<#�
<#�
<#�
<#�
<D��<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<T��<T��<u<D��<#�
<#�
<D��<#�
<D��<�t�<�9X<#�
<��
<D��<#�
<�o<#�
<#�
<#�
<��=�w<�`B<�9X<u<�o<�1=C�=o<#�
<#�
<49X<e`B<�C�<D��<ě�<�1<���<���<#�
<T��<u<�o<�1<e`B<�1<�/<#�
<e`B<T��<T��<T��<D��<#�
<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�t�<#�
<#�
<#�
<#�
<#�
<#�
<D��<u<49X<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447132012010314471320120103144713  AO  ARGQ                                                                        20111130141706  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130141706  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144713  IP                  G�O�G�O�G�O�                