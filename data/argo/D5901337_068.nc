CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-12-05 AOML 2.2 creation; 2015-12-13T23:15:42Z UW 3.1 conversion   
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
_FillValue                 �  K|   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  Mx   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  Uh   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]X   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  _T   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  gD   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  i@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  q0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  {   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �X   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �\   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �`   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �d   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901337 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               DA   AO  20111205113530  20190522121836  1901_5055_068                   2C  D   APEX                            2140                            040306                          846 @��(�� 1   @��)"!��@,�Q��c�V�1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @@  @�  @�  A   A   A@  A^ffA~ffA�33A�  A���A���A�  A�  A�  B ffBffBffB  B��B'��B/��B8  B@  BH  BP  BW��B_��Bh  Bp  Bx  B�33B�33B�  B���B���B���B�  B�33B�33B�  B�  B�  B�  B���B���B�  B�  B�  B�33B�  B�  B�  B�  B�  B�33B�  B�  B�  B�33B�33B�  B�  C   C  C  C  C�fC
  C�C  C  C  C�C  C  C  C  C  C�fC!�fC#�fC%�fC(  C*  C+�fC.�C0  C2  C4  C6  C8  C:  C<�C>  C@  CB  CC�fCF  CH  CJ  CL  CM�fCP  CR�CT  CU�fCW�fCZ  C\  C^�C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv�Cx  Cz  C{�fC}�fC�fC��3C�  C�  C�  C��C�  C��3C�  C��C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��3C��3C�  C�  C�  C�  C�  C��C�  C��C��C��C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C��C��C��C��3C��3C��3C��3C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C��3C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��C��C�  C��3C�  C��C�  C��3C�  C��C�  C��3C�  C��C�  C��3C�  C��C�  C��3C�  C��C�  C�  C�  C��3C��3D � DfD�fDfD�fD  D� D  Dy�D��Dy�D��Dy�D��Dy�D  D�fD	fD	�fD
  D
� D  Dy�D��D� D  Dy�D��D� DfD�fDfD�fD  D� D�D�fDfD� D  D� D��Dy�D��D� DfD� D  Dy�D��D� DfD�fD  D� D��Dy�D��D� DfD�fDfD�fD   D � D ��D!y�D!��D"y�D"��D#s3D#��D$y�D$��D%y�D&  D&y�D'  D'� D(  D(� D)  D)� D*  D*� D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D4  D4� D5  D5�fD6  D6y�D6��D7y�D7��D8� D9  D9� D:  D:� D;  D;�fD<fD<�fD=  D=y�D=��D>y�D?  D?� D@fD@�fDA  DAy�DA��DB� DC  DC� DD  DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHs3DI  DI�fDJfDJ�fDKfDK�fDL�DL��DMfDM�fDNfDN� DO  DO� DO��DPy�DP��DQy�DQ��DRy�DS  DS�fDTfDT� DU  DUy�DV  DV�fDW  DW� DW��DX� DYfDY� DZ  DZ�fD[  D[� D\fD\� D]  D]� D]��D^� D_fD_� D_��D`� DafDa� Da��Db� DcfDc� Dd  Ddy�De  De�fDffDf� Dg  Dgy�Dh  Dh�fDi  Diy�Dj  Dj� Dj��Dk� Dl  Dly�Dm  Dm� Dn  Dn� DofDo�fDo��Dpy�DqfDq�fDr  Dry�Ds  Ds� Ds��D�L�D��3D��fD�0 D��fD�� D��fD���D�C3D�y�D�� D� D�P D��fD� D�L�D�3D��3D�ffD�i�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@&ff@fff@�33@�33A��A9��AX  Ax  A�  A���A���A���A���A���A���A���B��B��BffB  B&  B.  B6ffB>ffBFffBNffBV  B^  BfffBnffBvffB~��B�ffB�33B�  B�  B�  B�33B�ffB�ffB�33B�33B�33B�33B�  B�  B�33B�33B�33B�ffB�33B�33B�33B�33B�33B�ffB�33B�33B�33B�ffB�ffB�33B�33B�33C��C��C��C� C	��C�3C��C��C��C�3C��C��C��C��C��C� C!� C#� C%� C'��C)��C+� C-�3C/��C1��C3��C5��C7��C9��C;�3C=��C?��CA��CC� CE��CG��CI��CK��CM� CO��CQ�3CS��CU� CW� CY��C[��C]�3C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu�3Cw��Cy��C{� C}� C� C�� C���C���C���C�ٚC���C�� C���C�ٚC�ٚC���C���C���C���C�� C���C���C���C���C���C�ٚC�� C���C���C���C���C���C���C���C���C���C���C���C�ٚC�� C�� C���C���C���C���C���C�ٚC���C�ٚC�ٚC�ٚC�� C���C���C���C���C���C�ٚC���C���C���C���C���C���C���C���C�ٚC���C���C�ٚC�ٚC�ٚC�ٚC�� C�� C�� C�� C���C���C���C���C�ٚC�ٚC���C���C���C���C���C���C�� C�ٚC���C���C���C���C���C���C���C���C���C���C���C���C���C�ٚC���C�ٚC�ٚC���C�� C���C�ٚC���C�� C���C�ٚC���C�� C���C�ٚC���C�� C���C�ٚC���C�� C���C�ٚC���C���C���C�� C�� D ffD ��Dl�D��Dl�D�fDffD�fD` D� D` D� D` D� D` D�fDl�D��D	l�D	�fD
ffD
�fD` D� DffD�fD` D� DffD��Dl�D��Dl�D�fDffD�3Dl�D��DffD�fDffD� D` D� DffD��DffD�fD` D� DffD��Dl�D�fDffD� D` D� DffD��Dl�D��Dl�D�fD ffD � D!` D!� D"` D"� D#Y�D#� D$` D$� D%` D%�fD&` D&�fD'ffD'�fD(ffD(�fD)ffD)�fD*ffD*� D+` D+� D,` D,� D-` D-� D.` D.� D/` D/� D0` D0� D1` D1� D2` D2� D3` D3�fD4ffD4�fD5l�D5�fD6` D6� D7` D7� D8ffD8�fD9ffD9�fD:ffD:�fD;l�D;��D<l�D<�fD=` D=� D>` D>�fD?ffD?��D@l�D@�fDA` DA� DBffDB�fDCffDC�fDD` DD� DE` DE� DF` DF� DG` DG� DHY�DH�fDIl�DI��DJl�DJ��DKl�DK�3DLs3DL��DMl�DM��DNffDN�fDOffDO� DP` DP� DQ` DQ� DR` DR�fDSl�DS��DTffDT�fDU` DU�fDVl�DV�fDWffDW� DXffDX��DYffDY�fDZl�DZ�fD[ffD[��D\ffD\�fD]ffD]� D^ffD^��D_ffD_� D`ffD`��DaffDa� DbffDb��DcffDc�fDd` Dd�fDel�De��DfffDf�fDg` Dg�fDhl�Dh�fDi` Di�fDjffDj� DkffDk�fDl` Dl�fDmffDm�fDnffDn��Dol�Do� Dp` Dp��Dql�Dq�fDr` Dr�fDsffDs� D�@ D��fD���D�#3D���D��3D���D�� D�6fD�l�D��3D�3D�C3Dӹ�D�3D�@ D�fD��fD�Y�D�\�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�K�A�M�A�O�A�O�A�Q�A�VA�I�A�A�A�=qA�7LA�$�A���Aȗ�Aȏ\Aȇ+A�z�A�jA�bNA�^5A�VA�M�A�G�A�?}A�9XA�5?A�1'A�(�A�"�A�VA�A�A���A���A�  A�  A���A���A�  A���A���A��A��/AǺ^A�x�A���A�7LA��HA�M�A�A� �A��mA�$�A�p�A�z�A�O�A���A�n�A�ĜA��PA�  A�A�33A���A��DA���A�+A��7A�oA���A���A��mA��RA���A��uA�jA��A��+A���A�33A�
=A���A��DA�dZA��\A�-A��A�A�A�A�p�A�33A�dZA���A�ȴA�^5A�M�A��A{��Ax{At�Ao��Ag\)Ac/A\�yAW
=AR��AP9XAL�yAHĜAE��AD�\AC�AA��A@M�A>z�A<�+A:ZA8A�A7�wA6�+A4�jA2ĜA.�`A-�wA,5?A+�A+;dA)�A'��A& �A%x�A&  A&bA%��A$��A#l�A$�A%�A!"�A r�A!�;A�#A�A��At�A�AA��A;dAn�A$�A{A$�AA�A9XAbA�mA�AbA��A�^A;dA��A��A��AĜAoA?}A��At�An�A�mA �AE�AƨAz�A&�A��AA\)A�A�9Av�A��A�uA��A��A�FA��A\)A-A�hAC�A��A��Az�A��A
�A
{A	��A�HAr�A�A1'A�TA�#A  A�AhsAXAA;dA\)A��AA�Al�A�AM�A��A��A��A�PA|�AhsA/A��AM�A�FA�A7LA �`A @�K�@��@�{@��@�`B@��u@��m@�ȴ@�5?@��7@���@�(�@�ff@��@�  @��@�@�@�@�hs@��@�j@�|�@�33@�^5@�@�?}@��/@�@�(�@��@�ƨ@�P@�=q@�hs@�z�@�1'@�ƨ@�K�@�\@�E�@�@�-@�9@��
@�+@�E�@�7@�Ĝ@�1@�|�@�33@���@��y@�~�@ݙ�@�/@܃@�b@ۮ@�C�@�ȴ@�~�@�{@��`@�bN@��@���@ם�@֗�@�ff@��#@�?}@��@�r�@ӥ�@�"�@���@�ff@�5?@��`@�Z@� �@�ƨ@��@�p�@̋D@�9X@���@˶F@ˍP@�\)@�K�@�
=@��H@ʧ�@�5?@���@�X@ț�@���@ǅ@���@ư!@�n�@�M�@ź^@�&�@��/@ě�@�A�@��@�@�^5@���@���@�A�@�t�@��H@�V@�@�X@��@�z�@��m@���@�+@���@�n�@�J@�hs@��@���@��@�C�@�@�@�
=@���@�ff@��@�x�@�7L@��@�A�@���@��@���@�~�@�E�@��@�`B@��@��9@�Q�@�  @�|�@�33@��H@���@�v�@�J@�?}@��@��D@�1'@�ƨ@�\)@�
=@�^5@��-@���@�G�@�Q�@�1@��;@��;@�1@��
@�;d@���@�E�@���@��h@��9@�Z@�(�@� �@�(�@�Q�@�(�@���@�33@�ȴ@�$�@���@�G�@��/@�Ĝ@��9@���@�A�@�b@���@��;@�dZ@�
=@���@��y@��@���@�~�@�-@��@��@��/@�Z@��F@�\)@�33@��@��R@��@���@��@�hs@�G�@���@��u@�Z@��
@�+@�ȴ@��\@�ff@�=q@���@���@��-@��h@��@��@���@�z�@�Z@�9X@��@��w@��P@�\)@�+@��@�n�@��@��^@��@�7L@�7L@�Ĝ@}�-@qG�@i��@`bN@XĜ@O�;@I7L@B�!@:�\@3t�@-�@'�@!G�@~�@E�@7L@V@	�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�K�A�M�A�O�A�O�A�Q�A�VA�I�A�A�A�=qA�7LA�$�A���Aȗ�Aȏ\Aȇ+A�z�A�jA�bNA�^5A�VA�M�A�G�A�?}A�9XA�5?A�1'A�(�A�"�A�VA�A�A���A���A�  A�  A���A���A�  A���A���A��A��/AǺ^A�x�A���A�7LA��HA�M�A�A� �A��mA�$�A�p�A�z�A�O�A���A�n�A�ĜA��PA�  A�A�33A���A��DA���A�+A��7A�oA���A���A��mA��RA���A��uA�jA��A��+A���A�33A�
=A���A��DA�dZA��\A�-A��A�A�A�A�p�A�33A�dZA���A�ȴA�^5A�M�A��A{��Ax{At�Ao��Ag\)Ac/A\�yAW
=AR��AP9XAL�yAHĜAE��AD�\AC�AA��A@M�A>z�A<�+A:ZA8A�A7�wA6�+A4�jA2ĜA.�`A-�wA,5?A+�A+;dA)�A'��A& �A%x�A&  A&bA%��A$��A#l�A$�A%�A!"�A r�A!�;A�#A�A��At�A�AA��A;dAn�A$�A{A$�AA�A9XAbA�mA�AbA��A�^A;dA��A��A��AĜAoA?}A��At�An�A�mA �AE�AƨAz�A&�A��AA\)A�A�9Av�A��A�uA��A��A�FA��A\)A-A�hAC�A��A��Az�A��A
�A
{A	��A�HAr�A�A1'A�TA�#A  A�AhsAXAA;dA\)A��AA�Al�A�AM�A��A��A��A�PA|�AhsA/A��AM�A�FA�A7LA �`A @�K�@��@�{@��@�`B@��u@��m@�ȴ@�5?@��7@���@�(�@�ff@��@�  @��@�@�@�@�hs@��@�j@�|�@�33@�^5@�@�?}@��/@�@�(�@��@�ƨ@�P@�=q@�hs@�z�@�1'@�ƨ@�K�@�\@�E�@�@�-@�9@��
@�+@�E�@�7@�Ĝ@�1@�|�@�33@���@��y@�~�@ݙ�@�/@܃@�b@ۮ@�C�@�ȴ@�~�@�{@��`@�bN@��@���@ם�@֗�@�ff@��#@�?}@��@�r�@ӥ�@�"�@���@�ff@�5?@��`@�Z@� �@�ƨ@��@�p�@̋D@�9X@���@˶F@ˍP@�\)@�K�@�
=@��H@ʧ�@�5?@���@�X@ț�@���@ǅ@���@ư!@�n�@�M�@ź^@�&�@��/@ě�@�A�@��@�@�^5@���@���@�A�@�t�@��H@�V@�@�X@��@�z�@��m@���@�+@���@�n�@�J@�hs@��@���@��@�C�@�@�@�
=@���@�ff@��@�x�@�7L@��@�A�@���@��@���@�~�@�E�@��@�`B@��@��9@�Q�@�  @�|�@�33@��H@���@�v�@�J@�?}@��@��D@�1'@�ƨ@�\)@�
=@�^5@��-@���@�G�@�Q�@�1@��;@��;@�1@��
@�;d@���@�E�@���@��h@��9@�Z@�(�@� �@�(�@�Q�@�(�@���@�33@�ȴ@�$�@���@�G�@��/@�Ĝ@��9@���@�A�@�b@���@��;@�dZ@�
=@���@��y@��@���@�~�@�-@��@��@��/@�Z@��F@�\)@�33@��@��R@��@���@��@�hs@�G�@���@��u@�Z@��
@�+@�ȴ@��\@�ff@�=q@���@���@��-@��h@��@��@���@�z�@�Z@�9X@��@��w@��P@�\)@�+@��@�n�@��@��^@��@�7L@�7L@�Ĝ@}�-@qG�@i��@`bN@XĜ@O�;@I7L@B�!@:�\@3t�@-�@'�@!G�@~�@E�@7L@V@	�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�TB	�BB	�B	�B	�B	�B	��B
B
bB
%�B
F�B
J�B
VB
n�B
�DB
�9B
�HB:^Bp�B��B�}BɺB�;B�B�B�yB�`B�NB�#B�B�B�B��BB�jB�-B�B��B��B�BffB=qB�B
��B
�B
�B
�B
��B
hsB
=qB
"�B
+B	�B	��B	�FB	��B	t�B	E�B	'�B		7B�B�BB�
B�B�5B�B��B��B��B��B	B	DB	+B	+B	B	1B	�B	�B	1B��B��B	B	PB	{B	\B	�B	�B	.B	>wB	M�B	N�B	W
B	�+B	�hB	x�B	{�B	�uB	�uB	�oB	�JB	�B	�%B	�PB	��B	��B	��B	��B	�B	�^B	B	ŢB	ƨB	ȴB	��B	�
B	�HB	�NB	�TB	�TB	�ZB	�sB	�B	��B
B
B	��B	�B	�B	��B
B
%B
B	��B	�B	�B	�B	�B	�B	�B	��B
B	��B
1B

=B
DB

=B
+B
B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	�B	��B	��B
B
B
  B
B
	7B
JB
JB
PB
DB
1B
PB
JB
PB
PB
hB
oB
oB
hB
bB
hB
hB
\B
PB

=B
+B
%B
+B
	7B
	7B

=B
JB
DB
DB
	7B
	7B
	7B
1B
+B
B
B
B
B
B
B
  B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
%B
%B
B
B
B
B
B
B
B
%B
%B
B
B
%B
%B
+B
+B
+B
+B
+B
+B
B
B
B
B
B
%B
1B
	7B
	7B
1B
+B
+B
1B
1B
	7B
	7B

=B
DB
PB
PB
\B
\B
\B
bB
\B
bB
hB
hB
hB
hB
oB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
{B
{B
uB
uB
oB
oB
oB
uB
uB
uB
uB
uB
uB
uB
uB
uB
uB
{B
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
#�B
)�B
/B
9XB
=qB
C�B
G�B
O�B
R�B
W
B
\)B
`BB
ffB
jB
o�B
v�B
z�B
}�B
�B
�%1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B
1B
�B
-B
H�B
M�B
[#B
u�B
��B
B
��BI�B�B��B��B�B�B��B��B��B��B�B�B�B�sB�yB�BB��BÖB�LB�LB�LB�'B��B�BZB33B
=BB
��B
ŢB
ÖB
�DB
Q�B
=qB
�B
%B	�B	��B	B	��B	ffB	S�B	-B	JB�B�B�B�B��B	B	%B	%B	%B	hB	�B	uB	PB	VB	�B	)�B	5?B	hB		7B	B	
=B	�B	"�B	�B	�B	�B	/B	B�B	S�B	VB	Q�B	�VB	��B	{�B	y�B	��B	��B	��B	��B	�1B	�B	�JB	��B	��B	��B	��B	�B	�^B	ÖB	ƨB	ǮB	ȴB	��B	�B	�TB	�fB	�fB	�ZB	�ZB	�mB	�B	��B
	7B

=B
  B	��B	�B	��B

=B
hB
DB	��B	��B	�B	�B	�B	�B	�B	��B
+B	��B
	7B
JB
bB
oB
DB
B
B
  B
B
B
  B	��B	��B	��B	�B	�B	��B	��B	�B	��B	��B
%B
B
B
  B

=B
hB
oB
uB
\B
PB
bB
PB
\B
VB
oB
{B
�B
{B
�B
�B
{B
uB
hB
bB

=B
1B

=B
DB
JB
VB
bB
\B
VB
PB
PB
VB
bB
PB

=B
	7B
%B
B
B
B
B
B
B
  B
B
B
B	��B	��B	��B	��B	��B	��B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
+B
B
B
%B
	7B
+B
B
B
B
B
B
+B
1B
1B
1B
B
%B
%B
+B
+B
+B
+B
+B
+B
%B
B
B
B
B
%B
1B
	7B
	7B
1B
+B
	7B

=B
DB
	7B
	7B

=B
DB
PB
PB
hB
\B
\B
uB
hB
bB
uB
hB
oB
oB
oB
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
uB
�B
oB
{B
uB
{B
uB
�B
uB
{B
{B
uB
�B
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
$�B
)�B
/B
9XB
=qB
C�B
H�B
O�B
S�B
W
B
\)B
`BB
ffB
jB
p�B
v�B
z�B
}�B
�B
�%1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�1<���<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<e`B<��
<u<u<e`B<���<�o<e`B<#�
<#�
<e`B<���<�o<��
<�o<u<���<���<�t�<#�
<#�
<#�
<���<�/<�/<�h<�`B<�1<T��<��
=\)<�j=0 �=C�<��
<���<�j<���<���<���=�w=P�`=o=0 �=\)<���<���<���<ě�<�o<#�
<#�
<#�
<49X<T��<e`B<u<D��<#�
<#�
<u<�o<�j<#�
<#�
<#�
<#�
<T��<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�9X<#�
<#�
<�o<49X<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.4 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201131250312012011312503120120113125031  AO  ARGQ                                                                        20111205113530  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111205113530  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120113125031  IP                  G�O�G�O�G�O�                