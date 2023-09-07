CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:08Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005191708  20181005191708  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               XA   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��d����1   @��el� @4�hr�!�d7�E���1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      XA   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B ffB��B��B��B��B'��B0  B8ffB@  BG��BO��BX  B`  Bh  Bp  Bx  B�  B�  B�  B���B�  B�  B���B�  B�33B�33B�33B�  B���B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  C   C�fC�fC  C  C
  C�fC  C  C  C  C�fC�fC  C�C  C   C"  C$  C&  C(  C*  C+�fC.  C0�C2�C4�C6  C7�fC9�fC<  C=�fC?�fCA�fCD  CF  CH�CJ  CK�fCM�fCO�fCR  CS�fCU�fCX  CZ�C\  C]�fC`  Cb�Cd  Ce�fCh  Cj  Cl  Cn  Cp�Cr  Cs�fCv  Cw�fCz  C|  C}�fC�  C�  C�  C��C��C��C�  C��3C�  C��C��C�  C�  C��3C��3C��3C�  C�  C��C�  C��C�  C�  C�  C��3C�  C��C��C�  C�  C��C�  C�  C��3C�  C��C��C��C��C��C��C��C��C��C�  C��C��C��3C��3C��3C��3C��C�  C��C��C�  C��3C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C��3C��3C��3C��3C��3C�  C�  C��3C�  C��3C�  C�  C��3C��C��3C��3C�  C�  C��fC�  C��C�  C��3C��C��3C�  C��C��C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C��C�  C�  C�  C�  C��3C��3C��C�  C��C��3C�  C�  C�  C��3C�  C�  D   D �fDfD�fD  Dy�D��Dy�D��D� DfD��D  Dy�DfD� D��D� D	  D	� D
fD
� D
��D� DfD� DfDy�D  D�fD��D� D��Dy�D��D� DfD�fDfD� D��D� DfDy�D  D� D  Dy�D  D�fD  D�fDfD�fD�D�fD  D� D��D� D  D� D  D� D��D �fD!  D!� D"fD"� D#fD#�fD$fD$� D%  D%� D%��D&y�D'fD'�fD(fD(y�D)fD)� D*  D*� D+fD+� D,  D,�fD-fD-�fD.  D.� D/fD/� D/��D0y�D1fD1� D2fD2��D3fD3�fD4fD4� D5  D5y�D5��D6� D7fD7� D8fD8� D8��D9y�D:  D:y�D:��D;y�D;��D<�fD<��D=� D=��D>� D>��D?y�D@  D@�fD@�3DAs3DA��DB� DB��DC� DD  DD� DEfDEs3DE��DF�fDF��DG� DH  DH� DI  DIy�DI��DJ�fDK  DK� DK��DL� DM  DM� DNfDN� DN��DOy�DO��DP�fDQfDQ�fDRfDR� DS  DS�fDT  DTy�DU  DU�fDU�3DVy�DV��DW� DW��DX�fDX��DYy�DY�3DZy�DZ�3D[y�D\  D\� D]  D]� D^  D^y�D_  D_� D`  D`�fDa  Da��Db  Dby�DcfDcy�DdfDdy�De  De�fDf  Df� Dg  Dg� Dg��Dh� DifDi� DjfDj�fDj�3Dky�Dk��Dly�DmfDm�fDm��Dn��Do  Do� Dp  Dpy�DqfDq� Dr  Dr�fDr��Ds�fDt  Dty�DufDu� Dv  Dv� Dw  Dw�fDw�3Dy��D�O
D��H111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@ə�A��A$��AD��Ad��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�33B��B��B��B��B ��B(��B133B9��BA33BH��BP��BY33Ba33Bi33Bq33By33B���B���B���B�fgB���B���B�fgB���B���B���B���B���B�fgB���B���B���B���Bę�Bș�B̙�BЙ�Bԙ�Bؙ�Bܙ�B���B䙚B虚B왚B�B���B���B���C L�C33C33CL�CL�C
L�C33CL�CL�CL�CL�C33C33CL�CfgCL�C L�C"L�C$L�C&L�C(L�C*L�C,33C.L�C0fgC2fgC4fgC6L�C833C:33C<L�C>33C@33CB33CDL�CFL�CHfgCJL�CL33CN33CP33CRL�CT33CV33CXL�CZfgC\L�C^33C`L�CbfgCdL�Cf33ChL�CjL�ClL�CnL�CpfgCrL�Ct33CvL�Cx33CzL�C|L�C~33C�&fC�&fC�&fC�33C�33C�33C�&fC��C�&fC�33C�33C�&fC�&fC��C��C��C�&fC�&fC�33C�&fC�33C�&fC�&fC�&fC��C�&fC�33C�33C�&fC�&fC�33C�&fC�&fC��C�&fC�33C�33C�@ C�33C�33C�33C�33C�33C�33C�&fC�33C�33C��C��C��C��C�33C�&fC�33C�33C�&fC��C�&fC�&fC�&fC��C�&fC�&fC�&fC�&fC�33C�&fC�&fC�&fC��C��C��C��C��C�&fC�&fC��C�&fC��C�&fC�&fC��C�33C��C��C�&fC�&fC��C�&fC�33C�&fC��C�33C��C�&fC�33C�33C��C�&fC�&fC�&fC�&fC�&fC�33C�&fC�&fC�&fC�&fC�&fC�&fC��C�33C�&fC�&fC�&fC�&fC��C��C�33C�&fC�33C��C�&fC�&fC�&fC��C�&fC�&fD 3D ��D�D��D3D��D�D��D�D�3D�D� D3D��D�D�3D�D�3D	3D	�3D
�D
�3D�D�3D�D�3D�D��D3D��D�D�3D�D��D�D�3D�D��D�D�3D�D�3D�D��D3D�3D3D��D3D��D3D��D�D��D  D��D3D�3D�D�3D3D�3D3D�3D �D ��D!3D!�3D"�D"�3D#�D#��D$�D$�3D%3D%�3D&�D&��D'�D'��D(�D(��D)�D)�3D*3D*�3D+�D+�3D,3D,��D-�D-��D.3D.�3D/�D/�3D0�D0��D1�D1�3D2�D2� D3�D3��D4�D4�3D53D5��D6�D6�3D7�D7�3D8�D8�3D9�D9��D:3D:��D;�D;��D<�D<��D=�D=�3D>�D>�3D?�D?��D@3D@��DAfDA�fDB�DB�3DC�DC�3DD3DD�3DE�DE�fDF�DF��DG�DG�3DH3DH�3DI3DI��DJ�DJ��DK3DK�3DL�DL�3DM3DM�3DN�DN�3DO�DO��DP�DP��DQ�DQ��DR�DR�3DS3DS��DT3DT��DU3DU��DVfDV��DW�DW�3DX�DX��DY�DY��DZfDZ��D[fD[��D\3D\�3D]3D]�3D^3D^��D_3D_�3D`3D`��Da3Da� Db3Db��Dc�Dc��Dd�Dd��De3De��Df3Df�3Dg3Dg�3Dh�Dh�3Di�Di�3Dj�Dj��DkfDk��Dl�Dl��Dm�Dm��Dn�Dn� Do3Do�3Dp3Dp��Dq�Dq�3Dr3Dr��Ds�Ds��Dt3Dt��Du�Du�3Dv3Dv�3Dw3Dw��Dw�fDy��D�X�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�dZA�`BA�O�A�9XA�bA��`A��
A���A���A���A�ĜAܾwAܶFAܬA���A�VAҴ9A�1'A��HA�A�O�A��Aɴ9A���A�A��AA��+A�1'A��^A�{A�|�A�K�A�%A��A�t�A�hsA���A�`BA���A���A��A���A��-A�dZA�;dA� �A��A��A���A��7A�n�A�dZA�I�A���A��PA�
=A�t�A�|�A���A�bA�bNA���A�dZA���A���A�ȴA���A�M�A�A��`A�
=A�{A�^5A�hsA�A��wA�VA�{A���A�\)A�n�A��+A�~�A��;A��A|�A}�A{��A{dZAz5?Aw��Av �Aux�At�AtZAq��AnM�Al�+Aj��AhE�Ag�Ad��AcS�Aa�TA`�uA^1A[��AX�+AUhsAQƨAN1AJ��AI�AG7LADbABZA@�RA?;dA=��A<�A;�A:�A8��A8�A7XA5/A4��A3dZA1��A0�A0�jA/��A.M�A-�A-+A,��A,~�A,5?A+ƨA+"�A)XA&ZA%�;A%�wA%%A$�uA$�A#�-A#\)A"v�A!
=A M�A �A�#A+A1'A`BA~�A�-A"�A��AI�A�mA?}A�AE�A��Ap�AoAr�A�^A�AS�A"�A�A�A�/A�FA��A=qA33A��AffA  A
=A	�AI�Al�A&�A�uA(�A�`AffA"�Al�@��m@�=q@��j@�"�@�X@���@�$�@�1@���@�v�@�7@�ƨ@��@�{@�j@�+@���@��@�G�@�@�;d@�v�@�-@�hs@���@�w@�33@��y@��@��;@�n�@ܣ�@��@��m@���@ڰ!@ڏ\@�v�@�^5@�-@�@١�@���@�33@�@ְ!@�G�@�1'@��
@�|�@��@ҟ�@��#@�z�@��@�\)@θR@�M�@�&�@̼j@�1@�{@ə�@�j@ƸR@�$�@�X@å�@��@��@�bN@�^5@�@��T@��@�@�@��^@�V@�A�@�ƨ@�K�@���@���@�7L@���@�r�@���@��F@��@�K�@�;d@�K�@�33@���@��@�@��-@�x�@�%@�C�@�"�@�Ĝ@�V@�7L@��@��@��\@��R@��@��P@�{@�&�@�z�@���@���@�hs@���@���@��@�(�@�1'@�Z@��u@���@�Ĝ@�I�@�(�@�b@�9X@��@���@���@�1@�1@���@�;d@�ȴ@�E�@�$�@�J@�{@�n�@��@�ff@�n�@�E�@��@�5?@���@��H@���@�^5@�@�=q@�n�@�V@�-@��@���@��#@��T@��^@���@��7@��@��/@��D@�Q�@� �@��@���@��
@���@��@���@�|�@�\)@�;d@�"�@���@��H@��H@��+@�J@���@��-@���@��7@�x�@�X@��@���@��9@�bN@�  @��;@���@�S�@�
=@�@��y@��R@���@�n�@�$�@���@���@��@�p�@�`B@�X@�G�@��@�V@��@�r�@�Z@�1'@��w@��P@�dZ@�33@�K�@�K�@�C�@��@��R@�v�@�5?@���@��^@�O�@�j@��@��
@��@��P@�S�@���@�~�@�$�@��@�`B@�?}@�%@���@��9@�Z@� �@�1@��
@�
=@���@���@���@���@�ff@�n�@�v�@�v�@�-@��h@�X@�G�@�/@��@��D@�Z@��@��w@�t�@�"�@�5?@��@���@�hs@�?}@��9@��D@�Z@�(�@�b@���@�K�@��@��@��!@�=q@��@���@�hs@�G�@�7L@�/@��@�%@���@��@��$@kb�@_�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�dZA�`BA�O�A�9XA�bA��`A��
A���A���A���A�ĜAܾwAܶFAܬA���A�VAҴ9A�1'A��HA�A�O�A��Aɴ9A���A�A��AA��+A�1'A��^A�{A�|�A�K�A�%A��A�t�A�hsA���A�`BA���A���A��A���A��-A�dZA�;dA� �A��A��A���A��7A�n�A�dZA�I�A���A��PA�
=A�t�A�|�A���A�bA�bNA���A�dZA���A���A�ȴA���A�M�A�A��`A�
=A�{A�^5A�hsA�A��wA�VA�{A���A�\)A�n�A��+A�~�A��;A��A|�A}�A{��A{dZAz5?Aw��Av �Aux�At�AtZAq��AnM�Al�+Aj��AhE�Ag�Ad��AcS�Aa�TA`�uA^1A[��AX�+AUhsAQƨAN1AJ��AI�AG7LADbABZA@�RA?;dA=��A<�A;�A:�A8��A8�A7XA5/A4��A3dZA1��A0�A0�jA/��A.M�A-�A-+A,��A,~�A,5?A+ƨA+"�A)XA&ZA%�;A%�wA%%A$�uA$�A#�-A#\)A"v�A!
=A M�A �A�#A+A1'A`BA~�A�-A"�A��AI�A�mA?}A�AE�A��Ap�AoAr�A�^A�AS�A"�A�A�A�/A�FA��A=qA33A��AffA  A
=A	�AI�Al�A&�A�uA(�A�`AffA"�Al�@��m@�=q@��j@�"�@�X@���@�$�@�1@���@�v�@�7@�ƨ@��@�{@�j@�+@���@��@�G�@�@�;d@�v�@�-@�hs@���@�w@�33@��y@��@��;@�n�@ܣ�@��@��m@���@ڰ!@ڏ\@�v�@�^5@�-@�@١�@���@�33@�@ְ!@�G�@�1'@��
@�|�@��@ҟ�@��#@�z�@��@�\)@θR@�M�@�&�@̼j@�1@�{@ə�@�j@ƸR@�$�@�X@å�@��@��@�bN@�^5@�@��T@��@�@�@��^@�V@�A�@�ƨ@�K�@���@���@�7L@���@�r�@���@��F@��@�K�@�;d@�K�@�33@���@��@�@��-@�x�@�%@�C�@�"�@�Ĝ@�V@�7L@��@��@��\@��R@��@��P@�{@�&�@�z�@���@���@�hs@���@���@��@�(�@�1'@�Z@��u@���@�Ĝ@�I�@�(�@�b@�9X@��@���@���@�1@�1@���@�;d@�ȴ@�E�@�$�@�J@�{@�n�@��@�ff@�n�@�E�@��@�5?@���@��H@���@�^5@�@�=q@�n�@�V@�-@��@���@��#@��T@��^@���@��7@��@��/@��D@�Q�@� �@��@���@��
@���@��@���@�|�@�\)@�;d@�"�@���@��H@��H@��+@�J@���@��-@���@��7@�x�@�X@��@���@��9@�bN@�  @��;@���@�S�@�
=@�@��y@��R@���@�n�@�$�@���@���@��@�p�@�`B@�X@�G�@��@�V@��@�r�@�Z@�1'@��w@��P@�dZ@�33@�K�@�K�@�C�@��@��R@�v�@�5?@���@��^@�O�@�j@��@��
@��@��P@�S�@���@�~�@�$�@��@�`B@�?}@�%@���@��9@�Z@� �@�1@��
@�
=@���@���@���@���@�ff@�n�@�v�@�v�@�-@��h@�X@�G�@�/@��@��D@�Z@��@��w@�t�@�"�@�5?@��@���@�hs@�?}@��9@��D@�Z@�(�@�b@���@�K�@��@��@��!@�=q@��@���@�hs@�G�@�7L@�/@��@�%@���@��@��$@kb�@_�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B+B+B(�B&�B$�B#�B#�B#�B#�B#�B#�B#�B"�B!�B�BB��B��B	7B	7BPB�B�B(�B?}BQ�BbNBk�Bm�Bq�Bt�B�B�=B�{B��B��B��B��B�B�B�B��B�B��B��B��B��B��B�oB�PBz�BiyB]/B>wB�BDB	7BB��B�B�#B��B�wB�XB�9B�?B�3B��B�bB�By�BiyBXBL�B?}B.B�BB
�ZB
��B
ɺB
�jB
��B
��B
� B
^5B
G�B
8RB
.B
+B
�B
PB
B	��B	��B	�B	�;B	ȴB	�dB	�!B	��B	�{B	�1B	}�B	t�B	k�B	\)B	M�B	;dB	+B	�B	1B��B��B�B�TB�/B�
B��B��BɺBǮBĜB��B�qB�^B�9B�!B�B�B�B��B��B��B��B��B��B��B��B��B��B��B�oB�hB�oB�uB�uB�oB�oB�hB�oB�{B�uB�uB�uB�uB�{B�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�-B�?B�FB�XB�^B�dB�qB��BBÖBÖBÖBBĜB��B��B��B�B�)B�)B�)B�B�B�#B�HB�B�B��B��B��B��B��B	B	+B	
=B	JB	VB	{B	�B	�B	�B	�B	�B	�B	"�B	$�B	$�B	%�B	(�B	+B	,B	-B	0!B	0!B	,B	�B	{B	�B	�B	hB	uB	�B	#�B	"�B	$�B	$�B	!�B	�B	�B	�B	$�B	0!B	9XB	9XB	;dB	?}B	C�B	I�B	S�B	]/B	cTB	e`B	ffB	l�B	o�B	q�B	r�B	v�B	y�B	x�B	x�B	z�B	z�B	~�B	�B	�%B	�=B	�JB	�hB	�oB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�B	�B	�B	�B	�!B	�'B	�-B	�-B	�9B	�FB	�XB	�wB	�}B	��B	��B	��B	B	B	ÖB	ĜB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�)B	�)B	�/B	�5B	�BB	�ZB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
%B
+B
1B
+B
+B
1B
	7B
	7B

=B
DB
DB
DB
PB
PB
PB
PB
PB
VB
\B
\B
\B
\B
bB
\B
\B
bB
bB
bB
hB
oB
oB
oB
oB
oB
uB
uB
uB
{B
�B
�B
1�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B+B+B(�B&�B$�B#�B#�B#�B#�B#�B#�B#�B"�B!�B�BB��B��B	7B	7BPB�B�B(�B?}BQ�BbNBk�Bm�Bq�Bt�B�B�=B�{B��B��B��B��B�B�B�B��B�B��B��B��B��B��B�oB�PBz�BiyB]/B>wB�BDB	7BB��B�B�#B��B�wB�XB�9B�?B�3B��B�bB�By�BiyBXBL�B?}B.B�BB
�ZB
��B
ɺB
�jB
��B
��B
� B
^5B
G�B
8RB
.B
+B
�B
PB
B	��B	��B	�B	�;B	ȴB	�dB	�!B	��B	�{B	�1B	}�B	t�B	k�B	\)B	M�B	;dB	+B	�B	1B��B��B�B�TB�/B�
B��B��BɺBǮBĜB��B�qB�^B�9B�!B�B�B�B��B��B��B��B��B��B��B��B��B��B��B�oB�hB�oB�uB�uB�oB�oB�hB�oB�{B�uB�uB�uB�uB�{B�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�-B�?B�FB�XB�^B�dB�qB��BBÖBÖBÖBBĜB��B��B��B�B�)B�)B�)B�B�B�#B�HB�B�B��B��B��B��B��B	B	+B	
=B	JB	VB	{B	�B	�B	�B	�B	�B	�B	"�B	$�B	$�B	%�B	(�B	+B	,B	-B	0!B	0!B	,B	�B	{B	�B	�B	hB	uB	�B	#�B	"�B	$�B	$�B	!�B	�B	�B	�B	$�B	0!B	9XB	9XB	;dB	?}B	C�B	I�B	S�B	]/B	cTB	e`B	ffB	l�B	o�B	q�B	r�B	v�B	y�B	x�B	x�B	z�B	z�B	~�B	�B	�%B	�=B	�JB	�hB	�oB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�B	�B	�B	�B	�!B	�'B	�-B	�-B	�9B	�FB	�XB	�wB	�}B	��B	��B	��B	B	B	ÖB	ĜB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�)B	�)B	�/B	�5B	�BB	�ZB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
%B
+B
1B
+B
+B
1B
	7B
	7B

=B
DB
DB
DB
PB
PB
PB
PB
PB
VB
\B
\B
\B
\B
bB
\B
\B
bB
bB
bB
hB
oB
oB
oB
oB
oB
uB
uB
uB
{B
�B
�B
1�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.30 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191708                              AO  ARCAADJP                                                                    20181005191708    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191708  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191708  QCF$                G�O�G�O�G�O�8000            