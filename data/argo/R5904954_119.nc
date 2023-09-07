CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:15Z creation      
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
resolution        =���   axis      Z        l  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  L    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  Sl   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  mh   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �$   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �4   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �8   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �<   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181005191715  20181005191715  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               wA   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��$���1   @��&F)��@4�=p��
�d`�j~��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      wA   A   B   @�  @�  @���AffA@  A`  A�  A�  A�  A���A�  A�  A�  A�  B   B  B  BffB ffB(  B0  B8  B?��BH  BP  BXffB`  Bh  Bp  Bx  B�  B���B�  B���B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B���B���B�  B�  B�33B�33B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<�C>  C?�fCA�fCD  CF  CH�CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\�C^�C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cu�fCx  Cz  C{�fC~  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C��3C�  C�  C��3C��3C�  C�  C��3C��3C�  C�  C��3C�  C��3C�  C�  C�  C��C��C�  C��C��C��3C�  C��3C��C�  C�  C�  C�  C��3C�  C��3C��C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C��C��C��3C�  C��C�  C�  C�  C�  C�  C��3C��3C��C�  C��3C��C�  C�  C��C�  C��C��3C��C��C��C��C��3C�  C�  C�  C�  C�  C��C��3C�  C�  C�  C�  D   D y�D �3D� D  Dy�DfD�fDfDy�D��D�fD��D� DfD�fDfD� D	  D	y�D	��D
y�DfD��D  D� DfDy�D  Dy�D��D�fDfD� DfDs3D  D� D  D� DfD�fD  D� D  Dy�D  D�fD  Dy�D��D� D  D�fDfD� DfD�fDfD�fDfD��DfD�fD fD � D ��D!y�D!��D"y�D"�3D#y�D$  D$� D%fD%�fD&�D&�fD'fD'� D(  D(� D)fD)� D*fD*� D*��D+� D,fD,� D-fD-� D-��D.y�D/fD/�fD0�D0�fD1fD1� D2  D2� D2��D3� D4  D4y�D4��D5� D6  D6��D7fD7� D8  D8�fD9fD9�fD:  D:� D:��D;� D<  D<y�D=fD=�fD>fD>� D>��D?�fD@fD@y�DA  DA�fDBfDB�fDCfDC� DD  DD� DE  DE� DF  DF�fDG  DG� DH  DHy�DH��DI� DJ  DJ� DJ��DKy�DK��DL� DM  DMy�DN  DN�fDO  DO�fDP  DP� DP��DQy�DR  DR� DSfDS�fDT  DT� DU  DU� DV  DV� DWfDW� DX  DXy�DY  DY� DZfDZ� D[  D[�fD\fD\� D\��D]y�D^  D^�fD_  D_�fD`  D`y�D`��Day�Da��Dby�DcfDc�fDd  Dd� De  Dey�De��Df� Dg  Dgy�Dh  Dh� Di  Di�fDjfDj� Dk  Dky�Dk��Dl�fDmfDm�fDn  Dn�fDo  Doy�Dp  Dp� Dq  Dq� Dr  Dr�fDsfDs�fDtfDt� Du  Du� Du��Dv�fDw  Dwy�DwٚDy�fD�N�D��
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@ə�A33A#33AD��Ad��A�ffA�ffA�ffA�33A�ffA�ffA�ffA�ffB33B	33B33B��B!��B)33B133B933B@��BI33BQ33BY��Ba33Bi33Bq33By33B���B�fgB���B�fgB�fgB�fgB���B���B���B���B���B���B���B���B���B���B���Bę�Bș�B���B���Bԙ�B�fgB�fgB���B䙚B���B���B�B���B���B���C L�CL�CL�CL�CL�C
L�CL�CL�CL�CL�CL�CL�CL�CL�CL�CL�C L�C"L�C$L�C&L�C(L�C*L�C,L�C.L�C0L�C2L�C4L�C6L�C8L�C:L�C<fgC>L�C@33CB33CDL�CFL�CHfgCJL�CLL�CNL�CPL�CRL�CTL�CVL�CXL�CZL�C\fgC^fgC`L�CbL�CdL�CfL�ChL�CjL�ClL�CnL�CpL�CrL�CtL�Cv33CxL�CzL�C|33C~L�C�&fC��C�&fC�&fC��C�&fC�&fC�&fC�&fC�&fC��C�&fC�&fC�&fC�&fC��C��C��C�&fC�&fC�&fC�&fC�&fC��C�&fC�&fC��C��C�&fC�&fC��C��C�&fC�&fC��C�&fC��C�&fC�&fC�&fC�33C�33C�&fC�33C�33C��C�&fC��C�33C�&fC�&fC�&fC�&fC��C�&fC��C�33C�&fC�&fC�33C�&fC�&fC�&fC�&fC�&fC��C�&fC�33C�33C��C�&fC�33C�&fC�&fC�&fC�&fC�&fC��C��C�33C�&fC��C�33C�&fC�&fC�33C�&fC�33C��C�33C�@ C�@ C�33C��C�&fC�&fC�&fC�&fC�&fC�33C��C�&fC�&fC�&fC�&fD 3D ��DfD�3D3D��D�D��D�D��D�D��D�D�3D�D��D�D�3D	3D	��D
�D
��D�D� D3D�3D�D��D3D��D�D��D�D�3D�D�fD3D�3D3D�3D�D��D3D�3D3D��D3D��D3D��D�D�3D3D��D�D�3D�D��D�D��D�D� D�D��D �D �3D!�D!��D"�D"��D#fD#��D$3D$�3D%�D%��D&  D&��D'�D'�3D(3D(�3D)�D)�3D*�D*�3D+�D+�3D,�D,�3D-�D-�3D.�D.��D/�D/��D0  D0��D1�D1�3D23D2�3D3�D3�3D43D4��D5�D5�3D63D6� D7�D7�3D83D8��D9�D9��D:3D:�3D;�D;�3D<3D<��D=�D=��D>�D>�3D?�D?��D@�D@��DA3DA��DB�DB��DC�DC�3DD3DD�3DE3DE�3DF3DF��DG3DG�3DH3DH��DI�DI�3DJ3DJ�3DK�DK��DL�DL�3DM3DM��DN3DN��DO3DO��DP3DP�3DQ�DQ��DR3DR�3DS�DS��DT3DT�3DU3DU�3DV3DV�3DW�DW�3DX3DX��DY3DY�3DZ�DZ�3D[3D[��D\�D\�3D]�D]��D^3D^��D_3D_��D`3D`��Da�Da��Db�Db��Dc�Dc��Dd3Dd�3De3De��Df�Df�3Dg3Dg��Dh3Dh�3Di3Di��Dj�Dj�3Dk3Dk��Dl�Dl��Dm�Dm��Dn3Dn��Do3Do��Dp3Dp�3Dq3Dq�3Dr3Dr��Ds�Ds��Dt�Dt�3Du3Du�3Dv�Dv��Dw3Dw��Dw��Dy��D�XRD�ؤ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�XA�ZA�XA�XA�XA�XA�ZA�ZA�\)A�\)A�\)A�`BA�`BA�`BA�bNA�`BA�XA�"�A���A۝�Aڴ9A�~�A԰!A�K�A��/A�G�A�t�A��A�r�Aţ�A��
A��HA��A�I�A���A��`A��A�?}A�oA��A���A�ffA��A���A�x�A�+A���A�A�A���A��9A���A��A���A���A�"�A�+A�(�A�ĜA�A�A�A���A��A�?}A�hsA�?}A�JA�ĜA�r�A���A�A�A��A�K�A���A�dZA�C�A���A�r�A�
=A��yA��\A�bA�5?A���A�ffA�+A�dZA�(�A��RA��A��A���A�XA�E�A�33A�  A��A�ĜA���A��;A�9XA~��A{+Az{Av��Ar�uApr�Al��Ak/Ai?}Agp�AeC�Aa�A]�PAZQ�AWATE�AR�AQC�AP��AO��AO�AM�AKt�AJ�yAI�AGAFJAEG�AD �AB��AA�
AAA>  A<��A;C�A:�A9A9\)A8��A7K�A6A�A6{A5;dA3��A3C�A2E�A0ȴA/��A.9XA-�A+�A+�A)�#A)7LA'��A&�uA%��A%�7A$��A#S�A#�A"�RA!�A ��A33A�;A��A5?A^5A�RA�/AĜA(�A�Al�A ��A r�@��!@���@���@���@���@��7@�o@���@�X@�ff@��@�x�@�?}@��@�9@�o@�@��m@��@�9X@���@�V@�ff@�z�@�bN@�X@���@�Z@և+@���@��T@���@�j@���@�v�@�Ĝ@���@�M�@�X@Å@��#@���@��@�/@�  @��\@�`B@���@��m@��
@�A�@�+@�z�@�~�@���@��w@� �@���@��@�O�@�C�@�G�@�n�@�9X@�5?@� �@�o@��h@�O�@���@�\)@�J@���@�  @�/@�-@�ȴ@�-@�{@��@�ff@�+@�^5@��
@��@�"�@���@�
=@���@��h@�hs@��u@��@���@��`@�`B@���@���@�|�@�G�@�K�@�&�@��@��T@�o@��D@���@��@�Z@�1'@���@��w@��w@��w@���@�33@��+@�$�@�V@��@���@��\@�E�@�M�@��@�5?@��@��@�p�@���@��m@��;@��@��m@��;@�ƨ@��P@�;d@��R@���@��+@�n�@�V@�{@���@��^@�z�@�-@���@���@��@��j@��j@��j@�Ĝ@���@�A�@�1@�A�@�  @�dZ@�"�@�@��@�ȴ@�@��@��H@�-@��^@��-@��^@���@��-@��7@��@��@�V@��`@�A�@��P@�
=@���@���@�-@�M�@�V@�{@��#@���@��-@�&�@��@��j@�j@�9X@��@��@���@��P@�S�@�@��@��R@��+@�J@���@�@��7@�hs@�?}@���@�bN@�A�@���@�;d@��R@�E�@�{@���@��h@�`B@�&�@���@��/@���@��u@�Z@��@��m@���@�t�@��P@��P@�t�@�C�@�o@��@�ff@���@�@�/@�Ĝ@�9X@�(�@�b@�ƨ@��@�~�@�^5@�=q@��#@��@�hs@�?}@�&�@��@�V@��9@�bN@�9X@�1'@� �@���@��@���@�t�@�dZ@�;d@�ȴ@��\@��\@��+@��@��@��@�@��@� �@��@��
@�dZ@���@���@�n�@��@��@�G�@�/@�/@��@��`@��9@��@�j@~�@m��@]+�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�XA�ZA�XA�XA�XA�XA�ZA�ZA�\)A�\)A�\)A�`BA�`BA�`BA�bNA�`BA�XA�"�A���A۝�Aڴ9A�~�A԰!A�K�A��/A�G�A�t�A��A�r�Aţ�A��
A��HA��A�I�A���A��`A��A�?}A�oA��A���A�ffA��A���A�x�A�+A���A�A�A���A��9A���A��A���A���A�"�A�+A�(�A�ĜA�A�A�A���A��A�?}A�hsA�?}A�JA�ĜA�r�A���A�A�A��A�K�A���A�dZA�C�A���A�r�A�
=A��yA��\A�bA�5?A���A�ffA�+A�dZA�(�A��RA��A��A���A�XA�E�A�33A�  A��A�ĜA���A��;A�9XA~��A{+Az{Av��Ar�uApr�Al��Ak/Ai?}Agp�AeC�Aa�A]�PAZQ�AWATE�AR�AQC�AP��AO��AO�AM�AKt�AJ�yAI�AGAFJAEG�AD �AB��AA�
AAA>  A<��A;C�A:�A9A9\)A8��A7K�A6A�A6{A5;dA3��A3C�A2E�A0ȴA/��A.9XA-�A+�A+�A)�#A)7LA'��A&�uA%��A%�7A$��A#S�A#�A"�RA!�A ��A33A�;A��A5?A^5A�RA�/AĜA(�A�Al�A ��A r�@��!@���@���@���@���@��7@�o@���@�X@�ff@��@�x�@�?}@��@�9@�o@�@��m@��@�9X@���@�V@�ff@�z�@�bN@�X@���@�Z@և+@���@��T@���@�j@���@�v�@�Ĝ@���@�M�@�X@Å@��#@���@��@�/@�  @��\@�`B@���@��m@��
@�A�@�+@�z�@�~�@���@��w@� �@���@��@�O�@�C�@�G�@�n�@�9X@�5?@� �@�o@��h@�O�@���@�\)@�J@���@�  @�/@�-@�ȴ@�-@�{@��@�ff@�+@�^5@��
@��@�"�@���@�
=@���@��h@�hs@��u@��@���@��`@�`B@���@���@�|�@�G�@�K�@�&�@��@��T@�o@��D@���@��@�Z@�1'@���@��w@��w@��w@���@�33@��+@�$�@�V@��@���@��\@�E�@�M�@��@�5?@��@��@�p�@���@��m@��;@��@��m@��;@�ƨ@��P@�;d@��R@���@��+@�n�@�V@�{@���@��^@�z�@�-@���@���@��@��j@��j@��j@�Ĝ@���@�A�@�1@�A�@�  @�dZ@�"�@�@��@�ȴ@�@��@��H@�-@��^@��-@��^@���@��-@��7@��@��@�V@��`@�A�@��P@�
=@���@���@�-@�M�@�V@�{@��#@���@��-@�&�@��@��j@�j@�9X@��@��@���@��P@�S�@�@��@��R@��+@�J@���@�@��7@�hs@�?}@���@�bN@�A�@���@�;d@��R@�E�@�{@���@��h@�`B@�&�@���@��/@���@��u@�Z@��@��m@���@�t�@��P@��P@�t�@�C�@�o@��@�ff@���@�@�/@�Ĝ@�9X@�(�@�b@�ƨ@��@�~�@�^5@�=q@��#@��@�hs@�?}@�&�@��@�V@��9@�bN@�9X@�1'@� �@���@��@���@�t�@�dZ@�;d@�ȴ@��\@��\@��+@��@��@��@�@��@� �@��@��
@�dZ@���@���@�n�@��@��@�G�@�/@�/@��@��`@��9@��@�j@~�@m��@]+�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBbNBl�Bl�BjBbNBT�BG�BL�BO�BR�BT�BW
BXBZBbNBk�Bu�By�B|�B~�B�B�%B�%B�+B�+B�B�7B�VB�oB�oB�bB�VB�\B�DB�7B�1B�%B�Bw�Bm�BgmBcTB[#BM�BA�B8RB)�B �B�B�B�BoB1B�B�5B��B��B�B�hB}�Bw�Bp�BaHBM�BE�B8RB#�B\B
��B
�fB
��B
��B
��B
ĜB
�}B
�XB
�B
��B
��B
��B
�+B
hsB
^5B
Q�B
9XB
�B
uB	��B	�HB	��B	�jB	�'B	��B	��B	�PB	v�B	dZB	T�B	F�B	5?B	(�B	$�B	!�B	�B	�B	VB	B��B��B�B�mB�TB�5B�B�
B��B��BƨBB�}B�wB�jB�^B�FB�9B�3B�'B�B�B��B��B��B��B��B��B��B�{B�oB�bB�PB�PB�DB�DB�=B�=B�7B�7B�7B�=B]O�BO�BP�BS�BffBx�B{�B{�By�Bt�Bs�Bn�Bl�Br�Bn�Be`BdZB_;BW
BT�BM�BQ�BZBaHBjBq�Br�Bv�Bq�Bp�Bm�BhsBhsBiyBiyBiyBp�B�B�1B�+B�B�B�B�1B�1B�+B�B|�Bv�Bl�BaHB]/B[#BZBZBZBZBYBZB[#B^5B_;BcTB}�B|�Bv�Bp�Bm�Br�Bx�B}�B~�Bx�Bq�By�B�7B��B��B��B�uB��B�{B��B�uB�{B��B�FB��B��B��B��B��B�B��B��BƨB��B�sB�B�B��B��B��B��B	PB	
=B	1B	JB	�B	"�B	"�B	�B	�B	�B	{B	�B	<jB	N�B	W
B	XB	ZB	[#B	_;B	`BB	`BB	`BB	aHB	dZB	hsB	l�B	u�B	u�B	v�B	y�B	z�B	}�B	~�B	�B	�B	�+B	�1B	�7B	�7B	�=B	�VB	�\B	�\B	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�oB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�3B	�9B	�FB	�XB	�jB	�qB	�wB	��B	��B	��B	�}B	�}B	��B	��B	��B	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�)B	�)B	�/B	�/B	�5B	�;B	�;B	�BB	�HB	�NB	�ZB	�ZB	�`B	�fB	�mB	�fB	�fB	�fB	�`B	�fB	�fB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B
�B
#�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222224222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 BXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBbNBl�Bl�BjBbNBT�BG�BL�BO�BR�BT�BW
BXBZBbNBk�Bu�By�B|�B~�B�B�%B�%B�+B�+B�B�7B�VB�oB�oB�bB�VB�\B�DB�7B�1B�%B�Bw�Bm�BgmBcTB[#BM�BA�B8RB)�B �B�B�B�BoB1B�B�5B��B��B�B�hB}�Bw�Bp�BaHBM�BE�B8RB#�B\B
��B
�fB
��B
��B
��B
ĜB
�}B
�XB
�B
��B
��B
��B
�+B
hsB
^5B
Q�B
9XB
�B
uB	��B	�HB	��B	�jB	�'B	��B	��B	�PB	v�B	dZB	T�B	F�B	5?B	(�B	$�B	!�B	�B	�B	VB	B��B��B�B�mB�TB�5B�B�
B��B��BƨBB�}B�wB�jB�^B�FB�9B�3B�'B�B�B��B��B��B��B��B��B��B�{B�oB�bB�PB�PB�DB�DB�=B�=B�7B�7B�7B�=B]O�BO�BP�BS�BffBx�B{�B{�By�Bt�Bs�Bn�Bl�Br�Bn�Be`BdZB_;BW
BT�BM�BQ�BZBaHBjBq�Br�Bv�Bq�Bp�Bm�BhsBhsBiyBiyBiyBp�B�B�1B�+B�B�B�B�1B�1B�+B�B|�Bv�Bl�BaHB]/B[#BZBZBZBZBYBZB[#B^5B_;BcTB}�B|�Bv�Bp�Bm�Br�Bx�B}�B~�Bx�Bq�By�B�7B��B��B��B�uB��B�{B��B�uB�{B��B�FB��B��B��B��B��B�B��B��BƨB��B�sB�B�B��B��B��B��B	PB	
=B	1B	JB	�B	"�B	"�B	�B	�B	�B	{B	�B	<jB	N�B	W
B	XB	ZB	[#B	_;B	`BB	`BB	`BB	aHB	dZB	hsB	l�B	u�B	u�B	v�B	y�B	z�B	}�B	~�B	�B	�B	�+B	�1B	�7B	�7B	�=B	�VB	�\B	�\B	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�oB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�3B	�9B	�FB	�XB	�jB	�qB	�wB	��B	��B	��B	�}B	�}B	��B	��B	��B	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�)B	�)B	�/B	�/B	�5B	�;B	�;B	�BB	�HB	�NB	�ZB	�ZB	�`B	�fB	�mB	�fB	�fB	�fB	�`B	�fB	�fB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B
�B
#�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222224222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.30 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191715                              AO  ARCAADJP                                                                    20181005191715    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191715  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191715  QCF$                G�O�G�O�G�O�8000            