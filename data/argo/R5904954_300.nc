CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:57Z creation      
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
_FillValue                 �  A0   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \P   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^@   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  g�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  wp   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y`   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �,   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �0   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �4   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �8   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �<   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �|   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005191757  20181005191757  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              ,A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��d9��u1   @��d�}:@58�t�j�d�(�\1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     ,A   A   A   @&ff@�  @�  A   A   A>ffA`  A�  A�  A�33A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  C   C  C  C  C�fC
  C�C  C  C  C  C  C  C  C�C�C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD�CF�CH  CJ  CL  CN  CP  CQ�fCT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Cg�fCi�fCl  Cn  Cp  Cr  Ct  Cv�Cx�Cz  C|  C~  C�  C�  C�  C��C�  C�  C�  C��C�  C�  C�  C��3C��3C��C�  C��3C��3C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C��C�  C��3C��3C��3C��3C�  C��C�  C��3C�  C��C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C��C�  C��3C��3C��3C��3C��3C�  C��C��C��C��C�  C�  C��C��C�  C�  C��3C��3C��3C�  C�  C�  C��C�  C�  C�  C��3C��fC��3C�  C��3C�  C�  C�  C��3C�  C��3C��C�  C��3C��3C�  C�  C�  C��3C��C��C��C��C�  C��3C��3C��3D y�D ��Dy�D��D� DfD�fD  D� D  D� D  Dy�D��Dy�D��Dy�D	  D	� D
  D
� D  D� D��Dy�D  D�fDfDy�D  D� D��Dy�DfD��D  Dy�D�3Dy�DfD� D��Dy�D��D� D  D� D��D� D  D�fD  Dy�D  D� D  D� DfD� D  Dy�D  D� D   D �fD!  D!y�D"fD"�fD#  D#� D$fD$� D%  D%� D%��D&�fD'�D'� D'��D(y�D(��D)� D*  D*y�D*��D+� D,  D,y�D,��D-y�D.fD.y�D/fD/y�D0  D0� D0�3D1� D1��D2� D3fD3�fD4fD4��D5  D5�fD6fD6y�D6��D7y�D7��D8� D9fD9�fD:fD:� D;  D;y�D<  D<�fD=fD=��D>fD>�fD?  D?� D?��D@y�DA  DAy�DA��DBy�DC  DC� DDfDD�fDD��DEy�DF  DF� DGfDG�fDG��DH�fDH��DIy�DJ  DJ�fDK  DKy�DL  DLy�DL��DMy�DNfDNy�DN�3DO� DP  DP�fDQ  DQ� DR  DR� DS  DS� DS��DT� DU�DU� DV  DV� DV��DW�fDX  DX�fDYfDY� DZ  DZ�fD[  D[y�D[��D\� D\��D]� D]��D^y�D_fD_y�D`  D`�fDa  Da� Db  Db�fDb��Dc� Dd  Dd� De  De� De��Df� Dg  Dgy�DhfDh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dmy�DnfDn� Do  Do�fDp  Dp� Dp��Dq�fDq��Dr� Ds  Ds� Dt  Dt� DufDu� Du��Dv� Dy�D�%D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@9��@���@ə�A��A$��AC33Ad��A�ffA�ffA���A�ffA�ffA�ffA�ffA�ffB33B	33B33B33B!33B)33B133B933BA33BI33BQ33BY33Ba33Bi33Bq33By33B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���Bę�Bș�B̙�BЙ�Bԙ�Bؙ�Bܙ�B���B䙚B虚B왚B�B�fgB���B���C L�CL�CL�CL�C33C
L�CfgCL�CL�CL�CL�CL�CL�CL�CfgCfgC L�C"L�C$L�C&L�C(L�C*L�C,L�C.L�C0L�C2L�C4L�C6L�C8L�C:L�C<L�C>L�C@L�CBL�CDfgCFfgCHL�CJL�CLL�CNL�CPL�CR33CTL�CVL�CXL�CZL�C\L�C^L�C`L�CbL�CdL�CfL�Ch33Cj33ClL�CnL�CpL�CrL�CtL�CvfgCxfgCzL�C|L�C~L�C�&fC�&fC�&fC�33C�&fC�&fC�&fC�33C�&fC�&fC�&fC��C��C�33C�&fC��C��C�&fC�&fC�33C�&fC�&fC�&fC�&fC�&fC�33C�&fC�&fC�&fC�&fC�&fC�33C�33C�33C�33C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C��C�&fC�&fC�&fC�&fC�33C�&fC��C��C��C��C�&fC�33C�&fC��C�&fC�33C�&fC�&fC�&fC��C��C��C�&fC�&fC�&fC�&fC�33C�&fC��C��C��C��C��C�&fC�33C�33C�33C�33C�&fC�&fC�33C�33C�&fC�&fC��C��C��C�&fC�&fC�&fC�33C�&fC�&fC�&fC��C��C��C�&fC��C�&fC�&fC�&fC��C�&fC��C�33C�&fC��C��C�&fC�&fC�&fC��C�33C�33C�33C�33C�&fC��C��D �D ��D�D��D�D�3D�D��D3D�3D3D�3D3D��D�D��D�D��D	3D	�3D
3D
�3D3D�3D�D��D3D��D�D��D3D�3D�D��D�D� D3D��DfD��D�D�3D�D��D�D�3D3D�3D�D�3D3D��D3D��D3D�3D3D�3D�D�3D3D��D3D�3D 3D ��D!3D!��D"�D"��D#3D#�3D$�D$�3D%3D%�3D&�D&��D'  D'�3D(�D(��D)�D)�3D*3D*��D+�D+�3D,3D,��D-�D-��D.�D.��D/�D/��D03D0�3D1fD1�3D2�D2�3D3�D3��D4�D4� D53D5��D6�D6��D7�D7��D8�D8�3D9�D9��D:�D:�3D;3D;��D<3D<��D=�D=� D>�D>��D?3D?�3D@�D@��DA3DA��DB�DB��DC3DC�3DD�DD��DE�DE��DF3DF�3DG�DG��DH�DH��DI�DI��DJ3DJ��DK3DK��DL3DL��DM�DM��DN�DN��DOfDO�3DP3DP��DQ3DQ�3DR3DR�3DS3DS�3DT�DT�3DU  DU�3DV3DV�3DW�DW��DX3DX��DY�DY�3DZ3DZ��D[3D[��D\�D\�3D]�D]�3D^�D^��D_�D_��D`3D`��Da3Da�3Db3Db��Dc�Dc�3Dd3Dd�3De3De�3Df�Df�3Dg3Dg��Dh�Dh�3Di3Di�3Dj3Dj�3Dk3Dk�3Dl3Dl�3Dm3Dm��Dn�Dn�3Do3Do��Dp3Dp�3Dq�Dq��Dr�Dr�3Ds3Ds�3Dt3Dt�3Du�Du�3Dv�Dv�3Dy,)D�.�D�ٚ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AƧ�AƧ�AƧ�AƉ7A�ZA��;A�%A���A�`BA�$�A�ȴA��A�\)A�;dA��A��#A��A��+A�bNA�/A���A���A�K�A��A�"�A��DA�33A���A�bNA��A��yA���A�C�A��A�jA�A�A�?}A�;dA�/A��;A���A��+A�S�A�{A�A���A��A��mA��;A���A���A�|�A�5?A�oA���A���A�VA��A�;dA��
A��!A��A�G�A�+A��A���A��FA��\A�~�A�ffA�\)A�XA�Q�A�C�A�{A��A��;A���A��9A���A�1'A��#A���A���A�p�A�E�A�&�A��A���A��HA��uA��/A���A�r�A���A���A�O�A���A��HA�Q�A�  A���A�bNA�(�A���A��DA��A���A�{A���A��9A�p�A��A�x�A�A�l�A��FA�"�A��jA��FA��!A���A�=qA�;dA�=qA��A��`A�(�A��
A���A~�A|~�A{Ay�Axn�Awt�AvĜAv��AvZAu��Au��Au&�At{As�^AsO�ArM�Ap�Ao�#Am��Ak�FAj=qAiK�Ah�Af�`Ac�Aa�7A_oA]�A[%AW+AUl�AS&�AP�!AM�wAJ��AI�TAI��AIdZAH��AG�AF�yAE�;ADffAC��AC�hAA�
A@{A>��A<��A;��A;�A;%A:A�A7ƨA61A4��A2�!A1hsA0��A/�PA-��A-%A+��A*bA)�7A)�A(Q�A'�A&�+A%;dA#�A!�
A�A�\A�AȴAbNAA��A/A?}Ap�A��A/AA�A�AQ�A�FAG�A�yA�\A
�RA
(�A	�mA	�wA	��A	�A	K�A�`A��A$�A;dA�A��AX@�J@��@���@�"�@�hs@���@��@��@�V@�@��#@��@�j@웦@�r�@�9X@�1@�ƨ@��@�J@畁@�/@�@ݑh@�S�@ى7@���@�bN@�o@�{@���@�33@�o@��m@��y@ʇ+@��#@�Ĝ@Ǿw@�|�@�33@�
=@��H@�~�@�{@�@�$�@Ƈ+@���@Ƨ�@ũ�@���@�  @�@��@���@�&�@��@��@�t�@��!@��@���@�p�@��u@�
=@��T@�I�@�v�@���@�%@�Ĝ@��@�(�@��F@���@�K�@�+@�V@�G�@�Ĝ@�%@���@�r�@�1@��F@��F@��P@��P@��@��@��7@���@���@���@���@�1'@��@���@�+@���@���@��@�`B@���@���@�z�@�1'@���@�+@�M�@��#@�@���@�%@��9@�A�@���@���@�n�@�=q@���@���@���@���@�p�@�?}@�X@�p�@�p�@�X@�?}@���@��u@��D@�j@�1@��F@���@�|�@�
=@��R@���@��\@��+@�v�@�V@�$�@���@�@���@��@��T@���@�@�?}@��@� �@��m@��F@��@�;d@�S�@��P@�C�@�M�@���@���@���@���@�bN@�Q�@�A�@�b@�  @� �@��m@�33@��@��H@���@���@�v�@�=q@�5?@��T@�O�@�V@��`@��@�bN@���@��@�ƨ@��F@���@�S�@�K�@�;d@��y@���@���@�=q@�@���@���@�?}@��@���@���@��@�1@��@��;@��F@���@�dZ@��@���@��@��y@���@�ȴ@���@��!@�~�@�=q@��@��h@��7@�O�@��@�Ĝ@��j@�z�@�Q�@��@�  @�ƨ@���@�t�@�K�@�"�@�
=@���@��@���@�^5@�5?@��@�{@��@���@��@�p�@�&�@��@�r�@��@}rG@j��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AƧ�AƧ�AƧ�AƉ7A�ZA��;A�%A���A�`BA�$�A�ȴA��A�\)A�;dA��A��#A��A��+A�bNA�/A���A���A�K�A��A�"�A��DA�33A���A�bNA��A��yA���A�C�A��A�jA�A�A�?}A�;dA�/A��;A���A��+A�S�A�{A�A���A��A��mA��;A���A���A�|�A�5?A�oA���A���A�VA��A�;dA��
A��!A��A�G�A�+A��A���A��FA��\A�~�A�ffA�\)A�XA�Q�A�C�A�{A��A��;A���A��9A���A�1'A��#A���A���A�p�A�E�A�&�A��A���A��HA��uA��/A���A�r�A���A���A�O�A���A��HA�Q�A�  A���A�bNA�(�A���A��DA��A���A�{A���A��9A�p�A��A�x�A�A�l�A��FA�"�A��jA��FA��!A���A�=qA�;dA�=qA��A��`A�(�A��
A���A~�A|~�A{Ay�Axn�Awt�AvĜAv��AvZAu��Au��Au&�At{As�^AsO�ArM�Ap�Ao�#Am��Ak�FAj=qAiK�Ah�Af�`Ac�Aa�7A_oA]�A[%AW+AUl�AS&�AP�!AM�wAJ��AI�TAI��AIdZAH��AG�AF�yAE�;ADffAC��AC�hAA�
A@{A>��A<��A;��A;�A;%A:A�A7ƨA61A4��A2�!A1hsA0��A/�PA-��A-%A+��A*bA)�7A)�A(Q�A'�A&�+A%;dA#�A!�
A�A�\A�AȴAbNAA��A/A?}Ap�A��A/AA�A�AQ�A�FAG�A�yA�\A
�RA
(�A	�mA	�wA	��A	�A	K�A�`A��A$�A;dA�A��AX@�J@��@���@�"�@�hs@���@��@��@�V@�@��#@��@�j@웦@�r�@�9X@�1@�ƨ@��@�J@畁@�/@�@ݑh@�S�@ى7@���@�bN@�o@�{@���@�33@�o@��m@��y@ʇ+@��#@�Ĝ@Ǿw@�|�@�33@�
=@��H@�~�@�{@�@�$�@Ƈ+@���@Ƨ�@ũ�@���@�  @�@��@���@�&�@��@��@�t�@��!@��@���@�p�@��u@�
=@��T@�I�@�v�@���@�%@�Ĝ@��@�(�@��F@���@�K�@�+@�V@�G�@�Ĝ@�%@���@�r�@�1@��F@��F@��P@��P@��@��@��7@���@���@���@���@�1'@��@���@�+@���@���@��@�`B@���@���@�z�@�1'@���@�+@�M�@��#@�@���@�%@��9@�A�@���@���@�n�@�=q@���@���@���@���@�p�@�?}@�X@�p�@�p�@�X@�?}@���@��u@��D@�j@�1@��F@���@�|�@�
=@��R@���@��\@��+@�v�@�V@�$�@���@�@���@��@��T@���@�@�?}@��@� �@��m@��F@��@�;d@�S�@��P@�C�@�M�@���@���@���@���@�bN@�Q�@�A�@�b@�  @� �@��m@�33@��@��H@���@���@�v�@�=q@�5?@��T@�O�@�V@��`@��@�bN@���@��@�ƨ@��F@���@�S�@�K�@�;d@��y@���@���@�=q@�@���@���@�?}@��@���@���@��@�1@��@��;@��F@���@�dZ@��@���@��@��y@���@�ȴ@���@��!@�~�@�=q@��@��h@��7@�O�@��@�Ĝ@��j@�z�@�Q�@��@�  @�ƨ@���@�t�@�K�@�"�@�
=@���@��@���@�^5@�5?@��@�{@��@���@��@�p�@�&�@��@�r�@��@}rG@j��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B��B�B\)Bq�Bt�By�B�B�DB�PB�bB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�!B�?B�^B��BÖBÖBÖBÖBŢBƨBǮBȴBɺBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�#B�#B�)B�/B�5B�5B�;B�HB�BB�BB�BB�HB�HB�HB�NB�ZB�ZB�ZB�ZB�TB�ZB�yB	7B�B�B$�B,B,B,B(�B#�B�B�B�BbB
=BB�B�B�mB�;B��B�}B��B�B^5B6FB �B�BuBJB  B�B�NB�B�wB�hBw�Bk�BJ�B�BB
�sB
��B
��B
��B
��B
�}B
�3B
��B
��B
�7B
x�B
q�B
jB
`BB
ZB
T�B
R�B
Q�B
O�B
O�B
M�B
F�B
A�B
=qB
7LB
-B
$�B
�B
+B	��B	�B	�B	�BB	ƨB	�LB	��B	�{B	�B	k�B	[#B	I�B	8RB	&�B	�B	oB	bB	\B	JB	%B	B��B�B�B�B�`B�5B�B��B��B��B��BȴB�wB�^B�RB�B��B��B��B��B��B�hB�VB�JB�DB�7B�%B�B�B}�Bz�Bz�B{�Bx�Bv�Bv�Bx�Br�BjBiyBjBgmBe`B`BBYBVBT�BS�BS�BS�BZBYBZB[#B\)B\)B\)B[#BZBZBYBYBW
BR�BL�BG�BE�BG�BK�BO�BR�BS�BXB`BBffBjBk�Bk�Bk�Bl�Bm�Bn�Bo�Bp�Bp�Bn�BiyBbNBbNBffBjBn�Br�Bu�Bw�Bz�B}�B�B�B�%B�1B�JB�oB�uB�{B�{B�{B�{B��B��B��B��B��B��B��B�B�B�3B�?B�^B�wB��BǮBȴB��B��B��B��B��B��B��B��B��B��B��B��B�
B�B�B�B�B�/B�;B�ZB�B��B	  B	B	+B	DB	VB	bB	oB	uB	uB	�B	�B	�B	�B	 �B	"�B	)�B	-B	/B	1'B	49B	7LB	8RB	<jB	?}B	@�B	C�B	G�B	G�B	H�B	K�B	L�B	M�B	Q�B	R�B	R�B	R�B	VB	YB	[#B	]/B	^5B	`BB	aHB	bNB	e`B	jB	k�B	l�B	l�B	p�B	u�B	w�B	{�B	� B	�B	�B	�B	�B	�+B	�7B	�7B	�7B	�7B	�7B	�DB	�VB	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�!B	�-B	�-B	�-B	�9B	�?B	�LB	�XB	�dB	�qB	�}B	��B	ĜB	ŢB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�B	�B	�#B	�)B	�#B	�/B	�BB	�ZB	�fB	�fB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
�B
�B
)y2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222B�B�B�B�B��B�B\)Bq�Bt�By�B�B�DB�PB�bB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�!B�?B�^B��BÖBÖBÖBÖBŢBƨBǮBȴBɺBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�#B�#B�)B�/B�5B�5B�;B�HB�BB�BB�BB�HB�HB�HB�NB�ZB�ZB�ZB�ZB�TB�ZB�yB	7B�B�B$�B,B,B,B(�B#�B�B�B�BbB
=BB�B�B�mB�;B��B�}B��B�B^5B6FB �B�BuBJB  B�B�NB�B�wB�hBw�Bk�BJ�B�BB
�sB
��B
��B
��B
��B
�}B
�3B
��B
��B
�7B
x�B
q�B
jB
`BB
ZB
T�B
R�B
Q�B
O�B
O�B
M�B
F�B
A�B
=qB
7LB
-B
$�B
�B
+B	��B	�B	�B	�BB	ƨB	�LB	��B	�{B	�B	k�B	[#B	I�B	8RB	&�B	�B	oB	bB	\B	JB	%B	B��B�B�B�B�`B�5B�B��B��B��B��BȴB�wB�^B�RB�B��B��B��B��B��B�hB�VB�JB�DB�7B�%B�B�B}�Bz�Bz�B{�Bx�Bv�Bv�Bx�Br�BjBiyBjBgmBe`B`BBYBVBT�BS�BS�BS�BZBYBZB[#B\)B\)B\)B[#BZBZBYBYBW
BR�BL�BG�BE�BG�BK�BO�BR�BS�BXB`BBffBjBk�Bk�Bk�Bl�Bm�Bn�Bo�Bp�Bp�Bn�BiyBbNBbNBffBjBn�Br�Bu�Bw�Bz�B}�B�B�B�%B�1B�JB�oB�uB�{B�{B�{B�{B��B��B��B��B��B��B��B�B�B�3B�?B�^B�wB��BǮBȴB��B��B��B��B��B��B��B��B��B��B��B��B�
B�B�B�B�B�/B�;B�ZB�B��B	  B	B	+B	DB	VB	bB	oB	uB	uB	�B	�B	�B	�B	 �B	"�B	)�B	-B	/B	1'B	49B	7LB	8RB	<jB	?}B	@�B	C�B	G�B	G�B	H�B	K�B	L�B	M�B	Q�B	R�B	R�B	R�B	VB	YB	[#B	]/B	^5B	`BB	aHB	bNB	e`B	jB	k�B	l�B	l�B	p�B	u�B	w�B	{�B	� B	�B	�B	�B	�B	�+B	�7B	�7B	�7B	�7B	�7B	�DB	�VB	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�!B	�-B	�-B	�-B	�9B	�?B	�LB	�XB	�dB	�qB	�}B	��B	ĜB	ŢB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�B	�B	�#B	�)B	�#B	�/B	�BB	�ZB	�fB	�fB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
�B
�B
)y2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.30 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191757                              AO  ARCAADJP                                                                    20181005191757    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191757  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191757  QCF$                G�O�G�O�G�O�8000            