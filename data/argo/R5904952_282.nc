CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:06:09Z creation      
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
_FillValue                 �  A<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^|   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  fH   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h<   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �T   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �$   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �(   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �,   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20181005190609  20181005190609  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @�����S�1   @���`� V@1�Q���c���R1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     A   A   A   @333@y��@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�33A�  A�  B   B  BffB  B   B(  B0  B8  B?��BH  BP  BX  B`  Bh  Bp  Bx  B�  B�33B�  B�33B�33B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B���B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  C   C�fC  C  C  C	�fC�fC�fC�fC  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL�CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cq�fCt  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C��C�  C�  C�  C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  D   D � D  D�fD  D� D  D� D  D� DfD� D  Dy�D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fDfD� D  D� D  D� D  D� D  D� D  Dy�D��D� DfD� D  D�fD  Dy�D��D y�D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)y�D*  D*�fD+fD+�fD,fD,�fD-  D-� D.  D.� D/  D/�fD0  D0� D1  D1� D2  D2� D2��D3y�D3��D4y�D5  D5� D6  D6� D7  D7� D8  D8y�D9  D9� D9��D:� D;  D;� D<  D<y�D<��D=� D>  D>y�D>��D?� D@fD@�fDA  DA� DA��DB� DCfDC�fDDfDD�fDEfDE� DE��DFy�DF��DG� DHfDH�fDI  DI� DJ  DJ� DJ��DK� DLfDL� DL��DM� DN  DN� DO  DO�fDPfDP�fDQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV�fDW  DWy�DX  DX� DX��DY� DZ  DZ� D[  D[� D[��D\� D]  D]�fD^  D^� D^��D_� D`fD`�fDafDa� Db  Db� Dc  Dc� DdfDd�fDefDe�fDf  Df� Df��Dgy�Dh  Dh�fDi  Di� Dj  Dj� Dj��Dky�Dk��Dl� DmfDm� Dm��Dny�Do  Do� Do��Dp� DqfDq� Dq��Dry�Ds  Ds�fDt  Dt� Du  Du� Dv  Dv� Dw  Dw�fDw�3Dy�\D�*�D�`�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @Fff@�fg@ə�A��A$��AD��Ad��A�ffA�ffA�ffA�ffA�ffAљ�A�ffA�ffB33B	33B��B33B!33B)33B133B933B@��BI33BQ33BY33Ba33Bi33Bq33By33B���B���B���B���B���B���B���B�fgB���B���B���B���B���B���B���B���B���Bę�B�fgB̙�BЙ�B���Bؙ�B�fgB���B䙚B虚B왚B�B���B���B���C L�C33CL�CL�CL�C
33C33C33C33CL�CL�CL�CL�CL�CL�CL�C L�C"L�C$L�C&L�C(L�C*L�C,L�C.L�C0L�C2L�C4L�C6L�C8L�C:L�C<L�C>L�C@L�CBL�CDL�CFL�CHL�CJL�CLfgCNL�CPL�CRL�CTL�CVL�CXL�CZL�C\L�C^L�C`L�CbL�CdL�CfL�ChL�CjL�ClL�CnL�CpL�Cr33CtL�CvL�CxL�CzL�C|L�C~L�C�&fC�&fC�&fC�&fC�33C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�33C�33C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C�&fC�33C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�33C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�33C�33C�33C�33C�&fC�&fC�&fC�33C�&fC�&fC��C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�33C�&fC�&fD 3D �3D3D��D3D�3D3D�3D3D�3D�D�3D3D��D3D�3D3D�3D	3D	�3D
3D
�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D��D�D�3D3D�3D3D�3D3D�3D3D�3D3D��D�D�3D�D�3D3D��D3D��D �D ��D!3D!�3D"3D"�3D#3D#�3D$3D$�3D%3D%�3D&3D&�3D'3D'�3D(3D(�3D)3D)��D*3D*��D+�D+��D,�D,��D-3D-�3D.3D.�3D/3D/��D03D0�3D13D1�3D23D2�3D3�D3��D4�D4��D53D5�3D63D6�3D73D7�3D83D8��D93D9�3D:�D:�3D;3D;�3D<3D<��D=�D=�3D>3D>��D?�D?�3D@�D@��DA3DA�3DB�DB�3DC�DC��DD�DD��DE�DE�3DF�DF��DG�DG�3DH�DH��DI3DI�3DJ3DJ�3DK�DK�3DL�DL�3DM�DM�3DN3DN�3DO3DO��DP�DP��DQ3DQ�3DR3DR�3DS3DS�3DT3DT�3DU3DU�3DV3DV��DW3DW��DX3DX�3DY�DY�3DZ3DZ�3D[3D[�3D\�D\�3D]3D]��D^3D^�3D_�D_�3D`�D`��Da�Da�3Db3Db�3Dc3Dc�3Dd�Dd��De�De��Df3Df�3Dg�Dg��Dh3Dh��Di3Di�3Dj3Dj�3Dk�Dk��Dl�Dl�3Dm�Dm�3Dn�Dn��Do3Do�3Dp�Dp�3Dq�Dq�3Dr�Dr��Ds3Ds��Dt3Dt�3Du3Du�3Dv3Dv�3Dw3Dw��DxfDy��D�4{D�j>1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�Q�A�Q�A�VA�hsA�jA�l�A�jA�jA�l�A�l�A�l�A�n�A�p�A�r�A�r�A�p�A�p�A�r�A�n�A�dZA�5?A���A���Aɟ�A�K�A���A�ƨA�ĜA��A�-A���A�AǑhA�p�A�5?A��Aƛ�A� �A�VA�{A�
=AľwAēuA�=qA��AþwAÉ7A�VA�$�A��A��A���A�z�A��A���A��7A���A�M�A� �A�~�A��A���A�^5A�XA���A�9XA�oA��!A�A��wA�\)A��#A�+A��mA��jA�/A�x�A��wA�ZA��;A��A�$�A���A���A��A���A�1A�$�A�O�A���A�jA�?}A�bNA��
A�XA��A�A�A�E�A���A��A�S�A��A�v�A��+A��A�S�A��hA�A�ZA��uA�l�A��A~�9A{�At��Asp�Aq
=Ao�AnĜAnJAm|�Ai/Af{Ac|�AaC�A\��AV�!AR��AQ��AOG�AM�AK�AI�;AG��AE��AD��A@��A?G�A<^5A;�FA9�A6�\A2�A1�^A0n�A.��A.  A-C�A*��A)
=A(JA'��A'x�A'`BA&��A%&�A$�uA$jA$1A"�+A�A�uAQ�A��At�AC�A�A5?A��A1A�AC�A+A�/Az�A�yA�!A�uAQ�AJA�^A��A�TA��AhsA�Az�A-A��A�!AI�A�A�AJA��A��A��A�-A��AhsAhsA�A
�A	`BA9XA`BAZA�mA�PA�A��A�PA�jA9XAO�AA ��A �@��w@��@�C�@��R@�O�@�ƨ@�^5@�hs@�1'@�p�@�dZ@�~�@�@�\)@�F@��H@��@�j@�(�@��@�l�@�R@���@�G�@�\@�@�Z@�j@�V@�u@��@��@�@�w@�1@�=q@��@��@���@�^@�x�@���@�F@�$�@���@�A�@�1@�E�@߾w@�o@��H@�~�@�5?@ޟ�@�
=@ߕ�@߾w@߮@�33@�^5@ݲ-@�G�@�/@���@���@���@ڇ+@�$�@���@��T@ٲ-@�X@��@���@أ�@�  @׍P@�\)@��@�M�@���@Չ7@���@�Z@�\)@ҏ\@�`B@мj@�A�@υ@��H@��T@͉7@�/@��`@̓u@��;@�l�@�"�@�@��y@ʰ!@�=q@ɲ-@�?}@��/@Ȭ@�r�@ȣ�@Ȭ@ȓu@ț�@ȓu@ȃ@�I�@Ǖ�@�"�@Ƈ+@š�@��`@�t�@î@�o@�^5@�@�7L@��@��`@�j@�b@�b@��F@�S�@�
=@�v�@���@�G�@�%@���@���@���@�j@�Q�@��@��w@�+@��y@�ȴ@��\@�{@�?}@��/@�Ĝ@��@�l�@���@�{@���@�x�@�x�@��@�bN@���@��m@���@��;@���@�;d@��y@���@��R@��\@�^5@�$�@��-@�x�@�G�@��9@�(�@��@��w@�S�@���@���@���@�\)@�"�@�
=@��H@���@���@��H@�ȴ@�J@��@���@�r�@�9X@��@��@�ƨ@�\)@�
=@��@�M�@�J@��@��-@�?}@��/@��@�Q�@�  @��;@�ƨ@�@�n�@�~�@�v�@�5?@��7@��@��`@��u@�bN@�I�@�1@��;@��P@�S�@�33@���@��!@���@��+@�@�G�@���@���@��@�bN@��@�dZ@��@�{@��h@�p�@�O�@�?}@���@��u@�I�@��@�  @�ƨ@��P@�t�@��@��y@�ff@��@���@��@��@��^@���@�`B@��j@�j@�1'@�ƨ@��@��P@�33@��R@�ff@�@���@���@��@y��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�Q�A�Q�A�VA�hsA�jA�l�A�jA�jA�l�A�l�A�l�A�n�A�p�A�r�A�r�A�p�A�p�A�r�A�n�A�dZA�5?A���A���Aɟ�A�K�A���A�ƨA�ĜA��A�-A���A�AǑhA�p�A�5?A��Aƛ�A� �A�VA�{A�
=AľwAēuA�=qA��AþwAÉ7A�VA�$�A��A��A���A�z�A��A���A��7A���A�M�A� �A�~�A��A���A�^5A�XA���A�9XA�oA��!A�A��wA�\)A��#A�+A��mA��jA�/A�x�A��wA�ZA��;A��A�$�A���A���A��A���A�1A�$�A�O�A���A�jA�?}A�bNA��
A�XA��A�A�A�E�A���A��A�S�A��A�v�A��+A��A�S�A��hA�A�ZA��uA�l�A��A~�9A{�At��Asp�Aq
=Ao�AnĜAnJAm|�Ai/Af{Ac|�AaC�A\��AV�!AR��AQ��AOG�AM�AK�AI�;AG��AE��AD��A@��A?G�A<^5A;�FA9�A6�\A2�A1�^A0n�A.��A.  A-C�A*��A)
=A(JA'��A'x�A'`BA&��A%&�A$�uA$jA$1A"�+A�A�uAQ�A��At�AC�A�A5?A��A1A�AC�A+A�/Az�A�yA�!A�uAQ�AJA�^A��A�TA��AhsA�Az�A-A��A�!AI�A�A�AJA��A��A��A�-A��AhsAhsA�A
�A	`BA9XA`BAZA�mA�PA�A��A�PA�jA9XAO�AA ��A �@��w@��@�C�@��R@�O�@�ƨ@�^5@�hs@�1'@�p�@�dZ@�~�@�@�\)@�F@��H@��@�j@�(�@��@�l�@�R@���@�G�@�\@�@�Z@�j@�V@�u@��@��@�@�w@�1@�=q@��@��@���@�^@�x�@���@�F@�$�@���@�A�@�1@�E�@߾w@�o@��H@�~�@�5?@ޟ�@�
=@ߕ�@߾w@߮@�33@�^5@ݲ-@�G�@�/@���@���@���@ڇ+@�$�@���@��T@ٲ-@�X@��@���@أ�@�  @׍P@�\)@��@�M�@���@Չ7@���@�Z@�\)@ҏ\@�`B@мj@�A�@υ@��H@��T@͉7@�/@��`@̓u@��;@�l�@�"�@�@��y@ʰ!@�=q@ɲ-@�?}@��/@Ȭ@�r�@ȣ�@Ȭ@ȓu@ț�@ȓu@ȃ@�I�@Ǖ�@�"�@Ƈ+@š�@��`@�t�@î@�o@�^5@�@�7L@��@��`@�j@�b@�b@��F@�S�@�
=@�v�@���@�G�@�%@���@���@���@�j@�Q�@��@��w@�+@��y@�ȴ@��\@�{@�?}@��/@�Ĝ@��@�l�@���@�{@���@�x�@�x�@��@�bN@���@��m@���@��;@���@�;d@��y@���@��R@��\@�^5@�$�@��-@�x�@�G�@��9@�(�@��@��w@�S�@���@���@���@�\)@�"�@�
=@��H@���@���@��H@�ȴ@�J@��@���@�r�@�9X@��@��@�ƨ@�\)@�
=@��@�M�@�J@��@��-@�?}@��/@��@�Q�@�  @��;@�ƨ@�@�n�@�~�@�v�@�5?@��7@��@��`@��u@�bN@�I�@�1@��;@��P@�S�@�33@���@��!@���@��+@�@�G�@���@���@��@�bN@��@�dZ@��@�{@��h@�p�@�O�@�?}@���@��u@�I�@��@�  @�ƨ@��P@�t�@��@��y@�ff@��@���@��@��@��^@���@�`B@��j@�j@�1'@�ƨ@��@��P@�33@��R@�ff@�@���@���@��@y��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BƨBǮBǮBǮBǮBǮBǮBǮBǮBƨBƨBƨBǮBǮBǮBǮBǮBǮB��B��B�B	�B	o�B	�!B	�HB
�B
�B
(�B
L�B
XB
e`B
p�B
{�B
�B
�PB
��B
�9B
��B
�B1B�B0!BB�BK�BS�B^5BaHBaHBbNB_;BYBgmBu�B�+B�JB�B�3B�jB��BƨB��B�NB+B#�B2-BL�BO�BVBYBO�BO�BK�BH�BG�BL�BL�BG�BG�BD�B@�B?}B>wB9XB2-B0!B,B'�B �B�BoB��B��B�RB��B{�Bk�B>wB%B
�fB
��B
ƨB
��B
�jB
D�B	�B	�HB	�#B	�B	��B	�B	��B	�bB	�7B	jB	D�B	<jB	9XB	=qB	=qB	49B	5?B	7LB	+B	!�B	uB��B�sB�#B�B��B��B��B��BɺB��B��B�;B�;B�B��BÖB�FB�?B�RB�LBĜB�B�B��BɺBǮBƨBƨBŢBÖBÖBB��B�}B�}BƨBǮBȴB��B��B��B��B��B�B�`B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B	B	B	  B��B��B��B��B	B	bB	hB	oB	�B	�B	"�B	!�B	%�B	%�B	'�B	$�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	"�B	$�B	.B	0!B	2-B	49B	7LB	;dB	=qB	=qB	=qB	?}B	>wB	<jB	5?B	6FB	;dB	>wB	A�B	@�B	@�B	?}B	?}B	?}B	A�B	@�B	?}B	@�B	<jB	B�B	I�B	Q�B	W
B	ZB	ZB	YB	XB	YB	aHB	s�B	�B	�B	�B	�B	�B	�B	�B	|�B	w�B	z�B	�+B	� B	�bB	�\B	�bB	�\B	�VB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�-B	�9B	�9B	�?B	�FB	�RB	�XB	�^B	�dB	�dB	�jB	�wB	��B	��B	B	B	B	B	ÖB	ÖB	ĜB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�)B	�;B	�NB	�NB	�NB	�HB	�NB	�ZB	�`B	�`B	�TB	�sB	�B	�B	�B	�yB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
  B
  B
  B
  B	��B	��B	��B	��B
  B
  B
  B	��B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
%B
+B
	7B

=B

�B
�B
#�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 BƨBǮBǮBǮBǮBǮBǮBǮBǮBƨBƨBƨBǮBǮBǮBǮBǮBǮB��B��B�B	�B	o�B	�!B	�HB
�B
�B
(�B
L�B
XB
e`B
p�B
{�B
�B
�PB
��B
�9B
��B
�B1B�B0!BB�BK�BS�B^5BaHBaHBbNB_;BYBgmBu�B�+B�JB�B�3B�jB��BƨB��B�NB+B#�B2-BL�BO�BVBYBO�BO�BK�BH�BG�BL�BL�BG�BG�BD�B@�B?}B>wB9XB2-B0!B,B'�B �B�BoB��B��B�RB��B{�Bk�B>wB%B
�fB
��B
ƨB
��B
�jB
D�B	�B	�HB	�#B	�B	��B	�B	��B	�bB	�7B	jB	D�B	<jB	9XB	=qB	=qB	49B	5?B	7LB	+B	!�B	uB��B�sB�#B�B��B��B��B��BɺB��B��B�;B�;B�B��BÖB�FB�?B�RB�LBĜB�B�B��BɺBǮBƨBƨBŢBÖBÖBB��B�}B�}BƨBǮBȴB��B��B��B��B��B�B�`B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B	B	B	  B��B��B��B��B	B	bB	hB	oB	�B	�B	"�B	!�B	%�B	%�B	'�B	$�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	"�B	$�B	.B	0!B	2-B	49B	7LB	;dB	=qB	=qB	=qB	?}B	>wB	<jB	5?B	6FB	;dB	>wB	A�B	@�B	@�B	?}B	?}B	?}B	A�B	@�B	?}B	@�B	<jB	B�B	I�B	Q�B	W
B	ZB	ZB	YB	XB	YB	aHB	s�B	�B	�B	�B	�B	�B	�B	�B	|�B	w�B	z�B	�+B	� B	�bB	�\B	�bB	�\B	�VB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�-B	�9B	�9B	�?B	�FB	�RB	�XB	�^B	�dB	�dB	�jB	�wB	��B	��B	B	B	B	B	ÖB	ÖB	ĜB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�)B	�;B	�NB	�NB	�NB	�HB	�NB	�ZB	�`B	�`B	�TB	�sB	�B	�B	�B	�yB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
  B
  B
  B
  B	��B	��B	��B	��B
  B
  B
  B	��B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
%B
+B
	7B

=B

�B
�B
#�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.30 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190609                              AO  ARCAADJP                                                                    20181005190609    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190609  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190609  QCF$                G�O�G�O�G�O�8000            