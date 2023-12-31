CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:27Z creation      
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
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181005191727  20181005191727  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @�����1   @���)�,@5�^5?}�d}�$�/1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   B   @,��@�  @�  A   A   A@  A`  A�  A�  A�  A���A���A���A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  C   C�C  C  C  C
  C  C�C�C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C+�fC.  C/�fC2  C4  C6  C8  C9�fC;�fC=�fC@  CB  CD  CF  CH�CJ  CK�fCN  CP�CR  CT  CV�CX  CZ  C\�C^  C`  Ca�fCc�fCe�fCh�Cj�Cl  Cm�fCo�fCq�fCt  Cv  Cw�fCz  C|  C}�fC�  C�  C��3C�  C�  C��C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��C��C�  C��C��3C�  C�  C��C��C��C�  C�  C��3C��3C�  C�  C�  C�  C��C��C��C��3C�  C��C�  C�  C�  C��C��C�  C�  C��C�  C��C�  C�  C��C�  C��3C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C��C��C��C��3C��3C�  C��3C��3C�  C�  C�  C�  C�  C��C�  C��3C�  C��C��C��C�  C�  C�  C�  C��C��C��C��3C�  C�  C��3C�  C�  C��3C��3C�  C�  C�  C�  C�  C��3C�  C��C��C�  C��3C�  C�  C��C��C��C��C�  C�  C��3C�  C�  C��3C��3D y�D ��D� D  Dy�D  Dy�D��D� D  D� D  Dy�D�3Dy�D  D� D	  D	� D
fD
� D  D� D��Dy�D��D� D  Dy�D  D� D  D� D  D� D  D� D  D� D��Dy�D  D� D  Dy�D  D�fD  D� D  D� D��D� D  D�fDfD� D  Dy�D��Dy�D��D� D fD �fD!  D!� D"fD"� D#  D#�fD/� D0  D0y�D0��D1y�D1��D2y�D3  D3�fD4  D4� D5  D5� D6  D6� D7  D7� D7��D8y�D9  D9� D:  D:�fD;  D;y�D;��D<y�D=  D=�fD>  D>� D?  D?y�D?��D@y�DA  DA� DA��DBy�DC  DC� DD  DD� DE  DE� DF  DF� DGfDG� DH  DHy�DI  DI� DJ  DJ� DK  DKy�DL  DL� DM  DM�fDNfDN� DO  DO�fDP�DP�fDQ  DQ� DRfDR� DS  DS� DT  DTy�DT��DU�fDV  DV�fDW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[�fD\  D\�fD]fD]y�D^  D^� D^��D_y�D_��D`� Da  Da�fDbfDb� DcfDc�fDd  Ddy�De  De� Df  Df�fDg  Dg� Dh  Dhy�Dh��Diy�Di��Dj� Dk  Dky�Dl  Dl� Dl��Dm�fDnfDn� Do  Do� Dp  Dp� DqfDq� Dq��Dr� Ds  Dsy�DtfDt� Dt��Du� DvfDv� Dw  Dw�fDw� Dy��D�;31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @;�@�\)@�\)A�A#�AC�Ac�A��
A��
A��
A���A£�Aң�A��
A��
B �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B���B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�B�B�u�B�u�B�u�B�u�C :�CT{C:�C:�C:�C
:�C:�CT{CT{C:�C:�C:�C:�C:�C:�C:�C :�C":�C$:�C&:�C(:�C*:�C,!GC.:�C0!GC2:�C4:�C6:�C8:�C:!GC<!GC>!GC@:�CB:�CD:�CF:�CHT{CJ:�CL!GCN:�CPT{CR:�CT:�CVT{CX:�CZ:�C\T{C^:�C`:�Cb!GCd!GCf!GChT{CjT{Cl:�Cn!GCp!GCr!GCt:�Cv:�Cx!GCz:�C|:�C~!GC�qC�qC��C�qC�qC�*>C�qC�*>C�*>C�7C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC��C�*>C�*>C�qC�*>C��C�qC�qC�*>C�*>C�*>C�qC�qC��C��C�qC�qC�qC�qC�*>C�*>C�*>C��C�qC�*>C�qC�qC�qC�*>C�*>C�qC�qC�*>C�qC�*>C�qC�qC�*>C�qC��C�qC�qC�*>C�qC��C�qC�qC�qC�qC�qC�*>C�*>C�*>C��C��C�qC��C��C�qC�qC�qC�qC�qC�*>C�qC��C�qC�*>C�*>C�*>C�qC�qC�qC�qC�*>C�*>C�*>C��C�qC�qC��C�qC�qC��C��C�qC�qC�qC�qC�qC��C�qC�*>C�*>C�qC��C�qC�qC�*>C�*>C�*>C�*>C�qC�qC��C�qC�qC��D RD �RDRD��D�D�RD�D�RDRD��D�D��D�D�RD�D�RD�D��D	�D	��D
D
��D�D��DRD�RDRD��D�D�RD�D��D�D��D�D��D�D��D�D��DRD�RD�D��D�D�RD�D�D�D��D�D��DRD��D�D�DD��D�D�RDRD�RDRD��D D �D!�D!��D"D"��D#�D#�D/��D0�D0�RD1RD1�RD2RD2�RD3�D3�D4�D4��D5�D5��D6�D6��D7�D7��D8RD8�RD9�D9��D:�D:�D;�D;�RD<RD<�RD=�D=�D>�D>��D?�D?�RD@RD@�RDA�DA��DBRDB�RDC�DC��DD�DD��DE�DE��DF�DF��DGDG��DH�DH�RDI�DI��DJ�DJ��DK�DK�RDL�DL��DM�DM�DNDN��DO�DO�DP�DP�DQ�DQ��DRDR��DS�DS��DT�DT�RDURDU�DV�DV�DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[�D\�D\�D]D]�RD^�D^��D_RD_�RD`RD`��Da�Da�DbDb��DcDc�Dd�Dd�RDe�De��Df�Df�Dg�Dg��Dh�Dh�RDiRDi�RDjRDj��Dk�Dk�RDl�Dl��DmRDm�DnDn��Do�Do��Dp�Dp��DqDq��DrRDr��Ds�Ds�RDtDt��DuRDu��DvDv��Dw�Dw�Dw�Dy�\D�B�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�"�A�&�A�(�A�&�A�(�A�(�A�{A��A�{A��A�+A�-A�$�A� �A��A�"�A�$�A�"�A��A�/A�=qA�?}A�A�A�A�A�?}A�9XA�;dA�A�A�G�A�E�A�E�A�=qA�E�A�M�A�XAؑhA�;dA�ĜA؏\A�1'A��A׾wA�z�AԲ-Aӕ�A�G�A�jAЬA�9XA�|�A� �A�$�A���Aȣ�A��TAƅA���A�z�A��mA�{A���A��-A�oA��A��HA�M�A��7A��!A�9XA��9A���A�A�t�A�{A���A��TA���A�z�A�1'A��RA��mA�ȴA��A��jA�^5A��FA�+A��A���A�^5A���A�l�A���A��+A�ƨA�oA��A�|�A��mA�A��A�Q�A���A�?}A��RA�^5A��9A�A��A�7LA�JA�A���A�=qA��`A��RA�A��A~-A{�Aw7LAt��AtVAt(�AsAr�ArQ�Aq"�An�HAm/AlE�Aj��AeS�AcS�AaA]��A[`BA[AZffAYG�AW�-AU�AS�wAQ�-AN~�ALAKXAK
=AJv�AI�PAGAG?}AG&�AG�AF�RAD�yAB��A@�!A?"�A>��A=�
A=�A<��A;�A:��A9hsA6bA3��A1A/��A.bNA-oA+�#A++A*�DA)+A(ȴA'`BA&��A$�HA#�-A"��A"bA ^5AXA�jAbNAJA�RAt�A|�A�7A$�A��A��AoA-Ap�A1A9XAS�A
v�A	;dA~�AXAz�AA  AI�AA�A"�A&�A~�A�AS�@���A bNA ff@��T@��u@�Q�@��m@� �@�bN@��D@��P@��R@�O�@��@�"�@��\@�F@���@���@�v�@�7@���@��@��@�b@��@�ff@�{@�x�@�  @�;d@ܴ9@�n�@ف@�`B@ف@�@���@��m@�p�@�t�@ҧ�@Ѳ-@��@Ͼw@�~�@�E�@͉7@��/@�Q�@ˍP@ʰ!@�{@�hs@ȋD@��
@�t�@�@�n�@�@ź^@�M�@�G�@�G�@��m@�~�@��@��u@�Z@��@�A�@�|�@�v�@���@�l�@�^5@�j@��w@�l�@�33@��!@�G�@�z�@��w@�;d@�M�@���@���@��#@�hs@�%@��
@��y@�v�@�@�5?@��@�@���@��@�V@��@���@�hs@���@���@�ȴ@��+@�^5@���@�7L@���@� �@��y@��@��@���@���@��@�n�@���@��/@��@�1@��@���@���@���@�l�@�S�@�;d@��@�ȴ@��R@��+@�V@���@��#@��^@�`B@�V@��9@��@�z�@��@���@�z�@�bN@�bN@�I�@��w@�33@���@�J@��7@���@�1'@� �@�&�@�7L@��/@��@���@���@��@��7@��h@��@��@���@�j@�9X@�1@��;@���@�|�@��@�|�@��@���@��P@��@�  @� �@�b@�1@��m@��P@�o@���@���@���@�ff@�5?@�@�p�@���@��j@��D@�z�@�9X@�I�@�(�@� �@��m@��F@�\)@�@���@�~�@�{@���@��R@�33@�dZ@�\)@�S�@��@�~�@��@���@��@��T@���@���@���@�?}@��-@��@��T@��#@���@�x�@�X@��@�Ĝ@�j@�(�@��@���@�ƨ@�l�@��y@��!@��!@�v�@�@��#@���@���@�hs@��@��9@��9@��@��@�I�@�  @{A�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�"�A�&�A�(�A�&�A�(�A�(�A�{A��A�{A��A�+A�-A�$�A� �A��A�"�A�$�A�"�A��A�/A�=qA�?}A�A�A�A�A�?}A�9XA�;dA�A�A�G�A�E�A�E�A�=qA�E�A�M�A�XAؑhA�;dA�ĜA؏\A�1'A��A׾wA�z�AԲ-Aӕ�A�G�A�jAЬA�9XA�|�A� �A�$�A���Aȣ�A��TAƅA���A�z�A��mA�{A���A��-A�oA��A��HA�M�A��7A��!A�9XA��9A���A�A�t�A�{A���A��TA���A�z�A�1'A��RA��mA�ȴA��A��jA�^5A��FA�+A��A���A�^5A���A�l�A���A��+A�ƨA�oA��A�|�A��mA�A��A�Q�A���A�?}A��RA�^5A��9A�A��A�7LA�JA�A���A�=qA��`A��RA�A��A~-A{�Aw7LAt��AtVAt(�AsAr�ArQ�Aq"�An�HAm/AlE�Aj��AeS�AcS�AaA]��A[`BA[AZffAYG�AW�-AU�AS�wAQ�-AN~�ALAKXAK
=AJv�AI�PAGAG?}AG&�AG�AF�RAD�yAB��A@�!A?"�A>��A=�
A=�A<��A;�A:��A9hsA6bA3��A1A/��A.bNA-oA+�#A++A*�DA)+A(ȴA'`BA&��A$�HA#�-A"��A"bA ^5AXA�jAbNAJA�RAt�A|�A�7A$�A��A��AoA-Ap�A1A9XAS�A
v�A	;dA~�AXAz�AA  AI�AA�A"�A&�A~�A�AS�@���A bNA ff@��T@��u@�Q�@��m@� �@�bN@��D@��P@��R@�O�@��@�"�@��\@�F@���@���@�v�@�7@���@��@��@�b@��@�ff@�{@�x�@�  @�;d@ܴ9@�n�@ف@�`B@ف@�@���@��m@�p�@�t�@ҧ�@Ѳ-@��@Ͼw@�~�@�E�@͉7@��/@�Q�@ˍP@ʰ!@�{@�hs@ȋD@��
@�t�@�@�n�@�@ź^@�M�@�G�@�G�@��m@�~�@��@��u@�Z@��@�A�@�|�@�v�@���@�l�@�^5@�j@��w@�l�@�33@��!@�G�@�z�@��w@�;d@�M�@���@���@��#@�hs@�%@��
@��y@�v�@�@�5?@��@�@���@��@�V@��@���@�hs@���@���@�ȴ@��+@�^5@���@�7L@���@� �@��y@��@��@���@���@��@�n�@���@��/@��@�1@��@���@���@���@�l�@�S�@�;d@��@�ȴ@��R@��+@�V@���@��#@��^@�`B@�V@��9@��@�z�@��@���@�z�@�bN@�bN@�I�@��w@�33@���@�J@��7@���@�1'@� �@�&�@�7L@��/@��@���@���@��@��7@��h@��@��@���@�j@�9X@�1@��;@���@�|�@��@�|�@��@���@��P@��@�  @� �@�b@�1@��m@��P@�o@���@���@���@�ff@�5?@�@�p�@���@��j@��D@�z�@�9X@�I�@�(�@� �@��m@��F@�\)@�@���@�~�@�{@���@��R@�33@�dZ@�\)@�S�@��@�~�@��@���@��@��T@���@���@���@�?}@��-@��@��T@��#@���@�x�@�X@��@�Ĝ@�j@�(�@��@���@�ƨ@�l�@��y@��!@��!@�v�@�@��#@���@���@�hs@��@��9@��9@��@��@�I�@�  @{A�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
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
��B
��B
��B
��B
��B
��B
B
��B
ÖB
ĜB
ĜB
ĜB
ÖB
��B
B
ÖB
ĜB
ĜB
ĜB
B
ÖB
ƨB
��B
��B��BaHBffBl�Bs�Bk�BR�BZBjBw�B{�B}�B� B� B�B�1B�+B��B��B�B�B�!B�'B��B��B�
B�
B�B�B�fB�ZB�)B��B��B�;B�HB�BB�5B�B��B�^B�!B��B��B�hBt�B_;BN�BH�BB�B;dB7LB33B/B&�B�BbBPB	7BB��B��B�B�;B�B��B��B�RB��Bs�B[#BN�B<jB/B$�B�BPB
��B
��B
��B
��B
�+B
v�B
bNB
G�B
9XB
8RB
7LB
5?B
1'B
,B
%�B
�B
JB
B	�B	��B	�B	��B	�B	u�B	s�B	o�B	hsB	_;B	T�B	H�B	<jB	/B	$�B	!�B	�B	�B	�B	oB	hB	bB	\B	DB	  B�B�yB�NB�HB�;B�BB�sB�B�mB�ZB�BŢB�RB�-B�!B�B��B��B��B��B��B��B��B��B��B��B�uB�\B�DB�=B�1B�%B�B{�Bv�Bq�Bm�BjBffBdZBbNB^5BZBT�BR�BO�BM�BL�BL�BK�BM�BP�BaHBiyBl�Bs�Bw�Bv�Bs�Bt�Bl�Bw�B�Bz�Bw�Bw�By�B|�B� B�B�+B�+B�B�B�B� B}�Bx�Bt�Bo�BhsBe`BdZBcTBaHB_;B_;B^5B]/B]/B^5B_;BZB\)B\)B]/BcTBgmBe`BbNB^5B[#BYBXBXBYBYBYB]/B^5B_;B`BBaHBcTBe`BgmBhsBhsBiyBiyBk�Bu�Bx�B}�B~�B|�Bz�Bx�Bx�B{�B� B�B~�B{�Bx�B~�B�B�B�B�B�B� B� B� B~�B~�B|�By�B}�B�B�B�%B�+B�7B�PB�oB��B��B��B��B��B��B��B�'B�?B�FB�XB�RB�RB�wA�S�B	E�B	E�B	E�B	I�B	L�B	O�B	R�B	T�B	T�B	VB	W
B	W
B	[#B	`BB	e`B	hsB	l�B	o�B	p�B	p�B	s�B	t�B	u�B	w�B	x�B	z�B	z�B	z�B	z�B	�B	�B	�B	�B	�B	�B	�B	�%B	�+B	�1B	�1B	�+B	�+B	�%B	�+B	�1B	�=B	�PB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�3B	�9B	�?B	�LB	�RB	�^B	�qB	�}B	��B	��B	��B	��B	�}B	�}B	B	ĜB	ÖB	ÖB	ĜB	ĜB	ĜB	ĜB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�/B	�5B	�;B	�HB	�BB	�5B	�)B	�)B	�)B	�)B	�#B	�B	�#B	�5B	�NB	�`B	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
&2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222242222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B
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
��B
��B
��B
��B
��B
��B
B
��B
ÖB
ĜB
ĜB
ĜB
ÖB
��B
B
ÖB
ĜB
ĜB
ĜB
B
ÖB
ƨB
��B
��B��BaHBffBl�Bs�Bk�BR�BZBjBw�B{�B}�B� B� B�B�1B�+B��B��B�B�B�!B�'B��B��B�
B�
B�B�B�fB�ZB�)B��B��B�;B�HB�BB�5B�B��B�^B�!B��B��B�hBt�B_;BN�BH�BB�B;dB7LB33B/B&�B�BbBPB	7BB��B��B�B�;B�B��B��B�RB��Bs�B[#BN�B<jB/B$�B�BPB
��B
��B
��B
��B
�+B
v�B
bNB
G�B
9XB
8RB
7LB
5?B
1'B
,B
%�B
�B
JB
B	�B	��B	�B	��B	�B	u�B	s�B	o�B	hsB	_;B	T�B	H�B	<jB	/B	$�B	!�B	�B	�B	�B	oB	hB	bB	\B	DB	  B�B�yB�NB�HB�;B�BB�sB�B�mB�ZB�BŢB�RB�-B�!B�B��B��B��B��B��B��B��B��B��B��B�uB�\B�DB�=B�1B�%B�B{�Bv�Bq�Bm�BjBffBdZBbNB^5BZBT�BR�BO�BM�BL�BL�BK�BM�BP�BaHBiyBl�Bs�Bw�Bv�Bs�Bt�Bl�Bw�B�Bz�Bw�Bw�By�B|�B� B�B�+B�+B�B�B�B� B}�Bx�Bt�Bo�BhsBe`BdZBcTBaHB_;B_;B^5B]/B]/B^5B_;BZB\)B\)B]/BcTBgmBe`BbNB^5B[#BYBXBXBYBYBYB]/B^5B_;B`BBaHBcTBe`BgmBhsBhsBiyBiyBk�Bu�Bx�B}�B~�B|�Bz�Bx�Bx�B{�B� B�B~�B{�Bx�B~�B�B�B�B�B�B� B� B� B~�B~�B|�By�B}�B�B�B�%B�+B�7B�PB�oB��B��B��B��B��B��B��B�'B�?B�FB�XB�RB�RB�wA�S�B	E�B	E�B	E�B	I�B	L�B	O�B	R�B	T�B	T�B	VB	W
B	W
B	[#B	`BB	e`B	hsB	l�B	o�B	p�B	p�B	s�B	t�B	u�B	w�B	x�B	z�B	z�B	z�B	z�B	�B	�B	�B	�B	�B	�B	�B	�%B	�+B	�1B	�1B	�+B	�+B	�%B	�+B	�1B	�=B	�PB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�3B	�9B	�?B	�LB	�RB	�^B	�qB	�}B	��B	��B	��B	��B	�}B	�}B	B	ĜB	ÖB	ÖB	ĜB	ĜB	ĜB	ĜB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�/B	�5B	�;B	�HB	�BB	�5B	�)B	�)B	�)B	�)B	�#B	�B	�#B	�5B	�NB	�`B	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
&2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222242222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.23 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191727                              AO  ARCAADJP                                                                    20181005191727    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191727  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191727  QCF$                G�O�G�O�G�O�8000            