CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:34Z creation      
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
resolution        =���   axis      Z        p  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J,   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  L   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  Sx   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  f   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  m�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~<   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �4   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �D   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �H   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �X   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �\   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �`   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �dArgo profile    3.1 1.2 19500101000000  20181005190534  20181005190534  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @�٩}'�1   @�٪�o�@0��`A�7�c�     1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @9��@�  @�  A   A   A>ffA`  A�  A�  A�  A�  A�  A�33A�  A�  B   B  BffB  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bw��B�  B�  B�  B�  B�  B���B���B���B���B���B�  B�33B�  B�  B�  B�  B�  B���B���B�  B�33B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  C �C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@�CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C��3C��3C��C�  C�  C��C��C�  C��3C��3C�  C�  C�  C�  C�  C�  C��C�  C��C��C�  C��3C��3C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C��3C�  C�  C�  C�  C��C��C�  C�  C�  C��C��C�  C��3C�  C��C�  C�  C�  C��3C��3D y�D ��Dy�D��D� D  D� D  D� D��Dy�D  D� DfD� D��D� D	  D	� D
  D
� D  D� D  D�fD��Dy�D  D�fD  Dy�D��D� DfD� D  Dy�D  D� D  Dy�D  D� D  D� D��D� D  Dy�D��Dy�D��D� D  D� D  D� D  D� D��Dy�D��D� D   D y�D ��D!� D"  D"� D#  D#� D#��D$� D%  D%�fD&  D&� D'  D'y�D(  D(�fD)  D)� D*  D*� D*��D+y�D+��D,� D,��D-� D.fD.� D.��D/� D0fD0� D1  D1�fD2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D8��D9� D:  D:� D;  D;� D;��D<y�D=  D=� D=��D>y�D?  D?� D@  D@� DAfDA�fDBfDB�fDCfDC�fDDfDD� DD��DE� DF  DF�fDG  DG� DH  DH� DIfDI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DN��DO� DP  DPy�DQ  DQ�fDR  DR� DS  DS� DS��DT� DU  DU� DV  DV� DW  DW� DX  DX� DYfDY�fDZ  DZ� DZ��D[y�D\  D\�fD]  D]y�D^  D^�fD_fD_�fD`  D`� DafDa�fDb  Db� Dc  Dc� Dd  Dd� De  De�fDf  Df� Dg  Dgy�Dg��Dh� Di  Di� Dj  Djy�Dk  Dk�fDl  Dly�Dm  Dm� DnfDn�fDo  Do� Dp  Dpy�Dp��Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dw�fDy��D�@RD�D{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@HQ�@�\)@�\)A�A#�ABzAc�A��
A��
A��
A��
A��
A�
=A��
A��
B �B�BQ�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�u�B�u�B�u�B�u�B�u�B�B�B�B�B�B�B�B�B�B�B�u�B���B�u�B�u�B�u�B�u�B�u�B�B�B�B�B�u�BШ�B�u�B�u�B�u�B�u�B�u�B�u�B�B�B�u�B�u�B�u�B�u�C T{C:�C:�C:�C:�C
:�C:�C:�C:�C:�C:�C:�C:�C:�C:�C:�C :�C":�C$:�C&:�C(:�C*:�C,:�C.:�C0:�C2:�C4:�C6:�C8:�C::�C<:�C>:�C@T{CB:�CD:�CF:�CH:�CJ:�CL:�CN:�CP:�CR:�CT:�C�qC�qC�qC�qC�qC��C��C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�*>C�qC�qC�qC�qC�*>C�qC�qC�qC�qC�qC�qC�qC�*>C�*>C�*>C�qC�qC��C�qC�*>C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC��C��C�qC�qC�qC��C��C�*>C�qC�qC�*>C�*>C�qC��C��C�qC�qC�qC�qC�qC�qC�*>C�qC�*>C�*>C�qC��C��C�qC�*>C�*>C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC��C�qC�qC��C��C�qC�qC�qC�qC�*>C�*>C�qC�qC�qC�*>C�*>C�qC��C�qC�*>C�qC�qC�qC��D RD �RDRD�RDRD��D�D��D�D��DRD�RD�D��DD��DRD��D	�D	��D
�D
��D�D��D�D�DRD�RD�D�D�D�RDRD��DD��D�D�RD�D��D�D�RD�D��D�D��DRD��D�D�RDRD�RDRD��D�D��D�D��D�D��DRD�RDRD��D �D �RD!RD!��D"�D"��D#�D#��D$RD$��D%�D%�D&�D&��D'�D'�RD(�D(�D)�D)��D*�D*��D+RD+�RD,RD,��D-RD-��D.D.��D/RD/��D0D0��D1�D1�D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9RD9��D:�D:��D;�D;��D<RD<�RD=�D=��D>RD>�RD?�D?��D@�D@��DADA�DBDB�DCDC�DDDD��DERDE��DF�DF�DG�DG��DH�DH��DIDI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DORDO��DP�DP�RDQ�DQ�DR�DR��DS�DS��DTRDT��DU�DU��DV�DV��DW�DW��DX�DX��DYDY�DZ�DZ��D[RD[�RD\�D\�D]�D]�RD^�D^�D_D_�D`�D`��DaDa�Db�Db��Dc�Dc��Dd�Dd��De�De�Df�Df��Dg�Dg�RDhRDh��Di�Di��Dj�Dj�RDk�Dk�Dl�Dl�RDm�Dm��DnDn�Do�Do��Dp�Dp�RDqRDq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dw�Dy�GD�G�D�K�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ȴA�ƨA�ĜA�ĜA�A�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A���A��
A��
A��
A���A��
A���A���A��/A��#A��#A��#A��#A���AӮAӗ�Aә�Aӕ�Aӗ�Aә�Aӡ�Aӥ�AӬAӲ-AӼjA�AӾwAӋDA�bA��;A��`A�"�A�VAЁA�ZA���A�33A̸RAɍPA�A�l�A��A��A��uA���A���A��A��A�/A�x�A�/A�  A��A�9XA��!A�v�A���A���A��A��PA��uA�ƨA���A�S�A���A��FA�/A���A�n�A�%A�A���A�9XA���A�jA�A�E�A�9XA�\)A�7LA���A��A�C�A��yA� �A�+A�`BA�5?A�9XAW�;AV�/AVr�AVJAU33AQoAN �ALE�AI
=AF��AD��ACA=�A:~�A9�A8r�A8{A7�A6r�A5�A3��A2n�A0�RA/�7A.�RA.ffA-��A-�A,�A+��A(��A'\)A'�A&��A&�HA&�!A&~�A&bNA#��A!��A ��A   A�A�A�jA�+A�A��AXA�A��A �A�
AXAE�AG�A1'A�A��A�TAO�A
=A
��A
=qA��AE�Ap�A�A��A�-AM�A�hA �u@�+@���@��@�Q�@��@���@�K�@��@�D@��@�ȴ@�X@�@�o@�+@�?}@�Z@�1@��@���@�p�@���@�(�@睲@�dZ@�@�E�@�7@���@�z�@�  @�P@���@�x�@��@�1@�dZ@ޟ�@�V@�=q@�@܋D@۾w@�
=@ڗ�@�$�@���@�/@׮@�C�@���@Դ9@��m@ӶF@ӥ�@ӕ�@��@҇+@�$�@�hs@�&�@��@�r�@�(�@�9X@�dZ@��H@��@�Z@�K�@��@���@ʟ�@ʟ�@�b@��@�b@˶F@�ȴ@�x�@�dZ@��@�@��-@��@�^5@��7@��u@��u@�X@�n�@���@���@��@�E�@���@�$�@�O�@�b@�S�@�K�@��@�o@�5?@�x�@�&�@��u@��u@��D@�j@�  @��@�S�@�S�@�;d@��@�o@�C�@��@�dZ@��R@�@�@�^5@���@�O�@�O�@���@�bN@��@���@��@�ƨ@�5?@��#@���@�M�@���@��y@��y@��y@��@���@�$�@�@��@���@�hs@��@��9@�bN@�dZ@�~�@�x�@���@�A�@�  @�Q�@���@�&�@�j@��
@�"�@�~�@�^5@�V@���@�I�@�
=@��!@��!@���@�-@��@���@���@�;d@�t�@��F@��m@��u@��@�&�@��@���@� �@�S�@���@���@�=q@�n�@�E�@��@�@���@���@��/@��@�A�@�;d@�"�@���@�@�ȴ@��!@��!@�~�@��^@�/@��u@�9X@�j@�Z@�1@� �@��@�  @��H@�^5@�=q@�5?@��-@��`@��j@�z�@� �@��@���@�ƨ@��@���@���@��@��F@��@�l�@�ff@��-@�/@�V@��`@���@��D@���@�V@���@�A�@�|�@��y@�v�@�n�@�~�@�~�@�@���@�?}@��9@��j@���@�bN@�b@�t�@�o@�ȴ@�n�@�M�@�V@�v�@���@��H@��!@���@���@�ff@�=q@�E�@�J@��7@���@��@��D@� �@��F@�+@���@�~�@�n�@�ff@�V@�E�@�=q@�-@���@�p�@���@�e,@r11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ȴA�ƨA�ĜA�ĜA�A�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A���A��
A��
A��
A���A��
A���A���A��/A��#A��#A��#A��#A���AӮAӗ�Aә�Aӕ�Aӗ�Aә�Aӡ�Aӥ�AӬAӲ-AӼjA�AӾwAӋDA�bA��;A��`A�"�A�VAЁA�ZA���A�33A̸RAɍPA�A�l�A��A��A��uA���A���A��A��A�/A�x�A�/A�  A��A�9XA��!A�v�A���A���A��A��PA��uA�ƨA���A�S�A���A��FA�/A���A�n�A�%A�A���A�9XA���A�jA�A�E�A�9XA�\)A�7LA���A��A�C�A��yA� �A�+A�`BA�5?A�9XAW�;AV�/AVr�AVJAU33AQoAN �ALE�AI
=AF��AD��ACA=�A:~�A9�A8r�A8{A7�A6r�A5�A3��A2n�A0�RA/�7A.�RA.ffA-��A-�A,�A+��A(��A'\)A'�A&��A&�HA&�!A&~�A&bNA#��A!��A ��A   A�A�A�jA�+A�A��AXA�A��A �A�
AXAE�AG�A1'A�A��A�TAO�A
=A
��A
=qA��AE�Ap�A�A��A�-AM�A�hA �u@�+@���@��@�Q�@��@���@�K�@��@�D@��@�ȴ@�X@�@�o@�+@�?}@�Z@�1@��@���@�p�@���@�(�@睲@�dZ@�@�E�@�7@���@�z�@�  @�P@���@�x�@��@�1@�dZ@ޟ�@�V@�=q@�@܋D@۾w@�
=@ڗ�@�$�@���@�/@׮@�C�@���@Դ9@��m@ӶF@ӥ�@ӕ�@��@҇+@�$�@�hs@�&�@��@�r�@�(�@�9X@�dZ@��H@��@�Z@�K�@��@���@ʟ�@ʟ�@�b@��@�b@˶F@�ȴ@�x�@�dZ@��@�@��-@��@�^5@��7@��u@��u@�X@�n�@���@���@��@�E�@���@�$�@�O�@�b@�S�@�K�@��@�o@�5?@�x�@�&�@��u@��u@��D@�j@�  @��@�S�@�S�@�;d@��@�o@�C�@��@�dZ@��R@�@�@�^5@���@�O�@�O�@���@�bN@��@���@��@�ƨ@�5?@��#@���@�M�@���@��y@��y@��y@��@���@�$�@�@��@���@�hs@��@��9@�bN@�dZ@�~�@�x�@���@�A�@�  @�Q�@���@�&�@�j@��
@�"�@�~�@�^5@�V@���@�I�@�
=@��!@��!@���@�-@��@���@���@�;d@�t�@��F@��m@��u@��@�&�@��@���@� �@�S�@���@���@�=q@�n�@�E�@��@�@���@���@��/@��@�A�@�;d@�"�@���@�@�ȴ@��!@��!@�~�@��^@�/@��u@�9X@�j@�Z@�1@� �@��@�  @��H@�^5@�=q@�5?@��-@��`@��j@�z�@� �@��@���@�ƨ@��@���@���@��@��F@��@�l�@�ff@��-@�/@�V@��`@���@��D@���@�V@���@�A�@�|�@��y@�v�@�n�@�~�@�~�@�@���@�?}@��9@��j@���@�bN@�b@�t�@�o@�ȴ@�n�@�M�@�V@�v�@���@��H@��!@���@���@�ff@�=q@�E�@�J@��7@���@��@��D@� �@��F@�+@���@�~�@�n�@�ff@�V@�E�@�=q@�-@���@�p�@���@�e,@r11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�VB
�VB
�\B
�\B
�bB
�oB
�{B
��B
��B
��B
��B
��B
�LB
��B
��B
�)B
��B
�B
�NB
�NB
�ZBVBB�B<jB
�B
��B
�jB
��B
�`B
�BBJB�B&�B=qB_;B{�B�hB��B�B�B�TBuB2-Bk�Bn�B|�B�1B�1B{�By�Bo�BZBP�BG�B>wB;dB5?B0!B'�B�B{B1B�;BȴB�dB�!B��B��B�7Bm�B33BPB
��B
��B�9B�!B�B��B��B��B�VB�=B�+B�B� Bz�By�B}�B�B�B�B�B�B�%B�VB�hB��B��B��B��B��B��B�B�'B�qBÖBÖBÖBÖB��B��B�qB�^B�FB�'B��B��B��B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�!B�-B�B�B��B��B��B��B�B�B�!B�-B�3B�LB�dB�wB��B��B��B��BÖBĜBĜBƨBƨBƨBȴB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�)B�)B�)B�#B�#B�#B�#B�;B�BB�BB�BB�HB�`B�`B�mB�yB�B�B�B�B�B�B��B��B��B	B	1B	\B	oB	�B	{B	uB	bB	hB	�B	�B	"�B	'�B	49B	9XB	=qB	?}B	@�B	>wB	>wB	:^B	:^B	<jB	D�B	H�B	G�B	D�B	E�B	J�B	T�B	S�B	K�B	F�B	K�B	Q�B	Q�B	N�B	L�B	L�B	L�B	L�B	L�B	K�B	N�B	Q�B	T�B	ZB	^5B	_;B	_;B	`BB	`BB	bNB	e`B	ffB	hsB	l�B	n�B	p�B	q�B	u�B	y�B	� B	�PB	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	�B	�!B	�?B	�LB	�XB	�dB	�qB	B	B	ĜB	ƨB	ƨB	ƨB	ɺB	��B	ɺB	ƨB	ĜB	ÖB	B	B	B	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	ŢB	ĜB	ĜB	ĜB	B	ÖB	ÖB	��B	�wB	��B	ÖB	ŢB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�#B	�/B	�5B	�;B	�/B	�/B	�;B	�5B	�;B	�HB	�NB	�ZB	�ZB	�`B	�ZB	�TB	�NB	�HB	�`B	�yB	�B	�B	�B	�B	�B	�B	�sB	�sB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
+B
1B
	7B
	7B

=B
	7B
	7B

=B
PB
PB
JB
JB
DB

=B
	7B
1B
+B
+B
+B
+B
+B
+B
+B
+B
+B

=B
	B
dB
*22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222B
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�VB
�VB
�\B
�\B
�bB
�oB
�{B
��B
��B
��B
��B
��B
�LB
��B
��B
�)B
��B
�B
�NB
�NB
�ZBVBB�B<jB
�B
��B
�jB
��B
�`B
�BBJB�B&�B=qB_;B{�B�hB��B�B�B�TBuB2-Bk�Bn�B|�B�1B�1B{�By�Bo�BZBP�BG�B>wB;dB5?B0!B'�B�B{B1B�;BȴB�dB�!B��B��B�7Bm�B33BPB
��B
��B�9B�!B�B��B��B��B�VB�=B�+B�B� Bz�By�B}�B�B�B�B�B�B�%B�VB�hB��B��B��B��B��B��B�B�'B�qBÖBÖBÖBÖB��B��B�qB�^B�FB�'B��B��B��B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�!B�-B�B�B��B��B��B��B�B�B�!B�-B�3B�LB�dB�wB��B��B��B��BÖBĜBĜBƨBƨBƨBȴB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�)B�)B�)B�#B�#B�#B�#B�;B�BB�BB�BB�HB�`B�`B�mB�yB�B�B�B�B�B�B��B��B��B	B	1B	\B	oB	�B	{B	uB	bB	hB	�B	�B	"�B	'�B	49B	9XB	=qB	?}B	@�B	>wB	>wB	:^B	:^B	<jB	D�B	H�B	G�B	D�B	E�B	J�B	T�B	S�B	K�B	F�B	K�B	Q�B	Q�B	N�B	L�B	L�B	L�B	L�B	L�B	K�B	N�B	Q�B	T�B	ZB	^5B	_;B	_;B	`BB	`BB	bNB	e`B	ffB	hsB	l�B	n�B	p�B	q�B	u�B	y�B	� B	�PB	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	�B	�!B	�?B	�LB	�XB	�dB	�qB	B	B	ĜB	ƨB	ƨB	ƨB	ɺB	��B	ɺB	ƨB	ĜB	ÖB	B	B	B	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	ŢB	ĜB	ĜB	ĜB	B	ÖB	ÖB	��B	�wB	��B	ÖB	ŢB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�#B	�/B	�5B	�;B	�/B	�/B	�;B	�5B	�;B	�HB	�NB	�ZB	�ZB	�`B	�ZB	�TB	�NB	�HB	�`B	�yB	�B	�B	�B	�B	�B	�B	�sB	�sB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
+B
1B
	7B
	7B

=B
	7B
	7B

=B
PB
PB
JB
JB
DB

=B
	7B
1B
+B
+B
+B
+B
+B
+B
+B
+B
+B

=B
	B
dB
*22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.23 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190534                              AO  ARCAADJP                                                                    20181005190534    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190534  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190534  QCF$                G�O�G�O�G�O�8000            