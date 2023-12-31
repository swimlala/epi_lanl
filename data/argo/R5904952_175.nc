CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:45Z creation      
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
resolution        =���   axis      Z        h  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  K�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  S`   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  mP   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  }�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �p   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �p   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �p   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �p   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190545  20181005190545  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��)�xNh1   @��*l�f@1X�t�j�c��+1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @���@�  A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�33A�33B   B  B  B  B   B(ffB0  B8  B@  BHffBPffBXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C�fC�fC  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH�CJ  CL  CN  CP  CR  CT  C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C��C��C��C�  C��3C��3C�  C��3C��3C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C��C��C��C��C��C��C�  C�  C�  C�  C�  C��3C��3C�  C��C�  C�  C��3C��3C�  C�  C��3C��3C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C��3C�  C�  C��C�  C��C��C�  C�  C�  C�  C��C�  C��C��C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  D   D y�D  D� D  D� D��D� D  D� D��D� D  D� D  D� D  D� D��D	y�D	��D
y�D
��D� D  D� D  Dy�D  D�fD  Dy�D��Dy�D��D� DfD� DfD�fD  D� DfD� D  D� D  D� D  D� D  D� D  D� DfD�fD  D� D  D� D  D�fD  D� D   D y�D ��D!� D"  D"�fD#  D#� D$  D$� D%  D%� D&fD&�fD'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-y�D.  D.� D/  D/y�D0  D0� D1  D1� D1��D2y�D2��D3� D4  D4� D4��D5� D6  D6� D7  D7�fD8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>fD>� D>��D?� D@fD@�fDAfDA�fDBfDB� DC  DC� DC��DDy�DE  DEy�DE��DFy�DF��DG� DHfDH� DH��DI� DJfDJ� DJ��DK� DL  DL�fDMfDM� DM��DNy�DN��DO� DP  DP� DQfDQ� DQ��DR� DS  DS� DTfDT� DU  DU� DV  DVy�DW  DW� DX  DX� DYfDY� DZ  DZ� D[  D[�fD\  D\� D]  D]� D^  D^� D_  D_� D`fD`� D`��Da� Db  Db�fDcfDc� Dd  Dd� DefDe�fDffDf� Df��Dgy�Dg��Dhy�Dh��Di� Dj  Dj� Dk  Dk� Dk��Dl� Dm  Dm� Dn  Dn� Do  Doy�Dp  Dpy�Dq  Dq�fDr  Dry�Ds  Ds� Ds��Dt� Du  Du� Dv  Dv� Dw  Dwy�Dy��D�4{D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�@���Az�A$z�AB�GAdz�A�=qA�=qA�=qA�=qA�=qA�=qA�p�A�p�B�B	�B�B�B!�B)�B1�B9�BA�BI�BQ�BY�Ba�Bi�Bq�By�B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B�\)B�\)B�\)B�\)B��\B��\Bď\Bȏ\B̏\BЏ\Bԏ\B؏\B�B��\B�\B�\B�\B��\B�\B��\B��\C G�CG�CG�CG�CG�C
G�CG�CG�CG�CG�C.C.CG�CG�CG�CG�C G�C"G�C$G�C&G�C(G�C*G�C,G�C.G�C0G�C2G�C4G�C6G�C8G�C:G�C<G�C>G�C@G�CBG�CDG�CFG�CHaHCJG�CLG�CNG�CPG�CRG�CTG�C�
C�#�C�#�C�#�C�#�C�#�C�0�C�#�C�#�C�#�C�#�C�
C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�#�C�#�C�#�C�#�C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�#�C�#�C�
C�#�C�#�C�#�C�#�C�#�C�0�C�0�C�0�C�#�C�
C�
C�#�C�
C�
C�#�C�#�C�#�C�#�C�#�C�#�C�
C�
C�#�C�#�C�0�C�0�C�0�C�0�C�0�C�0�C�#�C�#�C�#�C�#�C�#�C�
C�
C�#�C�0�C�#�C�#�C�
C�
C�#�C�#�C�
C�
C�#�C�#�C�#�C�#�C�#�C�
C�
C�#�C�#�C�#�C�#�C�
C�#�C�#�C�0�C�#�C�0�C�0�C�#�C�#�C�#�C�#�C�0�C�#�C�0�C�0�C�#�C�#�C�#�C�#�C�#�C�
C�
C�#�C�#�C�#�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D�RD�D��D�D��D�D��DRD��DRD�RD�D��DRD��D�D��D�D��D�D��D�D��D�D��DRD�RD�D��D�D��D�D�RD�D��D �D ��D!�D!��D"�D"�RD#�D#��D$�D$��D%�D%��D&RD&�RD'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7�RD8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>RD>��D?�D?��D@RD@�RDARDA�RDBRDB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DHRDH��DI�DI��DJRDJ��DK�DK��DL�DL�RDMRDM��DN�DN��DO�DO��DP�DP��DQRDQ��DR�DR��DS�DS��DTRDT��DU�DU��DV�DV��DW�DW��DX�DX��DYRDY��DZ�DZ��D[�D[�RD\�D\��D]�D]��D^�D^��D_�D_��D`RD`��Da�Da��Db�Db�RDcRDc��Dd�Dd��DeRDe�RDfRDf��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq�RDr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dy�{D�=qD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��TA��/A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�
=A�JA�JA�VA�VA�bA�{A��A��A�A�ĜAͮAͩ�AͮA�ĜA��A��HA��/A��A�A�/A�$�A��A�G�A�{A���A�E�A��/A�jA��HAȸRA�jA�VAǮA���A�E�AđhA�\)A7A�33A���A�dZA��A���A��7A��A��HA�n�A��wA�9XA�ƨA��A�A�hsA���A�VA���A���A��^A�bNA��9A���A��A�l�A��+A��9A���A�bA�%A��A�hsA�S�A�%A�XA� �A�dZA���A�=qA�M�A��;A�oA��A�G�A��yA���A��wA�ĜA��RA<�A;7LA:�A8�A5A4�A0�yA-�A+�A*�uA*1'A)��A)XA(r�A&��A%G�A$r�A#|�A"n�A!t�A �!A�AC�AVA-A��A��A�!A~�A��A9XA�#A��A�hA�A�AVA�;A��A�RA-AC�A�yAjA��AĜA1'A�hA
=A
�A	�A�9AhsAQ�A��A�!Av�A(�A  A��AK�AdZA A�@��m@���@�
=@���@��@���@��@��@�@�/@�;d@���@�"�@��@�p�@�K�@�E�@웦@�bN@�Q�@�P@��@�@� �@�@�C�@�O�@��;@�~�@���@�/@��D@�Q�@�@�9X@��;@�l�@��y@�J@���@ؓu@�Q�@�(�@�1@�ƨ@�S�@�v�@պ^@�hs@��@�V@�&�@���@�+@�$�@с@Ͼw@�-@�/@�z�@�b@�C�@��@�ȴ@ʟ�@�^5@ɲ-@�Ĝ@�1@�ƨ@�|�@�
=@�5?@��T@�@�@ũ�@�hs@�/@Ĭ@��@��@�1'@�z�@ċD@�Q�@�1@��
@�|�@�+@��@�
=@¸R@�$�@�J@��T@�O�@�V@�bN@�b@��@���@��F@��@��@���@�E�@�@�x�@�X@�/@���@���@�z�@�Q�@��@�S�@�x�@�7L@�p�@���@�@���@��7@�x�@�hs@��@��9@�(�@��@�K�@��R@�$�@��@��`@���@�Q�@��w@�S�@�C�@�
=@���@��@��@�;d@���@���@��7@�Q�@���@���@���@�E�@���@��h@�X@���@�A�@�(�@�b@��w@���@�l�@�33@�33@�o@���@��y@��@��!@�ff@�{@��^@��@�Ĝ@��j@�Z@�l�@�n�@�=q@�=q@��^@�X@���@���@��h@��`@�
=@���@�$�@��@�@���@�X@�O�@��/@���@���@�;d@�o@���@��R@��+@�@���@�x�@���@�r�@�I�@� �@��
@��w@�+@��R@�^5@�M�@��@�hs@��@��9@���@���@�A�@���@���@��D@�r�@�(�@���@�K�@��y@�V@��@���@��7@�Ĝ@��9@�9X@���@��F@�C�@�
=@��!@�ff@�5?@��@��@�@���@��h@�O�@�/@��/@��@�j@�Q�@�9X@���@��;@��w@�t�@�+@���@�n�@�V@�{@�@��7@�hs@�O�@�&�@��/@��u@�r�@�Q�@��@�  @��
@��@��@�\)@�+@��@���@���@�^5@�-@���@��h@��@��j@���@�j@��@�1@��
@��@���@��@��y@���@���@�^5@���@}�9@jM�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��TA��/A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�
=A�JA�JA�VA�VA�bA�{A��A��A�A�ĜAͮAͩ�AͮA�ĜA��A��HA��/A��A�A�/A�$�A��A�G�A�{A���A�E�A��/A�jA��HAȸRA�jA�VAǮA���A�E�AđhA�\)A7A�33A���A�dZA��A���A��7A��A��HA�n�A��wA�9XA�ƨA��A�A�hsA���A�VA���A���A��^A�bNA��9A���A��A�l�A��+A��9A���A�bA�%A��A�hsA�S�A�%A�XA� �A�dZA���A�=qA�M�A��;A�oA��A�G�A��yA���A��wA�ĜA��RA<�A;7LA:�A8�A5A4�A0�yA-�A+�A*�uA*1'A)��A)XA(r�A&��A%G�A$r�A#|�A"n�A!t�A �!A�AC�AVA-A��A��A�!A~�A��A9XA�#A��A�hA�A�AVA�;A��A�RA-AC�A�yAjA��AĜA1'A�hA
=A
�A	�A�9AhsAQ�A��A�!Av�A(�A  A��AK�AdZA A�@��m@���@�
=@���@��@���@��@��@�@�/@�;d@���@�"�@��@�p�@�K�@�E�@웦@�bN@�Q�@�P@��@�@� �@�@�C�@�O�@��;@�~�@���@�/@��D@�Q�@�@�9X@��;@�l�@��y@�J@���@ؓu@�Q�@�(�@�1@�ƨ@�S�@�v�@պ^@�hs@��@�V@�&�@���@�+@�$�@с@Ͼw@�-@�/@�z�@�b@�C�@��@�ȴ@ʟ�@�^5@ɲ-@�Ĝ@�1@�ƨ@�|�@�
=@�5?@��T@�@�@ũ�@�hs@�/@Ĭ@��@��@�1'@�z�@ċD@�Q�@�1@��
@�|�@�+@��@�
=@¸R@�$�@�J@��T@�O�@�V@�bN@�b@��@���@��F@��@��@���@�E�@�@�x�@�X@�/@���@���@�z�@�Q�@��@�S�@�x�@�7L@�p�@���@�@���@��7@�x�@�hs@��@��9@�(�@��@�K�@��R@�$�@��@��`@���@�Q�@��w@�S�@�C�@�
=@���@��@��@�;d@���@���@��7@�Q�@���@���@���@�E�@���@��h@�X@���@�A�@�(�@�b@��w@���@�l�@�33@�33@�o@���@��y@��@��!@�ff@�{@��^@��@�Ĝ@��j@�Z@�l�@�n�@�=q@�=q@��^@�X@���@���@��h@��`@�
=@���@�$�@��@�@���@�X@�O�@��/@���@���@�;d@�o@���@��R@��+@�@���@�x�@���@�r�@�I�@� �@��
@��w@�+@��R@�^5@�M�@��@�hs@��@��9@���@���@�A�@���@���@��D@�r�@�(�@���@�K�@��y@�V@��@���@��7@�Ĝ@��9@�9X@���@��F@�C�@�
=@��!@�ff@�5?@��@��@�@���@��h@�O�@�/@��/@��@�j@�Q�@�9X@���@��;@��w@�t�@�+@���@�n�@�V@�{@�@��7@�hs@�O�@�&�@��/@��u@�r�@�Q�@��@�  @��
@��@��@�\)@�+@��@���@���@�^5@�-@���@��h@��@��j@���@�j@��@�1@��
@��@���@��@��y@���@���@�^5@���@}�9@jM�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�XB�XB�XB�XB�^B�^B�dB�dB�^B�^B�^B�^B�^B�^B�^B�XB�XB��B��B�}B��B��BBĜBɺB�
B  BW
B��B�B��B�
B�#B�/B�)B�fB��B	(�B	��B
��B
�?B
��B
�B6FBH�B\)Be`BgmBiyBm�Bu�B�hB��B��B��B�B��B��B��B�qB��B�B�sB�B�B��BhB�B$�B,B,B0!B?}BE�BK�BE�B?}B8RB0!B.B1'B49B0!B$�B\B��B��B��B��Bu�Bl�B]/BO�BD�B6FB)�B�B
��B
�HB
ɺB
�RB
��B
��B
�7B
|�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�!B�B�B�B��B��B��B��B��B��B��B��B�B�B�!B�'B�3B�?B�FB�XB��B�wB�qB�}B��B��B��B�wBBŢB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�#B�#B�BB�ZB�ZB�ZB�fB�yB�B�B�B�B�B��B��B��B��B��B	B	B		7B	DB	PB	VB	hB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	#�B	$�B	&�B	(�B	)�B	+B	,B	-B	0!B	5?B	8RB	9XB	;dB	>wB	E�B	G�B	H�B	H�B	M�B	O�B	O�B	O�B	O�B	P�B	Q�B	T�B	XB	XB	W
B	XB	ZB	ZB	ZB	[#B	^5B	^5B	`BB	aHB	dZB	dZB	hsB	jB	k�B	l�B	m�B	n�B	p�B	r�B	u�B	w�B	x�B	y�B	z�B	}�B	~�B	�B	�B	�B	�B	�B	�B	�B	�7B	�JB	�VB	�\B	�\B	�\B	�\B	�\B	�hB	�oB	�oB	�uB	�{B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�-B	�3B	�?B	�?B	�?B	�?B	�FB	�LB	�XB	�dB	�wB	��B	��B	��B	��B	�}B	ÖB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�/B	�BB	�HB	�HB	�NB	�NB	�NB	�ZB	�`B	�fB	�sB	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
1B
1B
	7B

=B

=B

=B
DB
JB
JB
JB
JB
JB
VB
VB
VB
\B
bB
hB
&�B
.�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B�XB�XB�XB�XB�^B�^B�dB�dB�^B�^B�^B�^B�^B�^B�^B�XB�XB��B��B�}B��B��BBĜBɺB�
B  BW
B��B�B��B�
B�#B�/B�)B�fB��B	(�B	��B
��B
�?B
��B
�B6FBH�B\)Be`BgmBiyBm�Bu�B�hB��B��B��B�B��B��B��B�qB��B�B�sB�B�B��BhB�B$�B,B,B0!B?}BE�BK�BE�B?}B8RB0!B.B1'B49B0!B$�B\B��B��B��B��Bu�Bl�B]/BO�BD�B6FB)�B�B
��B
�HB
ɺB
�RB
��B
��B
�7B
|�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�!B�B�B�B��B��B��B��B��B��B��B��B�B�B�!B�'B�3B�?B�FB�XB��B�wB�qB�}B��B��B��B�wBBŢB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�#B�#B�BB�ZB�ZB�ZB�fB�yB�B�B�B�B�B��B��B��B��B��B	B	B		7B	DB	PB	VB	hB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	#�B	$�B	&�B	(�B	)�B	+B	,B	-B	0!B	5?B	8RB	9XB	;dB	>wB	E�B	G�B	H�B	H�B	M�B	O�B	O�B	O�B	O�B	P�B	Q�B	T�B	XB	XB	W
B	XB	ZB	ZB	ZB	[#B	^5B	^5B	`BB	aHB	dZB	dZB	hsB	jB	k�B	l�B	m�B	n�B	p�B	r�B	u�B	w�B	x�B	y�B	z�B	}�B	~�B	�B	�B	�B	�B	�B	�B	�B	�7B	�JB	�VB	�\B	�\B	�\B	�\B	�\B	�hB	�oB	�oB	�uB	�{B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�-B	�3B	�?B	�?B	�?B	�?B	�FB	�LB	�XB	�dB	�wB	��B	��B	��B	��B	�}B	ÖB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�/B	�BB	�HB	�HB	�NB	�NB	�NB	�ZB	�`B	�fB	�sB	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
1B
1B
	7B

=B

=B

=B
DB
JB
JB
JB
JB
JB
VB
VB
VB
\B
bB
hB
&�B
.�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.28 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190545                              AO  ARCAADJP                                                                    20181005190545    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190545  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190545  QCF$                G�O�G�O�G�O�8000            