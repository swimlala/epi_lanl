CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:11Z creation      
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
_FillValue                    �dArgo profile    3.1 1.2 19500101000000  20181005190511  20181005190511  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @״��\� 1   @״�F<@2vE�����c�dZ�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   B   @333@y��@�  A   A   A@  Aa��A~ffA�33A�33A�  A�  A�33A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBP  BX  B`  Bh  Bp  Bw��B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B���B�  B�  B�  B�  C �C�C  C  C  C
  C  C  C  C  C�fC�fC  C  C�C  C   C"  C$  C&  C(  C)�fC,  C.  C0  C2  C4  C6  Cf  Cg�fCj  Cl  Cn  Cp  Cq�fCt  Cv  Cx  Cz  C|�C~  C�  C��C�  C�  C��C��C��C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C��C��C�  C�  C��C�  C��3C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C��3C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��C��C��C��C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C��C��C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  D fD �fD  Dy�D��Dy�D��D� D  D� D  D�fD  Dy�D��D� D  D�fD	fD	� D
fD
� D  D� D  D� D  D� D��Dy�D  D� D��D� D  D� DfD�fDfD� D  D� D��D� D  D� D  D� D��Dy�D��Dy�D��D� D  D� D  D� D  Dy�D��Dy�D  D�fD fD �fD!fD!� D"  D"� D#  D#�fD$fD$�fD%  D%y�D&  D&� D'  D'� D(fD(� D)  D)� D*fD*� D+  D+� D,  D,� D-  D-� D.  D.� D/fD/� D0  D0y�D1  D1� D2fD2�fD3  D3� D3��D4y�D4��D5� D6  D6y�D7  D7� D8  D8� D9  D9�fD:  D:y�D:��D;� D<  D<� D=  D=� D>fD>�fD?fD?� D?��D@� DA  DA� DB  DBy�DC  DC� DD  DD� DE  DE�fDF  DF� DGfDG� DH  DHy�DH��DIy�DI��DJ� DK  DK� DL  DLy�DM  DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DS  DSy�DS��DT� DU  DU� DV  DV� DW  DW� DW��DXy�DX��DYy�DZ  DZ� DZ��D[� D\fD\�fD]  D]�fD^fD^� D^��D_y�D_��D`� Da  Da� Db  Db� Dc  Dc�fDd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� DifDi� Di��Dj� Dk  Dky�Dk��Dl� Dm  Dm� Dn  Dn� Dn��Doy�Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� DtfDt�fDu  Du� DvfDv�fDw  Dwy�Dw�fDy��D�Q�D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@AG�@��
@�
=A�A#�AC�Ae�A���A���A���A�A�A���A�A�B �HB�HB�HB�HB �HB(�HB0�HB8�HB@�HBIG�BP�HBX�HB`�HBh�HBp�HBxz�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�=qB�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�=qB�p�B�p�B�p�B�=qB�p�B�p�B�p�B�p�C Q�CQ�C8RC8RC8RC
8RC8RC8RC8RC8RC�C�C8RC8RCQ�C8RC 8RC"8RC$8RC&8RC(8RC*�C,8RC.8RC08RC28RC48RC68RCf8RCh�Cj8RCl8RCn8RCp8RCr�Ct8RCv8RCx8RCz8RC|Q�C~8RC�)C�(�C�)C�)C�(�C�(�C�(�C�)C�)C�)C�)C�)C�(�C�)C�\C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�(�C�)C�)C�)C�\C�)C�(�C�(�C�)C�)C�(�C�)C�\C�\C�)C�)C�)C�)C�)C�)C�(�C�)C�)C�)C�)C�(�C�)C�)C�)C�)C�\C�\C�\C�)C�(�C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�(�C�(�C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�(�C�)C�(�C�(�C�(�C�(�C�)C�)C�)C�)C�\C�\C�)C�)C�)C�)C�)C�)C�(�C�(�C�)C�)C�\C�)C�)C�\C�)C�)C�)C�)C�)C�)C�)C�(�C�)C�)C�)C�)C�)C�)C�)D zD �zDD��D�D��D�D�DD�DD�zDD��D�D�DD�zD	zD	�D
zD
�DD�DD�DD�D�D��DD�D�D�DD�DzD�zDzD�DD�D�D�DD�DD�D�D��D�D��D�D�DD�DD�DD��D�D��DD�zD zD �zD!zD!�D"D"�D#D#�zD$zD$�zD%D%��D&D&�D'D'�D(zD(�D)D)�D*zD*�D+D+�D,D,�D-D-�D.D.�D/zD/�D0D0��D1D1�D2zD2�zD3D3�D4�D4��D5�D5�D6D6��D7D7�D8D8�D9D9�zD:D:��D;�D;�D<D<�D=D=�D>zD>�zD?zD?�D@�D@�DADA�DBDB��DCDC�DDDD�DEDE�zDFDF�DGzDG�DHDH��DI�DI��DJ�DJ�DKDK�DLDL��DMDM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DSDS��DT�DT�DUDU�DVDV�DWDW�DX�DX��DY�DY��DZDZ�D[�D[�D\zD\�zD]D]�zD^zD^�D_�D_��D`�D`�DaDa�DbDb�DcDc�zDdDd�DeDe�DfDf�DgDg�DhDh�DizDi�Dj�Dj�DkDk��Dl�Dl�DmDm�DnDn�Do�Do��DpDp�DqDq�DrDr�DsDs�DtzDt�zDuDu�DvzDv�zDwDw��Dw�zDy��D�X�D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A���A���A���A���A���A���A��`A��mA�  A�A�%A�A�%A�
=A�%A�%A�%A�A�A�%A���A���A���A�A���A���A��AԾwA�n�A�r�A�A�VAҡ�Aҝ�AғuAҏ\Aҙ�Aҥ�Aҙ�A�;dA�;dA��A�^5A�bA�
=A���Aˉ7AʸRAʶFA�^5A��/A�p�A�\)A�33A���A��;A���A�ffA�ȴA�33A��#A�p�Aŧ�A��A���A�bNA��A���A�JA��A�=qA�(�A��+A��PA�?}A��`A�p�A��FA���A��A���A��FA��wA�+A���A���A��+A�1'A��A�O�A}
=Ax�DAt=qAp�Al�+AjJAh��Ae�;Aa�A^��A\�!A[�FAZbNAYC�AWoAT~�AQ��AP�!AO�PAN��AN��AN9XAK�AI��AG�hACC�ABbNA?l�A> �A<�9A9l�A6n�A6{A5�7A3��A3�A1�-A/�#A.r�A-XA,$�A+O�A+
=A)%A&�A$=qA"�HA =qA��AA�#A��A�AffAC�A
=AA��A  A�A��A�AM�A1A�;A��A$�A33A�A�uA�TA�AffA��A��Ap�AjA�A��A
�A�Ap�AVA��Al�A&�A�!Al�AVA��A��AQ�A%A $�@��H@�hs@�ƨ@�o@���@�{@�dZ@��@�C�@��@�~�@�@�r�@�|�@�+@�?}@�Ĝ@�j@�ƨ@�
=@�-@��@�9@�1'@��@�D@�bN@�|�@�V@�j@��
@���@��@��@� �@�o@�=q@�x�@؋D@��;@�n�@Ӿw@�M�@���@щ7@�/@��;@Ο�@�{@Ͳ-@�&�@�Q�@˥�@�ȴ@�o@�&�@�1'@ȣ�@�r�@�I�@�1'@�ƨ@�@�E�@őh@� �@ģ�@Ĵ9@�  @Å@��@���@�z�@�A�@���@���@�K�@��\@�M�@�v�@�C�@��@���@��@�o@��@���@���@��h@��7@�G�@�V@��`@��D@�bN@�Z@�(�@�  @��;@���@�o@�$�@���@�-@�x�@�=q@�E�@���@��D@��u@�1'@�"�@�~�@���@��^@��7@�7L@��@���@�z�@�bN@�Z@�1'@�1@�1@�b@�(�@�1'@�(�@� �@��@��m@��w@���@�|�@�\)@�o@��@���@�n�@�=q@�5?@�J@��^@�G�@��u@���@��R@�M�@��@���@�7L@���@��/@���@�r�@�j@�j@�bN@�Z@�Z@�A�@���@��F@���@���@���@���@���@���@�S�@�C�@�K�@��@�X@�&�@��`@��u@��@��m@��w@��P@�K�@��@���@�V@�$�@��@�{@���@��@��`@���@��D@�j@�Q�@�9X@��@��@��@���@�v�@�^5@��@���@���@��h@��7@��7@��7@��h@��7@��h@��7@�hs@���@��@�K�@�+@��H@��R@���@�5?@�$�@���@���@���@��7@��7@��@�7L@��`@���@�Z@�b@��w@�33@��@�v�@���@�G�@���@�j@�I�@���@��9@��D@�Z@�A�@�1'@�(�@�  @��
@���@���@��F@��@�K�@���@���@��!@�^5@�E�@�J@���@�O�@��`@�j@�b@�dZ@��@�v�@�V@�5?@�{@��T@�hs@��@�I�@�9X@�(�@�1@��m@���@�K�@�"�@��@��R@�R�@}�9@l�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A���A���A���A���A���A���A��`A��mA�  A�A�%A�A�%A�
=A�%A�%A�%A�A�A�%A���A���A���A�A���A���A��AԾwA�n�A�r�A�A�VAҡ�Aҝ�AғuAҏ\Aҙ�Aҥ�Aҙ�A�;dA�;dA��A�^5A�bA�
=A���Aˉ7AʸRAʶFA�^5A��/A�p�A�\)A�33A���A��;A���A�ffA�ȴA�33A��#A�p�Aŧ�A��A���A�bNA��A���A�JA��A�=qA�(�A��+A��PA�?}A��`A�p�A��FA���A��A���A��FA��wA�+A���A���A��+A�1'A��A�O�A}
=Ax�DAt=qAp�Al�+AjJAh��Ae�;Aa�A^��A\�!A[�FAZbNAYC�AWoAT~�AQ��AP�!AO�PAN��AN��AN9XAK�AI��AG�hACC�ABbNA?l�A> �A<�9A9l�A6n�A6{A5�7A3��A3�A1�-A/�#A.r�A-XA,$�A+O�A+
=A)%A&�A$=qA"�HA =qA��AA�#A��A�AffAC�A
=AA��A  A�A��A�AM�A1A�;A��A$�A33A�A�uA�TA�AffA��A��Ap�AjA�A��A
�A�Ap�AVA��Al�A&�A�!Al�AVA��A��AQ�A%A $�@��H@�hs@�ƨ@�o@���@�{@�dZ@��@�C�@��@�~�@�@�r�@�|�@�+@�?}@�Ĝ@�j@�ƨ@�
=@�-@��@�9@�1'@��@�D@�bN@�|�@�V@�j@��
@���@��@��@� �@�o@�=q@�x�@؋D@��;@�n�@Ӿw@�M�@���@щ7@�/@��;@Ο�@�{@Ͳ-@�&�@�Q�@˥�@�ȴ@�o@�&�@�1'@ȣ�@�r�@�I�@�1'@�ƨ@�@�E�@őh@� �@ģ�@Ĵ9@�  @Å@��@���@�z�@�A�@���@���@�K�@��\@�M�@�v�@�C�@��@���@��@�o@��@���@���@��h@��7@�G�@�V@��`@��D@�bN@�Z@�(�@�  @��;@���@�o@�$�@���@�-@�x�@�=q@�E�@���@��D@��u@�1'@�"�@�~�@���@��^@��7@�7L@��@���@�z�@�bN@�Z@�1'@�1@�1@�b@�(�@�1'@�(�@� �@��@��m@��w@���@�|�@�\)@�o@��@���@�n�@�=q@�5?@�J@��^@�G�@��u@���@��R@�M�@��@���@�7L@���@��/@���@�r�@�j@�j@�bN@�Z@�Z@�A�@���@��F@���@���@���@���@���@���@�S�@�C�@�K�@��@�X@�&�@��`@��u@��@��m@��w@��P@�K�@��@���@�V@�$�@��@�{@���@��@��`@���@��D@�j@�Q�@�9X@��@��@��@���@�v�@�^5@��@���@���@��h@��7@��7@��7@��h@��7@��h@��7@�hs@���@��@�K�@�+@��H@��R@���@�5?@�$�@���@���@���@��7@��7@��@�7L@��`@���@�Z@�b@��w@�33@��@�v�@���@�G�@���@�j@�I�@���@��9@��D@�Z@�A�@�1'@�(�@�  @��
@���@���@��F@��@�K�@���@���@��!@�^5@�E�@�J@���@�O�@��`@�j@�b@�dZ@��@�v�@�V@�5?@�{@��T@�hs@��@�I�@�9X@�(�@�1@��m@���@�K�@�"�@��@��R@�R�@}�9@l�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�XB
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�dB
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�RB
�9B
��B
��B
�'BB�B �B!�B#�B&�B1'BG�BXBM�BW
B33BoB
��B
�B
ƨB
ÖB
�BDB,B:^BD�BYBdZBp�By�B�%B��B�B�RB��B�#B�BJB)�B;dBYBT�BP�B_;BhsBp�Bv�Bw�By�Bz�B�B��B�hBy�BhsBO�BC�B8RB.B(�B(�B'�Bm��B
�B	��B	�B	�qB	��B	��B	�PB	|�B	ffB	T�B	L�B	G�B	@�B	7LB	,B	�B	\B	+B	B	B��B��B�B�ZB�)B��B��B��BȴBĜB��B�wB�jB�XB�RB�^BBȴB��B��B��B��B��B��BĜB�dB�FB�B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�?B�?B�3B�!B��B��B��B��B��B��B��B��B��B��B��B��B�!B�B��B��B��B��B��B��B��B��B��B�'B�qB�XB�!B�B�B�!B�XB�XB�LB�9B�B�B�B�'B�-B�'B�-B�9B�FB�?B�3B�-B�9B�?B�FB�dB�qB�jB�qB�}B�wB�jB�FB�^B�qBBÖBǮBȴB��B��BȴBƨBƨBƨBƨBȴBȴBȴBȴB��B��B��B��B��B��B��B�B�HB�5B�;B�sB�B�B�B�B�B�B��B��B��B	  B	B	B	B		7B	DB	JB	PB	\B	bB	uB	�B	�B	 �B	(�B	(�B	'�B	,B	2-B	B�B	;dB	A�B	I�B	L�B	M�B	L�B	O�B	R�B	R�B	R�B	Q�B	Q�B	P�B	Q�B	VB	ZB	T�B	YB	aHB	bNB	bNB	iyB	r�B	t�B	s�B	r�B	r�B	r�B	s�B	y�B	{�B	z�B	z�B	{�B	{�B	|�B	�B	�B	�B	�7B	�JB	�VB	�VB	�VB	�\B	�bB	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�-B	�3B	�3B	�9B	�9B	�FB	�RB	�jB	�qB	�wB	�wB	�wB	�}B	��B	ĜB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�)B	�)B	�)B	�/B	�5B	�5B	�5B	�5B	�/B	�/B	�/B	�/B	�5B	�5B	�5B	�5B	�5B	�5B	�5B	�5B	�5B	�5B	�;B	�;B	�;B	�;B	�;B	�NB	�`B	�fB	�fB	�mB	�mB	�mB	�mB	�mB	�mB	�sB	�sB	�mB	�sB	�sB	�yB	�sB	�sB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
\B
�B
EB
(>22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222242222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222B
�XB
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�dB
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�RB
�9B
��B
��B
�'BB�B �B!�B#�B&�B1'BG�BXBM�BW
B33BoB
��B
�B
ƨB
ÖB
�BDB,B:^BD�BYBdZBp�By�B�%B��B�B�RB��B�#B�BJB)�B;dBYBT�BP�B_;BhsBp�Bv�Bw�By�Bz�B�B��B�hBy�BhsBO�BC�B8RB.B(�B(�B'�Bm��B
�B	��B	�B	�qB	��B	��B	�PB	|�B	ffB	T�B	L�B	G�B	@�B	7LB	,B	�B	\B	+B	B	B��B��B�B�ZB�)B��B��B��BȴBĜB��B�wB�jB�XB�RB�^BBȴB��B��B��B��B��B��BĜB�dB�FB�B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�?B�?B�3B�!B��B��B��B��B��B��B��B��B��B��B��B��B�!B�B��B��B��B��B��B��B��B��B��B�'B�qB�XB�!B�B�B�!B�XB�XB�LB�9B�B�B�B�'B�-B�'B�-B�9B�FB�?B�3B�-B�9B�?B�FB�dB�qB�jB�qB�}B�wB�jB�FB�^B�qBBÖBǮBȴB��B��BȴBƨBƨBƨBƨBȴBȴBȴBȴB��B��B��B��B��B��B��B�B�HB�5B�;B�sB�B�B�B�B�B�B��B��B��B	  B	B	B	B		7B	DB	JB	PB	\B	bB	uB	�B	�B	 �B	(�B	(�B	'�B	,B	2-B	B�B	;dB	A�B	I�B	L�B	M�B	L�B	O�B	R�B	R�B	R�B	Q�B	Q�B	P�B	Q�B	VB	ZB	T�B	YB	aHB	bNB	bNB	iyB	r�B	t�B	s�B	r�B	r�B	r�B	s�B	y�B	{�B	z�B	z�B	{�B	{�B	|�B	�B	�B	�B	�7B	�JB	�VB	�VB	�VB	�\B	�bB	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�-B	�3B	�3B	�9B	�9B	�FB	�RB	�jB	�qB	�wB	�wB	�wB	�}B	��B	ĜB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�)B	�)B	�)B	�/B	�5B	�5B	�5B	�5B	�/B	�/B	�/B	�/B	�5B	�5B	�5B	�5B	�5B	�5B	�5B	�5B	�5B	�5B	�;B	�;B	�;B	�;B	�;B	�NB	�`B	�fB	�fB	�mB	�mB	�mB	�mB	�mB	�mB	�sB	�sB	�mB	�sB	�sB	�yB	�sB	�sB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
\B
�B
EB
(>22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222242222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.22 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190511                              AO  ARCAADJP                                                                    20181005190511    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190511  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190511  QCF$                G�O�G�O�G�O�8000            