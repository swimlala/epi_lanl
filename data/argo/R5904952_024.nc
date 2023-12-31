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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190511  20181005190511  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @ױ���GU1   @ױ�s���@2�C��%�c��Q�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   A   @��@y��@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  C   C  C  C  C�C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CA�fCD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cc�fCf  Cg�fCi�fCl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C��3C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C��C��C�  C��3C�  C��C��C�  C��3C��3C��3C�  C�  C��C�  C��3C��3C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C��C��C�  C��3C��3C�  C��C�  C�  C�  C�  C�  C��C��C��C�  C��3C�  C��C�  C�  C��C�  C��3C�  C��C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C��3C�  C�  C�  C��C��C�  C�  C�  D   D � D  D� D��D� D  Dy�D  D� D  Dy�D  D� D  D� D  D� D	  D	� D	��D
� D  D� D  D� D  D�fD  D� D  D� D  D�fDfD� D��D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D��Dy�D  D� D  D�fD fD � D!  D!�fD"  D"� D#  D#� D$fD$� D%  D%�fD&  D&� D&��D'� D(  D(�fD)fD)� D)��D*� D+fD+� D,  D,� D,��D-� D-��D.� D/fD/� D0  D0� D0��D1y�D2  D2� D3  D3� D3��D4� D5fD5� D5��D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;y�D;��D<y�D<��D=� D>  D>y�D>��D?� D@  D@�fDAfDA�fDBfDB� DCfDC�fDDfDD� DD��DE� DF  DFy�DG  DG� DH  DH� DI  DI�fDJfDJ�fDK  DKy�DL  DL�fDM  DMy�DN  DN� DN��DO� DP  DP� DQ  DQ� DR  DRy�DS  DS� DT  DT� DU  DU�fDV  DV� DW  DW� DW��DX� DY  DY� DY��DZ� D[  D[�fD\  D\� D\��D]y�D^  D^� D_fD_�fD`  D`y�Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� DifDi� Di��Dj� Dk  Dk� Dl  Dl� Dm  Dmy�Dn  Dn�fDo  Do� Dp  Dp� Dq  Dqy�Dr  Dr� Ds  Ds�fDtfDt� Du  Duy�Dv  Dv� Dw  Dw� Dw� Dy�{D�Ff111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @'�@��
@�
=A�A#�AC�Ac�A�A�A�A�A�A�A�A�B �HB�HB�HB�HB �HB(�HB0�HB8�HB@�HBH�HBP�HBX�HB`�HBh�HBp�HBx�HB���B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�BУ�B�p�B�p�B�p�B�p�B��B�p�B�p�B�p�B�p�B�p�B�p�C 8RC8RC8RC8RCQ�C
8RC8RC8RC8RC8RC8RC8RC8RC8RC8RC8RC 8RC"8RC$8RC&8RC(8RC*8RC,8RC.8RC08RC28RC48RC68RC88RC:8RC<8RC>8RC@8RCB�CD8RCF8RCH8RCJ8RCL8RCN8RCP8RCR8RCT8RCV8RCX8RCZ8RC\8RC^8RC`8RCb8RCd�Cf8RCh�Cj�Cl8RCn8RCp8RCr8RCt8RCv8RCx8RCz8RC|8RC~8RC�)C�)C�\C�\C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�\C�\C�\C�\C�)C�)C�(�C�)C�)C�)C�)C�(�C�)C�)C�)C�)C�)C�)C�\C�\C�\C�)C�)C�(�C�(�C�)C�\C�)C�(�C�(�C�)C�\C�\C�\C�)C�)C�(�C�)C�\C�\C�)C�)C�)C�(�C�)C�)C�)C�)C�\C�)C�)C�)C�)C�(�C�(�C�(�C�)C�\C�\C�)C�(�C�)C�)C�)C�)C�)C�(�C�(�C�(�C�)C�\C�)C�(�C�)C�)C�(�C�)C�\C�)C�(�C�)C�)C�)C�)C�)C�(�C�(�C�(�C�)C�)C�)C�)C�)C�)C�)C�\C�)C�\C�)C�)C�)C�(�C�(�C�)C�)C�)D D �DD�D�D�DD��DD�DD��DD�DD�DD�D	D	�D
�D
�DD�DD�DD�zDD�DD�DD�zDzD�D�D�DD�DD�DzD�DD�DD�DD�DD�DD�DD�DzD�D�D��DD�DD�zD zD �D!D!�zD"D"�D#D#�D$zD$�D%D%�zD&D&�D'�D'�D(D(�zD)zD)�D*�D*�D+zD+�D,D,�D-�D-�D.�D.�D/zD/�D0D0�D1�D1��D2D2�D3D3�D4�D4�D5zD5�D6�D6�D7D7�D8D8�D9D9�D:D:�D;D;��D<�D<��D=�D=�D>D>��D?�D?�D@D@�zDAzDA�zDBzDB�DCzDC�zDDzDD�DE�DE�DFDF��DGDG�DHDH�DIDI�zDJzDJ�zDKDK��DLDL�zDMDM��DNDN�DO�DO�DPDP�DQDQ�DRDR��DSDS�DTDT�DUDU�zDVDV�DWDW�DX�DX�DYDY�DZ�DZ�D[D[�zD\D\�D]�D]��D^D^�D_zD_�zD`D`��DaDa�DbDb�DcDc�DdDd�DeDe�DfDf�DgDg�DhDh�DizDi�Dj�Dj�DkDk�DlDl�DmDm��DnDn�zDoDo�DpDp�DqDq��DrDr�DsDs�zDtzDt�DuDu��DvDv�DwDw�Dw�Dy��D�Mp111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��TA��AԍPAӰ!A��TA���A���A���Aқ�AѰ!A���A�ȴAН�AГuAЇ+A�t�A�jA�\)A�K�A�A�A�9XA�"�A���A�=qA��A��;A��#A���A�AήA�v�A͏\A���A�~�A�~�Aʡ�A�G�A�bA�G�A˸RA�x�A�I�AʼjA�v�A�M�A�{Aɇ+A���A�1'A�I�A���A�ZA��Aŗ�AľwAď\A�dZA��A���A�ƨA���A��A�9XA�{A��wA��A�t�A�I�A�VA�bA��A��A��yA��`A�A�A�K�A�$�A�p�A���A��+A���A�%A�^5A�&�A���A��9A�|�A��A�`BA���A�v�A��A�Q�A�XA�A��+A�9XA��jA�ffA���A��A�  A�G�A�ZA�
=A��A���A�hsA��uA|ȴAyS�AvZAu�TAt9XAnĜAe�TAb��A]�
A[%AWVAT$�AS
=AR�AR �AQoAN��AL5?AI�FAH5?AG�PAG/AFE�ABĜA@A�A?7LA>5?A=C�A<Q�A<bA;O�A9�hA7ƨA5�mA3XA2v�A2��A2$�A1�-A1G�A/7LA.=qA.^5A.I�A-dZA,Q�A*  A(=qA't�A&�A&�A%�A#�;A!hsA A�AO�A��AdZA
=A5?A�AA�HA��A�jA�!A~�AȴA��A1A�^A��A?}A��A�uAv�A�
AA�A��A�A  A9XA5?AS�A
��A	K�A1'A��A�7AAZAx�A�+A�-AffAJA�#A\)A b@���@�hs@��@�v�@��@�M�@���@�@�l�@�$�@��`@��@�G�@�33@�V@�&�@��;@�;d@���@�-@�7@�A�@◍@���@��@�+@�(�@ڸR@�@�x�@���@أ�@�I�@�S�@��@Ձ@ԃ@ӥ�@�"�@�=q@��/@�bN@ϕ�@�$�@�E�@�n�@Η�@���@��H@θR@�E�@���@��@ˍP@�33@��y@�-@�V@��@��T@��@��@�(�@��@¸R@§�@���@�V@�r�@�(�@�b@�9X@��@��^@�=q@���@��h@��T@�V@��y@�33@��y@���@��w@�@�E�@��+@�7L@�&�@��h@�J@��@�V@��\@��+@��@���@���@��@��@� �@�V@�dZ@�+@��H@��R@���@��@�&�@��@�1'@��@�l�@��@���@���@��\@��\@��@��@�hs@�ff@�-@�J@��#@���@�p�@���@��@�A�@�(�@���@�t�@�S�@�C�@�o@��y@���@���@�o@�^5@��+@��@�~�@��@��T@�G�@���@��`@��j@�9X@�  @�Z@��@���@���@���@��y@�=q@��@�J@��#@�x�@��@��/@���@���@�r�@�Z@���@�
=@���@�ff@�@�x�@�%@���@���@�j@�Q�@��
@�\)@��H@�n�@�ȴ@�;d@�t�@��@�l�@��R@�ff@�^5@�J@��#@�@���@��7@�x�@�V@��@��`@���@�r�@���@���@�E�@�@��-@�G�@��@��/@���@��u@���@���@���@�E�@��@���@��/@��D@�Z@��@��m@��w@��w@��F@�|�@�;d@�ȴ@�ȴ@���@��!@�n�@�$�@�x�@�/@��j@�j@�A�@�A�@�(�@�b@�  @�b@��;@�l�@�ff@��#@��7@�O�@��@��9@��@�Z@�Q�@�(�@��m@���@���@�|�@�l�@�dZ@�\)@�S�@�C�@��@���@�ȴ@��!@���@��+@�V@��7@�/@�V@��@�Q�@���@�l�@�K�@�C�@�C�@�+@�
=@��y@��@��!@�}V@{ݘ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��TA��AԍPAӰ!A��TA���A���A���Aқ�AѰ!A���A�ȴAН�AГuAЇ+A�t�A�jA�\)A�K�A�A�A�9XA�"�A���A�=qA��A��;A��#A���A�AήA�v�A͏\A���A�~�A�~�Aʡ�A�G�A�bA�G�A˸RA�x�A�I�AʼjA�v�A�M�A�{Aɇ+A���A�1'A�I�A���A�ZA��Aŗ�AľwAď\A�dZA��A���A�ƨA���A��A�9XA�{A��wA��A�t�A�I�A�VA�bA��A��A��yA��`A�A�A�K�A�$�A�p�A���A��+A���A�%A�^5A�&�A���A��9A�|�A��A�`BA���A�v�A��A�Q�A�XA�A��+A�9XA��jA�ffA���A��A�  A�G�A�ZA�
=A��A���A�hsA��uA|ȴAyS�AvZAu�TAt9XAnĜAe�TAb��A]�
A[%AWVAT$�AS
=AR�AR �AQoAN��AL5?AI�FAH5?AG�PAG/AFE�ABĜA@A�A?7LA>5?A=C�A<Q�A<bA;O�A9�hA7ƨA5�mA3XA2v�A2��A2$�A1�-A1G�A/7LA.=qA.^5A.I�A-dZA,Q�A*  A(=qA't�A&�A&�A%�A#�;A!hsA A�AO�A��AdZA
=A5?A�AA�HA��A�jA�!A~�AȴA��A1A�^A��A?}A��A�uAv�A�
AA�A��A�A  A9XA5?AS�A
��A	K�A1'A��A�7AAZAx�A�+A�-AffAJA�#A\)A b@���@�hs@��@�v�@��@�M�@���@�@�l�@�$�@��`@��@�G�@�33@�V@�&�@��;@�;d@���@�-@�7@�A�@◍@���@��@�+@�(�@ڸR@�@�x�@���@أ�@�I�@�S�@��@Ձ@ԃ@ӥ�@�"�@�=q@��/@�bN@ϕ�@�$�@�E�@�n�@Η�@���@��H@θR@�E�@���@��@ˍP@�33@��y@�-@�V@��@��T@��@��@�(�@��@¸R@§�@���@�V@�r�@�(�@�b@�9X@��@��^@�=q@���@��h@��T@�V@��y@�33@��y@���@��w@�@�E�@��+@�7L@�&�@��h@�J@��@�V@��\@��+@��@���@���@��@��@� �@�V@�dZ@�+@��H@��R@���@��@�&�@��@�1'@��@�l�@��@���@���@��\@��\@��@��@�hs@�ff@�-@�J@��#@���@�p�@���@��@�A�@�(�@���@�t�@�S�@�C�@�o@��y@���@���@�o@�^5@��+@��@�~�@��@��T@�G�@���@��`@��j@�9X@�  @�Z@��@���@���@���@��y@�=q@��@�J@��#@�x�@��@��/@���@���@�r�@�Z@���@�
=@���@�ff@�@�x�@�%@���@���@�j@�Q�@��
@�\)@��H@�n�@�ȴ@�;d@�t�@��@�l�@��R@�ff@�^5@�J@��#@�@���@��7@�x�@�V@��@��`@���@�r�@���@���@�E�@�@��-@�G�@��@��/@���@��u@���@���@���@�E�@��@���@��/@��D@�Z@��@��m@��w@��w@��F@�|�@�;d@�ȴ@�ȴ@���@��!@�n�@�$�@�x�@�/@��j@�j@�A�@�A�@�(�@�b@�  @�b@��;@�l�@�ff@��#@��7@�O�@��@��9@��@�Z@�Q�@�(�@��m@���@���@�|�@�l�@�dZ@�\)@�S�@�C�@��@���@�ȴ@��!@���@��+@�V@��7@�/@�V@��@�Q�@���@�l�@�K�@�C�@�C�@�+@�
=@��y@��@��!@�}V@{ݘ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
jB
hsB
VB
E�B
33B
1'B
2-B
2-B
2-B
�B
+B
  B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�fB	�)B	��B	��B	�B
9XB
u�B
�BVB(�B7LB@�BC�BJ�BXBP�BO�Bv�Bt�BcTBcTBm�Bs�B~�B�oB��B�FBɺB�mB��BB%BDBbBhBuB�BuBuBoBoBhBhBbBbBoBoB�BoB\BbB�B�B\B1B��B��B�B�yB�B��B�qB�\B_;BI�BB�B=qB5?B,BhBB
�B
��B
�{B
n�B
]/B
P�B
7LB

=B	�mB	��B	�9B	�B	��B	r�B	@�B	-B	�B	1B��B��B�B�B�B�B�sB�BB�B�
B��B��B��B��BĜB��B�wB�wB�}B�wB�^BƨBɺB��B�}B��B�
B�B�
B�B��BȴB��B�B��B��BƨB�}B�}B�wB�dB�FB�B��B��B�oB�VB�PB�JB�\B�oB�oB�uB�uB�uB�{B��B�uB�uB��B��B��B��B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�!B�!B�B�B��B��B��B�B�!B�'B�!B�!B�'B�'B�-B�'B�!B�B�B�B�B�B�B�B�B�B�B�B�B�'B�3B�3B�9B�?B�RB�XB�jB�jB�qB�jB�jB�qB��BɺB��B��B��B�
B�B�)B�5B�HB�TB�`B�B�B�B�B�B�B��B�B�B�B��B��B��B��B��B��B��B��B��B��B		7B	uB	�B	&�B	'�B	+B	/B	5?B	<jB	C�B	F�B	>wB	:^B	33B	9XB	=qB	>wB	B�B	F�B	L�B	N�B	R�B	YB	\)B	aHB	cTB	cTB	gmB	k�B	iyB	cTB	bNB	cTB	dZB	ffB	gmB	hsB	gmB	hsB	jB	o�B	s�B	s�B	t�B	u�B	v�B	x�B	z�B	y�B	z�B	�7B	�JB	�PB	�VB	�\B	�\B	�\B	�hB	�uB	�uB	�uB	�oB	�oB	�uB	�oB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�!B	�3B	�?B	�FB	�FB	�LB	�RB	�XB	�XB	�XB	�XB	�XB	�jB	ÖB	��B	��B	ÖB	ƨB	ȴB	ɺB	ɺB	ǮB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�)B	�)B	�#B	�B	�B	�#B	�5B	�;B	�;B	�5B	�BB	�HB	�HB	�HB	�HB	�NB	�NB	�NB	�ZB	�`B	�mB	�yB	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
?B
�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B
jB
hsB
VB
E�B
33B
1'B
2-B
2-B
2-B
�B
+B
  B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�fB	�)B	��B	��B	�B
9XB
u�B
�BVB(�B7LB@�BC�BJ�BXBP�BO�Bv�Bt�BcTBcTBm�Bs�B~�B�oB��B�FBɺB�mB��BB%BDBbBhBuB�BuBuBoBoBhBhBbBbBoBoB�BoB\BbB�B�B\B1B��B��B�B�yB�B��B�qB�\B_;BI�BB�B=qB5?B,BhBB
�B
��B
�{B
n�B
]/B
P�B
7LB

=B	�mB	��B	�9B	�B	��B	r�B	@�B	-B	�B	1B��B��B�B�B�B�B�sB�BB�B�
B��B��B��B��BĜB��B�wB�wB�}B�wB�^BƨBɺB��B�}B��B�
B�B�
B�B��BȴB��B�B��B��BƨB�}B�}B�wB�dB�FB�B��B��B�oB�VB�PB�JB�\B�oB�oB�uB�uB�uB�{B��B�uB�uB��B��B��B��B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�!B�!B�B�B��B��B��B�B�!B�'B�!B�!B�'B�'B�-B�'B�!B�B�B�B�B�B�B�B�B�B�B�B�B�'B�3B�3B�9B�?B�RB�XB�jB�jB�qB�jB�jB�qB��BɺB��B��B��B�
B�B�)B�5B�HB�TB�`B�B�B�B�B�B�B��B�B�B�B��B��B��B��B��B��B��B��B��B��B		7B	uB	�B	&�B	'�B	+B	/B	5?B	<jB	C�B	F�B	>wB	:^B	33B	9XB	=qB	>wB	B�B	F�B	L�B	N�B	R�B	YB	\)B	aHB	cTB	cTB	gmB	k�B	iyB	cTB	bNB	cTB	dZB	ffB	gmB	hsB	gmB	hsB	jB	o�B	s�B	s�B	t�B	u�B	v�B	x�B	z�B	y�B	z�B	�7B	�JB	�PB	�VB	�\B	�\B	�\B	�hB	�uB	�uB	�uB	�oB	�oB	�uB	�oB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�!B	�3B	�?B	�FB	�FB	�LB	�RB	�XB	�XB	�XB	�XB	�XB	�jB	ÖB	��B	��B	ÖB	ƨB	ȴB	ɺB	ɺB	ǮB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�)B	�)B	�#B	�B	�B	�#B	�5B	�;B	�;B	�5B	�BB	�HB	�HB	�HB	�HB	�NB	�NB	�NB	�ZB	�`B	�mB	�yB	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
?B
�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.22 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190511                              AO  ARCAADJP                                                                    20181005190511    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190511  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190511  QCF$                G�O�G�O�G�O�8000            