CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:13Z creation      
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
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181005190513  20181005190513  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               "A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @׷��o��1   @׷�l� @1�^5?|��c�-1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      "A   A   B   @�ff@�  A   A!��A@  A`  A~ffA�33A�  A���A���A�  A�33A�  B   B  BffB  B   B(  B0  B7��B@  BH  BP  BX  B`  BhffBp  Bx  B��B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33C �C�C�C�C�C
  C  C  C  C  C  C  C  C  C�C  C �C"�C$  C&  C(  C*  C,  C.  C0  C2  C4�C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�fC�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��C��C�  C�  C�  C��3C�  C�  C�  C�  C��3C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C��3C�  D   D � D  Dy�D  D� D  D� D��Dy�D  D� D  D� D  D� D  D� D	  D	� D
  D
�fDfD� D  D� D  D� D  D� D  D� D��D� D  D�fD  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D��D � D!  D!� D"  D"� D#  D#y�D#��D$� D%  D%� D&  D&�fD'  D'� D(  D(� D)fD)� D*  D*� D+  D+y�D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D2��D3y�D3��D4y�D5  D5� D6  D6� D7  D7� D8  D8� D8��D9y�D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?y�D@  D@� DAfDA� DB  DB� DCfDC�fDC��DDy�DP�fDQ  DQ� DR  DR� DS  DSy�DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[�fD\  D\� D]fD]�fD^  D^� D_  D_� D_��D`� DafDa� Db  Db� Dc  Dc� Dc��Ddy�Dd��Dey�De��Df� Dg  Dg� Dh  Dhy�Dh��Di� DjfDj�fDk  Dk� Dl  Dl� Dl��Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dq��Dr� Ds  Ds� DtfDt� Du  Du� Dv  Dv� Dw  Dwy�DwٚDy�=D�;�D�M�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�p�@�
=A�A%�AC�Ac�A���A���A�A��\A\A�A���A�B �HB�HBG�B�HB �HB(�HB0�HB8z�B@�HBH�HBP�HBX�HB`�HBiG�Bp�HBx�HB�=qB�p�B�p�B�p�B�p�B�p�B�p�B�p�B�=qB�p�B�p�B�p�B�p�B���B�p�B�p�B�p�B�p�B�p�B�p�B�=qB�=qB�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B���C Q�CQ�CQ�CQ�CQ�C
8RC8RC8RC8RC8RC8RC8RC8RC8RCQ�C8RC Q�C"Q�C$8RC&8RC(8RC*8RC,8RC.8RC08RC28RC4Q�C68RC88RC:8RC<8RC>8RC@8RCB8RCD8RCF8RCH8RCJ8RCL8RCN8RCP8RCR8RCT8RCV8RCX8RCZ8RC\8RC^8RC`8RCb8RCd8RCf8RCh8RCj8RCl8RCn8RCp8RCr8RCt8RCv8RCx8RCz8RC|8RC~8RC�\C�)C�)C�)C�)C�)C�)C�)C�(�C�)C�)C�)C�\C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�\C�\C�)C�(�C�(�C�)C�)C�)C�\C�)C�)C�)C�)C�\C�\C�)C�(�C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�\C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�\C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�\C�)C�)C�)C�)C�)C�)C�)C�(�C�(�C�)C�)C�)C�(�C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�(�C�(�C�(�C�)C�\C�)D D �DD��DD�DD�D�D��DD�DD�DD�DD�D	D	�D
D
�zDzD�DD�DD�DD�DD�D�D�DD�zDD�DD�DzD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DzD�D �D �D!D!�D"D"�D#D#��D$�D$�D%D%�D&D&�zD'D'�D(D(�D)zD)�D*D*�D+D+��D,D,�D-D-�D.D.�D/D/�D0D0�D1D1�D2D2�D3�D3��D4�D4��D5D5�D6D6�D7D7�D8D8�D9�D9��D:D:�D;D;�D<D<�D=D=�D>D>�D?D?��D@D@�DAzDA�DBDB�DCzDC�zDD�DD��DP�zDQDQ�DRDR�DSDS��DTDT�DUDU�DVDV�DWDW�DXDX�DYDY�DZDZ�D[D[�zD\D\�D]zD]�zD^D^�D_D_�D`�D`�DazDa�DbDb�DcDc�Dd�Dd��De�De��Df�Df�DgDg�DhDh��Di�Di�DjzDj�zDkDk�DlDl�Dm�Dm�DnDn�DoDo�DpDp�DqDq�Dr�Dr�DsDs�DtzDt�DuDu�DvDv�DwDw��Dw�Dy�QD�B�D�T�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�1'A�-A�/A�33A�5?A�5?A�5?A�9XA�7LA�5?A�7LA�7LA�9XA�9XA�;dA�9XA�33A�9XA�9XA�A�A�;dA�33A�-A�7LA�-A��A�1A���A��#Aӣ�A�ƨA�jAЋDA��mAϣ�A�ffA���AΛ�A�VA��A͡�A͓uA��A��;A̍PA���A��Aʕ�A�(�A���A�ȴAɶFA�bA�|�A���A���Aǥ�A� �AƋDAƃA���A�VA�A���A���A���A��A���A� �A��A�5?A�VA�  A�M�A��uA�VA��FA��A�{A�"�A��A�|�A��`A��PA�dZA�5?A���A�~�A��-A���A�hsA�XA�-A�A�A�C�A�E�A�O�A��A���A�\)A���A�ZA��!A�z�A�bNA�t�A��\A�\)A���A��A�33A��PA�(�A���A�A�9XA�M�A~1A{�TAzz�Ay�Au��As�Aq�^An�Ah�Ab�+A[K�AV�!AS`BAQG�AO��AK�TAI`BAFffAEoAD��ADI�AB�RA@��A=�A<E�A:~�A9��A7�mA6�A61'A5�A4�9A2��A/C�A-�#A,�+A+��A*jA)�^A(~�A&��A&  A%VA"  A ^5A�hA�RAA
=Av�AA�7A\)AĜA��A�-A�uAv�AĜA�mA��AhsA  Ar�AXAVA�A9XAAXA
bNA
JA	��A	�A��AXA��AVA�mAr�A;dA%A�#A9XA�A%A�mA�AjA{AK�A�\A7LA&�A�HA�A-A�A�A��A��A�wA��A\)A �/@�\)@��@�{@�  @�p�@���@��@���@��@�7L@�ƨ@�+@���@�z�@�@�V@蛦@�@��@���@��@�@�r�@�@���@�/@�z�@�o@�!@���@߮@܃@�+@�M�@���@ّh@���@�|�@���@��@�Z@�t�@�
=@�o@�o@�ȴ@�~�@Ѳ-@ѩ�@щ7@�&�@���@���@��;@�+@Η�@Ώ\@��@θR@Η�@���@�n�@�bN@��@�M�@�S�@�~�@š�@��@�z�@�z�@ă@ě�@ģ�@�hs@�@�G�@�Q�@�$�@�hs@�7L@�/@�/@�X@�V@�Z@���@��@�%@�E�@�~�@�$�@��T@��7@�O�@���@�I�@��@��!@��+@�n�@�ff@��@��@�(�@�K�@�K�@��@���@���@�n�@�{@��#@��-@��h@��@��@�%@��`@�9X@���@��@�+@�+@�o@��!@��#@��@���@��9@�Z@�  @��;@���@�\)@�+@�v�@�?}@���@��@�"�@�v�@��@�n�@�v�@�-@���@�/@��@��@��y@��\@�^5@�-@�@�J@�@���@�hs@�%@��u@��
@�|�@�S�@�S�@�o@�@���@��+@��/@���@�@��`@�z�@�j@�9X@�9X@�(�@�1'@���@�C�@�"�@�o@��y@�E�@�{@�@��@��T@��-@�7L@���@��9@�9X@��@�K�@�;d@�@�5?@���@���@��-@�V@���@��j@�9X@���@�S�@�o@��@���@�n�@��^@��/@��u@�9X@��m@��F@�S�@�
=@��+@�J@��@��-@��@�`B@�X@�7L@���@�9X@�1'@�(�@��
@��
@���@�K�@��@�
=@��R@���@�v�@�{@��#@���@��7@�O�@�&�@���@��/@��j@�.�@�YK@s1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�1'A�-A�/A�33A�5?A�5?A�5?A�9XA�7LA�5?A�7LA�7LA�9XA�9XA�;dA�9XA�33A�9XA�9XA�A�A�;dA�33A�-A�7LA�-A��A�1A���A��#Aӣ�A�ƨA�jAЋDA��mAϣ�A�ffA���AΛ�A�VA��A͡�A͓uA��A��;A̍PA���A��Aʕ�A�(�A���A�ȴAɶFA�bA�|�A���A���Aǥ�A� �AƋDAƃA���A�VA�A���A���A���A��A���A� �A��A�5?A�VA�  A�M�A��uA�VA��FA��A�{A�"�A��A�|�A��`A��PA�dZA�5?A���A�~�A��-A���A�hsA�XA�-A�A�A�C�A�E�A�O�A��A���A�\)A���A�ZA��!A�z�A�bNA�t�A��\A�\)A���A��A�33A��PA�(�A���A�A�9XA�M�A~1A{�TAzz�Ay�Au��As�Aq�^An�Ah�Ab�+A[K�AV�!AS`BAQG�AO��AK�TAI`BAFffAEoAD��ADI�AB�RA@��A=�A<E�A:~�A9��A7�mA6�A61'A5�A4�9A2��A/C�A-�#A,�+A+��A*jA)�^A(~�A&��A&  A%VA"  A ^5A�hA�RAA
=Av�AA�7A\)AĜA��A�-A�uAv�AĜA�mA��AhsA  Ar�AXAVA�A9XAAXA
bNA
JA	��A	�A��AXA��AVA�mAr�A;dA%A�#A9XA�A%A�mA�AjA{AK�A�\A7LA&�A�HA�A-A�A�A��A��A�wA��A\)A �/@�\)@��@�{@�  @�p�@���@��@���@��@�7L@�ƨ@�+@���@�z�@�@�V@蛦@�@��@���@��@�@�r�@�@���@�/@�z�@�o@�!@���@߮@܃@�+@�M�@���@ّh@���@�|�@���@��@�Z@�t�@�
=@�o@�o@�ȴ@�~�@Ѳ-@ѩ�@щ7@�&�@���@���@��;@�+@Η�@Ώ\@��@θR@Η�@���@�n�@�bN@��@�M�@�S�@�~�@š�@��@�z�@�z�@ă@ě�@ģ�@�hs@�@�G�@�Q�@�$�@�hs@�7L@�/@�/@�X@�V@�Z@���@��@�%@�E�@�~�@�$�@��T@��7@�O�@���@�I�@��@��!@��+@�n�@�ff@��@��@�(�@�K�@�K�@��@���@���@�n�@�{@��#@��-@��h@��@��@�%@��`@�9X@���@��@�+@�+@�o@��!@��#@��@���@��9@�Z@�  @��;@���@�\)@�+@�v�@�?}@���@��@�"�@�v�@��@�n�@�v�@�-@���@�/@��@��@��y@��\@�^5@�-@�@�J@�@���@�hs@�%@��u@��
@�|�@�S�@�S�@�o@�@���@��+@��/@���@�@��`@�z�@�j@�9X@�9X@�(�@�1'@���@�C�@�"�@�o@��y@�E�@�{@�@��@��T@��-@�7L@���@��9@�9X@��@�K�@�;d@�@�5?@���@���@��-@�V@���@��j@�9X@���@�S�@�o@��@���@�n�@��^@��/@��u@�9X@��m@��F@�S�@�
=@��+@�J@��@��-@��@�`B@�X@�7L@���@�9X@�1'@�(�@��
@��
@���@�K�@��@�
=@��R@���@�v�@�{@��#@���@��7@�O�@�&�@���@��/@��j@�.�@�YK@s1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
n�B
`BB
E�B
:^B
0!B
+B
,B
1'B
J�B
ZB
iyB
}�B
�B
��B
��B
�B
��B
�sB
��BB
=BDBJB�B$�B7LBD�BK�BW
BjB�7B�!B��B�/B�B1B�B,BB�B?}B6FBC�BD�BH�BI�BH�B$�B�B�B'�B6FB>wB7LB"�B�B�B{B\B%B��B�NBɺB�}B�RB�9B��B��B�BXB�BB
�B
��B
�-B
��B
��B
�PB
�B
p�B
gmB
_;B
VB
B�B
!�B
{B
1B
  B	��B	�ZB	�
B	��B	ĜB	��B	��B	ĜB	�B	�1B	`BB	5?B	�B	PB	B	  B��B�B�ZB�BB�5B�)B�B��BƨB��B�dB�LB�3B�'B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B��B��B�B�3BĜB�B��B�)B�B�fB�NB�ZB��B	�B	,B	;dB	=qB	>wB	A�B	J�B	P�B	R�B	T�B	W
B	W
B	W
B	VB	Q�B	O�B	R�B	N�B	J�B	C�B	I�B	J�B	H�B	H�B	C�B	A�B	>wB	49B	�B	hB	�B	�B	�B	�B	�B	 �B	&�B	/B	7LB	>wB	B�B	G�B	O�B	M�B	G�B	@�B	<jB	:^B	9XB	9XB	:^B	<jB	=qB	=qB	;dB	;dB	<jB	?}B	A�B	A�B	F�B	Q�B	VB	T�B	S�B	T�B	]/B	]/B	^5B	]/B	]/B	`BB	bNB	aHB	aHB	_;B	ZB	VB	VB	Q�B	N�B	L�B	L�B	N�B	P�B	Q�B	R�B	VB	]/B	hsB	iyB	gmB	dZB	m�B	n�B	o�B	p�B	q�B	w�B	v�B	v�B	x�B	�B	�bB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�'B	�9B	�9B	�?B	�9B	�XB	�qB	�wB	�wB	�}B	��B	B	B	ƨB	ǮB	ȴB	ȴB	ȴB	ǮB	ƨB	ŢB	ÖB	��B	��B	��B	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B%�B	�5B	�/B	�#B	�#B	�#B	�)B	�/B	�5B	�;B	�HB	�HB	�HB	�HB	�BB	�HB	�NB	�NB	�HB	�HB	�NB	�TB	�`B	�`B	�mB	�yB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
1B
1B
	7B
	7B
DB
�B
�B
/�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222422222222222222222222222222222222222222222222222222222222222222222222222222222222222 B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
n�B
`BB
E�B
:^B
0!B
+B
,B
1'B
J�B
ZB
iyB
}�B
�B
��B
��B
�B
��B
�sB
��BB
=BDBJB�B$�B7LBD�BK�BW
BjB�7B�!B��B�/B�B1B�B,BB�B?}B6FBC�BD�BH�BI�BH�B$�B�B�B'�B6FB>wB7LB"�B�B�B{B\B%B��B�NBɺB�}B�RB�9B��B��B�BXB�BB
�B
��B
�-B
��B
��B
�PB
�B
p�B
gmB
_;B
VB
B�B
!�B
{B
1B
  B	��B	�ZB	�
B	��B	ĜB	��B	��B	ĜB	�B	�1B	`BB	5?B	�B	PB	B	  B��B�B�ZB�BB�5B�)B�B��BƨB��B�dB�LB�3B�'B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B��B��B�B�3BĜB�B��B�)B�B�fB�NB�ZB��B	�B	,B	;dB	=qB	>wB	A�B	J�B	P�B	R�B	T�B	W
B	W
B	W
B	VB	Q�B	O�B	R�B	N�B	J�B	C�B	I�B	J�B	H�B	H�B	C�B	A�B	>wB	49B	�B	hB	�B	�B	�B	�B	�B	 �B	&�B	/B	7LB	>wB	B�B	G�B	O�B	M�B	G�B	@�B	<jB	:^B	9XB	9XB	:^B	<jB	=qB	=qB	;dB	;dB	<jB	?}B	A�B	A�B	F�B	Q�B	VB	T�B	S�B	T�B	]/B	]/B	^5B	]/B	]/B	`BB	bNB	aHB	aHB	_;B	ZB	VB	VB	Q�B	N�B	L�B	L�B	N�B	P�B	Q�B	R�B	VB	]/B	hsB	iyB	gmB	dZB	m�B	n�B	o�B	p�B	q�B	w�B	v�B	v�B	x�B	�B	�bB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�'B	�9B	�9B	�?B	�9B	�XB	�qB	�wB	�wB	�}B	��B	B	B	ƨB	ǮB	ȴB	ȴB	ȴB	ǮB	ƨB	ŢB	ÖB	��B	��B	��B	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B%�B	�5B	�/B	�#B	�#B	�#B	�)B	�/B	�5B	�;B	�HB	�HB	�HB	�HB	�BB	�HB	�NB	�NB	�HB	�HB	�NB	�TB	�`B	�`B	�mB	�yB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
1B
1B
	7B
	7B
DB
�B
�B
/�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222422222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.22 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190513                              AO  ARCAADJP                                                                    20181005190513    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190513  QCP$                G�O�G�O�G�O�F03E            AO  ARGQQCPL                                                                    20181005190513  QCF$                G�O�G�O�G�O�8000            