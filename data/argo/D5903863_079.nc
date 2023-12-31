CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       d2014-07-21T23:39:35Z creation; 2014-07-21T23:39:35Z updated; 2015-09-28T12:13:27Z converted from 3.0   
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7    PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7`   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8    PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8$   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8D   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8d   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~       axis      T           8h   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8p   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~            8t   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8|   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        <  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     <  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     <  _�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     <  n�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~8   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     <  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �D   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     <  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     <  �P   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     <  �\   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Ƙ   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     <  �h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ٤   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �    HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �P   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �`   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �d   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �t   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �x   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �|   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721233935  20170523133343  5903863 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               OA   AO  4298_0127_079                   2C  D   NAVIS_A                         0127                            120111                          863 @��a���1   @��bA;�@6)�^5?}�c��
=p�1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      OA   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/�fD0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @o\(@�G�@�G�A��A8��AX��Ax��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B(�B(�B(�B(�B&(�B.(�B6(�B>(�BF(�BN(�BV(�B^(�Bf(�Bn(�Bv(�B~(�B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{C�=C�=C�=C�=C	�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C!�=C#�=C%�=C'�=C)�=C+�=C-�=C/�=C1�=C3�=C5�=C7�=C9�=C;�=C=�=C?�=CA�=CC�=CE�=CG�=CI�=CK�=CM�=CO�=CQ�=CS�=CU�=CW�=CY�=C[�=C]�=C_�=Ca�=Cc�=Ce�=Cg�=Ci�=Ck�=Cm�=Co�=Cq�=Cs�=Cu�=Cw�=Cy�=C{�=C}�=C�=C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D b�D �Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�D	b�D	�D
b�D
�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�D b�D �D!b�D!�D"b�D"�D#b�D#�D$b�D$�D%b�D%�D&b�D&�D'b�D'�D(b�D(�D)b�D)�D*b�D*�D+b�D+�D,b�D,�D-b�D-�D.b�D.�D/h�D/�D0b�D0�D1b�D1�D2b�D2�D3b�D3�D4b�D4�D5b�D5�D6b�D6�D7b�D7�D8b�D8�D9b�D9�D:b�D:�D;b�D;�D<b�D<�D=b�D=�D>b�D>�D?b�D?�D@b�D@�DAb�DA�DBb�DB�DCb�DC�DDb�DD�DEb�DE�DFb�DF�DGb�DG�DHb�DH�DIb�DI�DJb�DJ�DKb�DK�DLb�DL�DMb�DM�DNb�DN�DOb�DO�DPb�DP�DQb�DQ�DRb�DR�DSb�DS�DTb�DT�DUb�DU�DVb�DV�DWb�DW�DXb�DX�DYb�DY�DZb�DZ�D[b�D[�D\b�D\�D]b�D]�D^b�D^�D_b�D_�D`b�D`�Dab�Da�Dbb�Db�Dcb�Dc�Ddb�Dd�Deb�De�Dfb�Df�Dgb�Dg�Dhb�Dh�Dib�Di�Djb�Dj�Dkb�Dk�Dlb�Dl�Dmb�Dm�Dnb�Dn�Dob�Do�Dpb�Dp�Dqb�Dq�Drb�Dr�Dsb�Ds�Dtb�Dt�Dub�Du�Dvb�Dv�Dwb�Dw�Dxb�Dx�Dyb�Dy�Dzb�Dz�D{b�D{�D|b�D|�D}b�D}�D~b�D~�Db�D�D�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�t{D��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD±HD��HD�1HD�qHDñHD��HD�1HD�qHDıHD��HD�1HD�qHDűHD��HD�1HD�qHDƱHD��HD�1HD�qHDǱHD��HD�1HD�qHDȱHD��HD�1HD�qHDɱHD��HD�1HD�qHDʱHD��HD�1HD�qHD˱HD��HD�1HD�qHḎHD��HD�1HD�qHDͱHD��HD�1HD�qHDαHD��HD�1HD�qHDϱHD��HD�1HD�qHDбHD��HD�1HD�qHDѱHD��HD�1HD�qHDұHD��HD�1HD�qHDӱHD��HD�1HD�qHDԱHD��HD�1HD�qHDձHD��HD�1HD�qHDֱHD��HD�1HD�qHDױHD��HD�1HD�qHDرHD��HD�1HD�qHDٱHD��HD�1HD�qHDڱHD��HD�1HD�qHD۱HD��HD�1HD�qHDܱHD��HD�1HD�qHDݱHD��HD�1HD�qHDޱHD��HD�1HD�qHD߱HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD��HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��H111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A��hA�t�A�ZA�7LA���A�O�A��A�A���A���A��A��TA���A��!A�A�A���A���A�M�A��A���A���A��7A�r�A�M�A�1'A��A�VA��mA���A���A��^A���A��hA��A�n�A�bNA�VA�O�A�E�A�1'A�+A�&�A��A��A��A�1A��#A��7A�O�A� �A�VA��A���A��hA���A��TA���A�;dA��`A�ffA�bA���A���A�1'A��+A��+A���A�^5A��A���A���A�t�A�1A�t�A��^A�?}A�bA�?}A�G�A���A���A���A��hA�O�A��DA�p�A���A�&�A�bA�  A�l�A�I�A��/A�A���A��A~��A{�Ax^5AvAq&�Ao�Ao�An5?Ak|�Ai��AhAfZAb��AZ��AX�RAXA�AW%AT�HAS7LAR~�AR �AQ��AQ��AP�HAO��ANbNAM�AKAK�AJr�AI"�AG�mAF��AE��AC�
ACO�ABZAAA@I�A?hsA>bA=7LA;�^A9�TA8�!A7/A6$�A5+A4ZA3S�A2�+A1�^A1VA0n�A.~�A-dZA,�A,v�A+ƨA*�A(�HA'�-A&jA%��A%&�A$�jA#A#+A!�A�^AXA�A%A�yA�RA�jA��A��A��A�^At�AbNA�;A�#AffAA�AJA�^AĜA+A�A�PA
�+A	t�A��AQ�A��A��AĜA(�A�wAl�A��A�HA��AVAx�A�AXA V@���@�o@�5?@�G�@�Ĝ@�A�@��@���@��9@�%@���@��
@홚@��@��@�|�@��@�~�@�X@���@�l�@�
=@�h@�bN@���@ް!@�%@��@�dZ@�E�@ٺ^@�7L@�9X@ו�@�l�@ְ!@�5?@���@���@�`B@�7L@ԓu@�dZ@�K�@ЋD@ͩ�@̓u@�1@���@ʧ�@���@��@�Z@î@�"�@�@��H@��@�G�@�V@��`@��w@���@��#@��m@���@���@��-@�p�@�Q�@�|�@�
=@��@��H@�ȴ@��\@�E�@���@���@���@��@��w@�ff@��@�p�@�Ĝ@��@���@�dZ@�r�@�7L@��y@���@�ƨ@� �@�A�@�I�@�Z@�z�@�Z@�  @��
@��F@�+@��R@�V@�p�@���@�I�@���@��P@�ȴ@��!@���@�~�@���@��@��/@��@�j@��
@���@�K�@���@�V@��@�@��@���@�x�@��u@�A�@�bN@���@�9X@��w@��P@�|�@�K�@�o@���@��\@�^5@�{@��^@�hs@�O�@�&�@��@�Ĝ@��j@��@�j@�1'@�1@��m@��w@�|�@�dZ@�\)@�K�@�33@��@�@��y@���@���@�-@���@��;@���@�t�@�\)@�C�@�+@��!@�n�@�@���@��^@��@�O�@�%@���@�9X@��m@��@�l�@�C�@��@�33@�;d@�
=@���@���@�\)@�@�hs@�`B@�`B@�`B@�7L@�7L@�G�@�p�@��@�`B@�X@��@�@��-@���@��@�x�@�X@�/@��@�%@��9@� �@��
@�dZ@��\@�E�@�$�@��@��T@��#@��^@��@�?}@��@���@��@�Ĝ@�j@� �@�  @���@���@��
@��F@���@�t�@�S�@��@��@�ȴ@��R@��!@���@�V@��@�{@��@��7@�X@�%@��`@��9@���@��u@�Z@��
@���@�S�@��y@���@�~�@�M�@�@��@��#@��^@��h@�`B@�X@�O�@�O�@�O�@�?}@��@�r�@�Z@�(�@�  @�P@~�@~$�@~$�@~{@~@}@}p�@|j@{�F@{�m@{��@z��@y��@y��@y�@xĜ@xbN@x �@w�@w��@w�@v�R@v��@vE�@v@u��@uO�@u�@t�D@t�@s�m@s�m@s�
@sS�@rn�@q��@q�@q�#@q��@qX@q&�@p�`@pĜ@p�u@pr�@pA�@pb@p  @o�@oK�@n5?@m�h@mV@l�@l�j@l�D@lz�@lZ@l1@k�F@k��@kt�@ko@j��@j-@i�7@hQ�@g|�@gl�@gl�@gl�@gl�@g\)@g+@g+@g+@g�@f$�@e�T@e�-@e�h@eV@d�@d9X@c�
@c��@c�@co@b��@b��@b~�@bn�@bn�@bn�@b�@a��@a7L@`��@`1'@_�@^�+@]��@\�@[C�@Y��@Y�7@Yx�@YG�@X��@XĜ@XQ�@W�;@W�P@V��@V5?@U�T@U�@UV@T��@S��@S33@R��@R��@Rn�@Q��@Q&�@P�9@P�u@Pr�@PbN@P1'@Pb@O��@Ol�@N�y@M�T@M?}@L��@L�j@L��@Lz�@Lj@L9X@K��@K��@K@J�H@J��@Jn�@Ix�@Ihs@Ihs@Ihs@IX@IG�@I&�@H�9@Hr�@H �@G�;@G�@G�P@Gl�@G\)@G;d@G
=@Fȴ@F��@Fff@FE�@F$�@E�@E@E�h@E�h@E`B@E?}@Dj@C33@B�\@B�@A�@A��@AX@A%@@bN@?��@?l�@?K�@?K�@?;d@?;d@?K�@?;d@?�@>��@=��@=�@>{@=�T@=�@=�T@=��@=�h@=p�@<�@<�@<Z@<�@;��@;"�@;o@:�H@:�H@:��@:n�@:-@9�#@9hs@9%@8�`@8Ĝ@8Ĝ@8�9@8�u@8 �@8  @7�@7�;@7�;@7��@7K�@7
=@6�y@6ȴ@6ȴ@6ȴ@6ȴ@6ff@6@5�h@4�/@4z�@49X@3�m@3ƨ@3"�@3@2�@2��@2~�@2n�@2M�@2=q@2�@1��@1�@1��@1X@0��@0bN@0 �@0b@/�@/��@/�@/�@/��@/�@.�y@.�@.��@.��@.ff@.E�@.@-�T@-@-�@-?}@,�@,��@,�D@,9X@,(�@+��@+��@*�@*n�@)��@)��@)�^@)��@)&�@(bN@(A�@( �@(  @'�;@'��@'l�@'+@&�@&ȴ@&�R@&��@&��@&��@&�+@&�+@&v�@&V@&@%?}@$�@$�/@$�j@$z�@$j@$I�@$�@$1@$1@$1@#��@#��@#��@#��@#�
@#t�@#C�@#33@#"�@#o@#@"�@"��@"�!@"��@"~�@"~�@"~�@"n�@"^5@"M�@!��@!�^@!x�@!X@!&�@ �`@ �9@ r�@ 1'@��@K�@
=@
=@�y@�y@�y@�y@�+@$�@��@?}@V@��@�j@�m@�@33@dZ@33@��@n�@-@�@��@��@��@�7@hs@7L@�@��@ �@�w@l�@+@+@+@;d@+@+@+@�@�@
=@��@V@{@�-@V@��@(�@�
@��@t�@��@n�@-@�@�@J@��@�#@�^@��@��@��@�7@%@��@Ĝ@��@�@1'@  @�@�;@�w@|�@l�@l�@K�@+@+@+@+@�@�@�@��@�y@�R@�+@V@5?@�@��@��@�@`B@`B@O�@/@�@�@��@j@(�@�@1@��@�
@t�@S�@C�@o@
�!@
��@
�\@
M�@
-@	�@	��@	�^@	�^111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A���A��hA�t�A�ZA�7LA���A�O�A��A�A���A���A��A��TA���A��!A�A�A���A���A�M�A��A���A���A��7A�r�A�M�A�1'A��A�VA��mA���A���A��^A���A��hA��A�n�A�bNA�VA�O�A�E�A�1'A�+A�&�A��A��A��A�1A��#A��7A�O�A� �A�VA��A���A��hA���A��TA���A�;dA��`A�ffA�bA���A���A�1'A��+A��+A���A�^5A��A���A���A�t�A�1A�t�A��^A�?}A�bA�?}A�G�A���A���A���A��hA�O�A��DA�p�A���A�&�A�bA�  A�l�A�I�A��/A�A���A��A~��A{�Ax^5AvAq&�Ao�Ao�An5?Ak|�Ai��AhAfZAb��AZ��AX�RAXA�AW%AT�HAS7LAR~�AR �AQ��AQ��AP�HAO��ANbNAM�AKAK�AJr�AI"�AG�mAF��AE��AC�
ACO�ABZAAA@I�A?hsA>bA=7LA;�^A9�TA8�!A7/A6$�A5+A4ZA3S�A2�+A1�^A1VA0n�A.~�A-dZA,�A,v�A+ƨA*�A(�HA'�-A&jA%��A%&�A$�jA#A#+A!�A�^AXA�A%A�yA�RA�jA��A��A��A�^At�AbNA�;A�#AffAA�AJA�^AĜA+A�A�PA
�+A	t�A��AQ�A��A��AĜA(�A�wAl�A��A�HA��AVAx�A�AXA V@���@�o@�5?@�G�@�Ĝ@�A�@��@���@��9@�%@���@��
@홚@��@��@�|�@��@�~�@�X@���@�l�@�
=@�h@�bN@���@ް!@�%@��@�dZ@�E�@ٺ^@�7L@�9X@ו�@�l�@ְ!@�5?@���@���@�`B@�7L@ԓu@�dZ@�K�@ЋD@ͩ�@̓u@�1@���@ʧ�@���@��@�Z@î@�"�@�@��H@��@�G�@�V@��`@��w@���@��#@��m@���@���@��-@�p�@�Q�@�|�@�
=@��@��H@�ȴ@��\@�E�@���@���@���@��@��w@�ff@��@�p�@�Ĝ@��@���@�dZ@�r�@�7L@��y@���@�ƨ@� �@�A�@�I�@�Z@�z�@�Z@�  @��
@��F@�+@��R@�V@�p�@���@�I�@���@��P@�ȴ@��!@���@�~�@���@��@��/@��@�j@��
@���@�K�@���@�V@��@�@��@���@�x�@��u@�A�@�bN@���@�9X@��w@��P@�|�@�K�@�o@���@��\@�^5@�{@��^@�hs@�O�@�&�@��@�Ĝ@��j@��@�j@�1'@�1@��m@��w@�|�@�dZ@�\)@�K�@�33@��@�@��y@���@���@�-@���@��;@���@�t�@�\)@�C�@�+@��!@�n�@�@���@��^@��@�O�@�%@���@�9X@��m@��@�l�@�C�@��@�33@�;d@�
=@���@���@�\)@�@�hs@�`B@�`B@�`B@�7L@�7L@�G�@�p�@��@�`B@�X@��@�@��-@���@��@�x�@�X@�/@��@�%@��9@� �@��
@�dZ@��\@�E�@�$�@��@��T@��#@��^@��@�?}@��@���@��@�Ĝ@�j@� �@�  @���@���@��
@��F@���@�t�@�S�@��@��@�ȴ@��R@��!@���@�V@��@�{@��@��7@�X@�%@��`@��9@���@��u@�Z@��
@���@�S�@��y@���@�~�@�M�@�@��@��#@��^@��h@�`B@�X@�O�@�O�@�O�@�?}@��@�r�@�Z@�(�@�  @�P@~�@~$�@~$�@~{@~@}@}p�@|j@{�F@{�m@{��@z��@y��@y��@y�@xĜ@xbN@x �@w�@w��@w�@v�R@v��@vE�@v@u��@uO�@u�@t�D@t�@s�m@s�m@s�
@sS�@rn�@q��@q�@q�#@q��@qX@q&�@p�`@pĜ@p�u@pr�@pA�@pb@p  @o�@oK�@n5?@m�h@mV@l�@l�j@l�D@lz�@lZ@l1@k�F@k��@kt�@ko@j��@j-@i�7@hQ�@g|�@gl�@gl�@gl�@gl�@g\)@g+@g+@g+@g�@f$�@e�T@e�-@e�h@eV@d�@d9X@c�
@c��@c�@co@b��@b��@b~�@bn�@bn�@bn�@b�@a��@a7L@`��@`1'@_�@^�+@]��@\�@[C�@Y��@Y�7@Yx�@YG�@X��@XĜ@XQ�@W�;@W�P@V��@V5?@U�T@U�@UV@T��@S��@S33@R��@R��@Rn�@Q��@Q&�@P�9@P�u@Pr�@PbN@P1'@Pb@O��@Ol�@N�y@M�T@M?}@L��@L�j@L��@Lz�@Lj@L9X@K��@K��@K@J�H@J��@Jn�@Ix�@Ihs@Ihs@Ihs@IX@IG�@I&�@H�9@Hr�@H �@G�;@G�@G�P@Gl�@G\)@G;d@G
=@Fȴ@F��@Fff@FE�@F$�@E�@E@E�h@E�h@E`B@E?}@Dj@C33@B�\@B�@A�@A��@AX@A%@@bN@?��@?l�@?K�@?K�@?;d@?;d@?K�@?;d@?�@>��@=��@=�@>{@=�T@=�@=�T@=��@=�h@=p�@<�@<�@<Z@<�@;��@;"�@;o@:�H@:�H@:��@:n�@:-@9�#@9hs@9%@8�`@8Ĝ@8Ĝ@8�9@8�u@8 �@8  @7�@7�;@7�;@7��@7K�@7
=@6�y@6ȴ@6ȴ@6ȴ@6ȴ@6ff@6@5�h@4�/@4z�@49X@3�m@3ƨ@3"�@3@2�@2��@2~�@2n�@2M�@2=q@2�@1��@1�@1��@1X@0��@0bN@0 �@0b@/�@/��@/�@/�@/��@/�@.�y@.�@.��@.��@.ff@.E�@.@-�T@-@-�@-?}@,�@,��@,�D@,9X@,(�@+��@+��@*�@*n�@)��@)��@)�^@)��@)&�@(bN@(A�@( �@(  @'�;@'��@'l�@'+@&�@&ȴ@&�R@&��@&��@&��@&�+@&�+@&v�@&V@&@%?}@$�@$�/@$�j@$z�@$j@$I�@$�@$1@$1@$1@#��@#��@#��@#��@#�
@#t�@#C�@#33@#"�@#o@#@"�@"��@"�!@"��@"~�@"~�@"~�@"n�@"^5@"M�@!��@!�^@!x�@!X@!&�@ �`@ �9@ r�@ 1'@��@K�@
=@
=@�y@�y@�y@�y@�+@$�@��@?}@V@��@�j@�m@�@33@dZ@33@��@n�@-@�@��@��@��@�7@hs@7L@�@��@ �@�w@l�@+@+@+@;d@+@+@+@�@�@
=@��@V@{@�-@V@��@(�@�
@��@t�@��@n�@-@�@�@J@��@�#@�^@��@��@��@�7@%@��@Ĝ@��@�@1'@  @�@�;@�w@|�@l�@l�@K�@+@+@+@+@�@�@�@��@�y@�R@�+@V@5?@�@��@��@�@`B@`B@O�@/@�@�@��@j@(�@�@1@��@�
@t�@S�@C�@o@
�!@
��@
�\@
M�@
-@	�@	��@	�^@	�^111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBT�BVBVBYB]/B^5B^5BcTBiyBl�Bm�Bm�Bn�Bn�Bo�Bp�Br�Bz�B�B��B��B�B�FB�RB�RB�XB�dB�^B�XB�RB�FB�9B�-B�'B�!B�B�B�B�!B�3B�?B�?B�?B�FB�?B�?B�?B�?B�9B�-B�!B�!B�B��B��B��B�hB�Bn�BbNBXB?}B.B&�BJB��B��B�BBȴBB��B�qB�B��B�bB�+B|�Bp�BffB`BBL�B)�B�B1B�`B��BȴB�XB��Bk�BN�B8RB"�BB
�5B
�-B
�oB
l�B
N�B
1'B
�B
  B	�B	��B	ÖB	�wB	�?B	��B	��B	�7B	y�B	bNB	9XB	.B	+B	$�B	�B	{B	oB	hB	\B	VB	DB	%B	B��B��B��B�B�B�yB�fB�HB�)B�B�B��B��B��BȴBĜB�}B�XB�FB�'B�B�B��B��B��B��B��B��B��B�{B�oB�hB�VB�=B�%B�B�B�B~�B~�B� B|�Bx�Bs�Bp�Bo�Bo�Bo�Bm�BffB^5B\)BYBYBW
BR�BN�BJ�BK�BJ�BI�BG�BD�BA�B?}B=qB;dB:^B9XB8RB8RB8RB5?B49B33B5?B49B5?B5?B49B2-B1'B.B,B,B,B,B+B+B+B)�B'�B&�B'�B'�B'�B(�B(�B'�B'�B)�B)�B)�B+B+B)�B+B.B/B49B49B33B1'B1'B2-B2-B49B6FB6FB7LB8RB9XBB�BF�BH�BL�BN�BS�BR�BM�BJ�BJ�BN�BXBm�BdZB`BB^5B]/BZBVBM�BI�BL�BQ�BR�BR�BQ�BO�BR�BXB[#B\)B[#B\)B_;B`BB`BBaHBbNBdZBe`BffBhsBm�Bo�Bv�Bx�B{�B� B�B�B�=B�oB��B��B��B�B�3B�wB��BÖBȴB��B��B�B�B�B�TB�fB�B��B��B��B��B��B��B��B��B��B��B	  B	  B	B	B	B	%B		7B	DB	PB	PB	VB	bB	\B	uB	�B	�B	 �B	%�B	&�B	&�B	'�B	'�B	(�B	)�B	+B	,B	.B	0!B	2-B	33B	6FB	:^B	=qB	=qB	>wB	A�B	E�B	G�B	H�B	H�B	I�B	K�B	L�B	L�B	L�B	M�B	M�B	N�B	N�B	O�B	R�B	YB	`BB	bNB	bNB	bNB	bNB	bNB	e`B	gmB	jB	m�B	n�B	p�B	r�B	s�B	t�B	u�B	w�B	y�B	z�B	|�B	}�B	� B	�B	�B	�B	�B	�7B	�+B	�1B	�7B	�7B	�=B	�=B	�JB	�VB	�bB	�oB	�oB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�3B	�3B	�9B	�FB	�RB	�RB	�RB	�RB	�XB	�^B	�^B	�^B	�^B	�^B	�^B	�dB	�dB	�jB	�wB	�}B	��B	��B	��B	B	B	ĜB	ƨB	ƨB	ǮB	ǮB	ǮB	ǮB	ȴB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�#B	�#B	�#B	�)B	�5B	�;B	�;B	�BB	�HB	�TB	�`B	�`B	�fB	�fB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
1B
1B

=B
DB
DB
DB
DB
DB
DB
JB
JB
JB
JB
PB
VB
VB
VB
VB
\B
\B
\B
bB
bB
bB
bB
bB
hB
hB
hB
hB
hB
oB
oB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
!�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
+B
,B
,B
,B
,B
,B
,B
,B
,B
,B
,B
,B
,B
,B
,B
,B
,B
-B
-B
-B
-B
.B
.B
.B
.B
/B
/B
0!B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
33B
33B
7LB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
@�B
@�B
A�B
A�B
A�B
B�B
A�B
A�B
B�B
C�B
C�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
S�B
T�B
T�B
T�B
T�B
T�B
VB
VB
VB
VB
W
B
W
B
W
B
W
B
XB
XB
XB
XB
XB
XB
YB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
\)B
]/B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
_;B
_;B
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
e`B
dZB
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
iyB
jB
jB
k�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
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
r�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BU%BV8BV`BY�B]�B^�B`8BeJBj�BmBm�Bm�Bn�Bn�BpBqgBt�B|�B��B�CB�cB�bB��B��B��B�&B�B��B��B�5B��B��B�vB��B��B��B��B��B��B�uB��B��B��B��B��B�vB�vB��B�.B��B�9B�B�}B�uB�B�]B�B�Bs�Be�Bc�BG B0�B2�B�B�YB��B�B�fB��B� BÞB�$B�lB��B��B�gBr�BgtBc�BT�B,B#B�B�_BοB��B�[B�kBo�BRjB;�B(B
-B
�B
�B
��B
s�B
V�B
8oB
�B
�B	�@B	ϽB	��B	��B	��B	��B	��B	�B	��B	s�B	>B	/mB	.(B	)�B	�B	8B	uB	LB	B	LB	�B		^B	qB	(B��B��B�#B��B�B�}B��B��B��B٫B��B�NB�xB�B��B�CB��B�dB� B��B�eB��B�4B��B��B��B�$B�B��B��B��B��B�hB��B��B��B��B�B�LB��B��B}$Bt�BqjBpBpBp]BrQBl�Ba6B^+BY�BY�BY�BX�BS�BN`BLUBKyBJ�BJ$BH�BDgBAB@:B>*B<�B:gB9sB9ZB:�B7B5~B4PB6�B4�B5�B6�B6�B5B4tB1
B-�B-B-tB-�B+�B+�B+�B*�B,KB,2B+�B,dB+6B*B)�B)�B,�B,mB+�B,LB+�B+�B,/B,�B.�B1*B6�B5�B4rB2�B2B3)B3�B5SB6�B7�B83B9"B9�BCjBG,BI�BN�BOVBW�BV�BOLBK�BL\BO:BWBt�Bf�BaKB_B^�B^JBZHBP�BM�BN�BS�BSfBUjBT�BQ�BT$BX|B[�B]�B\fB\�B_�B`�B`�Ba�Bb�Bd�BfBg�Bi�BnWBq�BwnBzB}B��B��B��B��B��B�jB�#B��B��B�?B��B��BçB�#BΐB�lB�sB�B��B�(B��B� B�{B�~B��B�4B�@B�,B�_B��B�aB��B	 �B	 �B	B	�B	�B	+B	
B	�B	�B	�B	B	�B	�B	%B	�B	?B	!�B	&�B	'qB	'@B	(yB	(~B	)�B	*�B	+xB	,�B	.�B	0�B	2�B	3�B	6�B	:�B	=�B	=�B	?B	B
B	FB	HB	I$B	IEB	JB	LB	M B	M.B	M(B	N1B	N3B	O6B	O\B	P�B	T�B	Z�B	`�B	b�B	b�B	b�B	b�B	c@B	e�B	h?B	kB	m�B	o-B	q1B	siB	t�B	u�B	v|B	xiB	z�B	{cB	}hB	~B	�3B	��B	��B	��B	��B	��B	��B	�}B	�vB	�{B	��B	�xB	�hB	�PB	��B	��B	��B	�fB	�YB	�B	�B	�5B	�B	�LB	�`B	�5B	�FB	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�#B	�	B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	��B	��B	��B	�=B	�.B	��B	�!B	ȕB	�=B	�eB	�"B	�:B	�B	�B	�TB	ʼB	�fB	΋B	ϺB	ѣB	�XB	ԅB	ըB	�hB	�}B	نB	٘B	ڨB	�kB	�nB	�^B	�`B	�sB	��B	��B	ސB	ߵB	ߪB	��B	��B	��B	�B	�B	�B	��B	��B	�\B	�#B	�B	��B	�JB	�KB	�B	�(B	�B	�B	�B	�B	�"B	�BB	�4B	�B	�1B	�2B	�HB	�HB	�=B	�|B	�dB	�6B	�B	�(B	�zB	��B	�pB	�0B	�9B	�]B	�YB	�RB	�eB	�JB	�WB	�IB	�[B	�\B	�BB	�NB	��B
B
�B
�B
kB
wB
{B
bB
pB
�B
�B
sB
�B
�B
�B
�B
	B
	lB
(B
�B
�B
�B
�B
�B
�B
�B
�B
�B
UB
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
B
�B
�B
OB
ZB
�B
�B
(B
�B
B
,B
�B
0B
(B
!B
�B
BB
)B
 =B
 HB
 KB
!�B
"UB
"JB
#B
#FB
#�B
$gB
$bB
$+B
$+B
$B
$8B
$.B
$IB
$^B
%|B
%�B
'�B
'WB
(]B
(FB
(EB
(;B
(SB
)fB
)|B
)�B
)KB
)CB
)�B
)�B
*DB
*;B
*8B
*EB
*FB
*SB
*�B
+pB
+~B
+qB
+dB
+WB
,aB
,QB
,aB
,nB
,vB
,]B
,xB
,^B
,`B
,kB
,kB
,jB
,EB
,lB
,bB
,�B
.FB
-�B
-�B
-uB
.nB
.�B
.�B
.�B
/�B
/|B
0rB
1hB
1mB
1dB
2aB
2sB
2�B
2�B
3�B
3]B
7vB
8�B
8�B
8�B
9�B
9�B
9�B
9�B
:�B
:�B
;�B
;�B
;�B
;�B
;�B
;�B
<�B
<�B
<�B
<�B
=�B
=�B
>�B
>�B
>�B
>�B
>�B
?B
?�B
?�B
?�B
?�B
?�B
@�B
@�B
A�B
A�B
A�B
B�B
A�B
BB
CB
D(B
DNB
EB
FB
FB
E�B
FMB
F�B
F�B
GB
GB
G�B
HB
G�B
HB
HB
G�B
HB
H>B
IOB
H>B
HB
G�B
IB
IB
I	B
H�B
H�B
IMB
JB
JB
JB
JB
JB
JB
J%B
KB
KB
K.B
K1B
K=B
LB
L7B
L?B
LB
L*B
LTB
M�B
MnB
NbB
N2B
NB
N"B
NvB
O�B
P2B
P4B
P3B
P6B
PQB
PAB
PNB
QaB
Q0B
Q1B
Q-B
Q.B
Q$B
Q1B
Q#B
Q.B
Q?B
QhB
Q�B
RhB
R5B
REB
R`B
S=B
SIB
SRB
S7B
S-B
S-B
S:B
S/B
S.B
S2B
SLB
S{B
T\B
TDB
TDB
TAB
TAB
TAB
T^B
TAB
TAB
TLB
T5B
U;B
UHB
UHB
UMB
TyB
UuB
UrB
UTB
UfB
UqB
VmB
V}B
V~B
V�B
W�B
WzB
WFB
W`B
XLB
XLB
XOB
X�B
X�B
X~B
Y�B
ZuB
ZcB
Z�B
Z�B
Z�B
[�B
[@B
\�B
]�B
\�B
\�B
\�B
\{B
\yB
]tB
]uB
]~B
]�B
]B
]�B
]�B
^�B
_�B
_�B
`�B
a�B
a}B
a�B
a�B
a�B
a�B
a�B
a�B
b�B
c�B
c�B
c�B
c�B
c�B
c�B
d�B
d�B
e�B
eB
e�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
gB
g�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
h�B
i�B
j�B
j�B
k�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
pB
pB
pB
o�B
pB
p�B
qB
p�B
p�B
p�B
p�B
p�B
p�B
qB
q!B
rB
sB
t B
tB
t B
tB
t@B
uB
uB
u"B
uLB
v
B
vB
v8B
vB
v4B
vB
wB
wB
w111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<H�_<#�
<#�
<N)}<#�
<#�
<$��<1@�<#�
<#�
<#�
<#�
<&)<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<&�j<#�
<#�
<#�
<#�
<#�
<=��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<��k<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =0.46 dbar                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CTM alpha = 0.141 & tau = 6.68 s with error equal to the correction                                                                                                                                                                                             Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  201603101135162016031011351620160310113516  AO  ARCAADJP                                                                    20140721233935    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721233935  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721233935  QCF$                G�O�G�O�G�O�0                                                                                                                                   G�O�G�O�G�O�                PM  ARSQPADJV1.1                                                                20160310113516  QC  PRES            @�ffD�  G�O�                PM  ARSQCTM V1.1                                                                20160310113516  QC  PSAL            @�ffD�  G�O�                PM  ARSQOWGUV1.0WOD + Argo                                                      20170523133343  IP                  G�O�G�O�G�O�                