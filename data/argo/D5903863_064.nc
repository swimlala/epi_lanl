CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       d2014-07-21T23:39:25Z creation; 2014-07-21T23:39:25Z updated; 2015-09-28T12:13:28Z converted from 3.0   
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
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  IH   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  pX   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ݸ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �$   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �d   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �t   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �x   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721233925  20170523133339  5903863 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               @A   AO  4298_0127_064                   2C  D   NAVIS_A                         0127                            120111                          863 @ֿ�NQ 1   @ֿ� �?�@7��1&��duXbM�1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      @A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:�fD;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@|(�@�G�@�G�A��A8��AX��Ax��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B(�B(�B(�B(�B&(�B.(�B6(�B>(�BF(�BN(�BV(�B^(�Bf(�Bn(�Bv(�B~(�B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�G�B�{B�{B�{B�{B�{B�{B�{B�{C�=C�=C�=C�=C	�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C!�=C#�=C%�=C'�=C)�=C+�=C-�=C/�=C1�=C3�=C5�=C7�=C9�=C;�=C=�=C?�=CA�=CC�=CE�=CG�=CI�=CK�=CM�=CO�=CQ�=CS�=CU�=CW�=CY�=C[�=C]�=C_�=Ca�=Cc�=Ce�=Cg�=Ci�=Ck�=Cm�=Co�=Cq�=Cs�=Cu�=Cw�=Cy�=C{�=C}�=C�=C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D b�D �Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�D	b�D	�D
b�D
�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�D b�D �D!b�D!�D"b�D"�D#b�D#�D$b�D$�D%b�D%�D&b�D&�D'b�D'�D(b�D(�D)b�D)�D*b�D*�D+b�D+�D,b�D,�D-b�D-�D.b�D.�D/b�D/�D0b�D0�D1b�D1�D2b�D2�D3b�D3�D4b�D4�D5b�D5�D6b�D6�D7b�D7�D8b�D8�D9b�D9�D:h�D:�D;b�D;�D<b�D<�D=b�D=�D>b�D>�D?b�D?�D@b�D@�DAb�DA�DBb�DB�DCb�DC�DDb�DD�DEb�DE�DFb�DF�DGb�DG�DHb�DH�DIb�DI�DJb�DJ�DKb�DK�DLb�DL�DMb�DM�DNb�DN�DOb�DO�DPb�DP�DQb�DQ�DRb�DR�DSb�DS�DTb�DT�DUb�DU�DVb�DV�DWb�DW�DXb�DX�DYb�DY�DZb�DZ�D[b�D[�D\b�D\�D]b�D]�D^b�D^�D_b�D_�D`b�D`�Dab�Da�Dbb�Db�Dcb�Dc�Ddb�Dd�Deb�De�Dfb�Df�Dgb�Dg�Dhb�Dh�Dib�Di�Djb�Dj�Dkb�Dk�Dlb�Dl�Dmb�Dm�Dnb�Dn�Dob�Do�Dpb�Dp�Dqb�Dq�Drb�Dr�Dsb�Ds�Dtb�Dt�Dub�Du�Dvb�Dv�Dwb�Dw�Dxb�Dx�Dyb�Dy�Dzb�Dz�D{b�D{�D|b�D|�D}b�D}�D~b�D~�Db�D�D�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�4{D�t{D��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��D�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD±HD��HD�1HD�qHDñHD��HD�1HD�qHDıHD��HD�1HD�qHDűHD��HD�1HD�qHDƱHD��HD�1HD�qHDǱHD��HD�1HD�qHDȱHD��HD�1HD�qHDɱHD��HD�1HD�qHDʱHD��HD�1HD�qHD˱HD��HD�1HD�qHḎHD��HD�1HD�qHDͱHD��HD�1HD�qHDαHD��HD�1HD�qHDϱHD��HD�1HD�qHDбHD��HD�1HD�qHDѱHD��HD�1HD�qHDұHD��HD�1HD�qHDӱHD��HD�1HD�qHDԱHD��HD�1HD�qHDձHD��HD�1HD�qHDֱHD��HD�1HD�qHDױHD��HD�1HD�qHDرHD��HD�1HD�qHDٱHD��HD�1HD�qHDڱHD��HD�1HD�qHD۱HD��HD�1HD�qHDܱHD��HD�1HD�qHDݱHD��HD�1HD�qHDޱHD��HD�1HD�qHD߱HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�.D�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD��HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1H1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A֟�A֬A֮Aְ!A֧�A֏\A�v�A�E�A�$�A��A��A��A��A� �A�"�A�"�A��A�bA�
=A�
=A�JA�JA�VA�VA�JA�%A�
=A�1A�%A�1A�1A�1A��A՗�A�\)A�=qA���A�33A�bA�C�A��TA��AŸRA���A�(�AÏ\A�jA�=qA��A°!A��A�C�A�A�A��yA��7A�1A���A�dZA�?}A�VA��A�+A���A���A�O�A� �A���A�x�A�9XA��DA�K�A��A���A���A�7LA���A�M�A��9A��uA��\A��wA�bA�G�A�VA���A��A�ZA���A�^5A��A��A���A��uA�v�A���A�ȴA�  A��^A�=qA�7LA��A�(�A�I�A��HA��\A�1'A���A�XA�t�A��yA�+A���A��A���A��+A�ffA�oA��!A�r�A� �A��^A�S�A�l�A��A���A��A�{A���A�A�A���A��uA�I�A���A�JA��A���A�33A~��A|M�Az-Ay��AyC�Ax��AxI�Au�;Ar�Ap�uAo��Ao7LAml�Aj5?AfĜAct�Aa&�A`1A_
=A^1A[��AZv�AY��AY|�AX~�AXbAW�TAV9XAS�TAQ��AO��AN=qAM��AMS�AM%ALn�AJ��AI��AGdZAE��AE��AE&�AD=qABȴAA��A@�A<ȴA;��A;K�A;"�A:�DA9t�A8��A7�FA7&�A5
=A1�#A0A.��A.E�A-x�A+�A+A)��A'��A&jA%A#�
A#`BA"�A!�-A�Av�A|�Av�AƨA��AK�A$�A�A��A
=A-A�mA|�A�Ap�A�9AbNA�A��AK�A�yAA�A\)A
��A
~�A
1A	�mA	�^A	A1'A�;AA�A|�A&�A �uA jA M�A A�@���@��@���@�&�@��\@��u@�R@�$�@���@�t�@�h@���@���@��@���@���@� �@�
=@�@�j@���@�V@��T@��D@�dZ@�;d@��@�n�@ݲ-@�z�@�\)@ڇ+@��@ٙ�@�X@��@؋D@�5?@�`B@�/@��`@ԛ�@�I�@Ұ!@�`B@�Ĝ@�z�@�b@�+@͑h@��@�~�@��T@�X@�Ĝ@��
@�@�E�@��`@���@��
@þw@Õ�@�S�@�+@�
=@��@��@��@�I�@��F@���@��@�ff@��/@��w@���@�ff@�@�/@�A�@�v�@�Ĝ@�|�@���@�v�@���@��@���@�Z@��@��F@�v�@�p�@���@�Z@��@�\)@��@�v�@��@�7L@�/@��@���@�Q�@�1@��
@�\)@��R@�5?@��@�V@���@���@�Q�@�1'@�1'@� �@�1@�  @�  @�  @���@�"�@���@�&�@���@�V@�5?@�(�@�hs@�ƨ@�l�@�t�@�K�@�V@��@��w@�dZ@�+@��@��y@��y@��@�o@�+@�@�S�@�|�@�X@�S�@��w@���@�  @���@�@��9@�K�@��R@��+@�V@���@�V@�r�@�1'@��w@�t�@�\)@��@��@���@�5?@�-@�$�@�J@���@���@���@���@���@��@��@��T@��^@�hs@�G�@�%@���@��9@�bN@�9X@��@��@��w@�\)@�;d@�+@��@�o@�
=@��@��y@�v�@���@�?}@�V@��@�I�@�9X@�(�@�b@���@��m@��m@�|�@��@���@�ȴ@��!@�$�@��h@�&�@���@��@��@�Z@�(�@��;@��@���@���@�t�@�l�@�t�@�t�@�l�@�l�@�l�@�S�@�C�@�;d@�C�@�33@�"�@�"�@�"�@�
=@��@���@�ff@�{@�@���@�x�@�hs@�O�@�7L@�7L@�%@��D@�r�@�I�@�(�@��@��
@���@�K�@��y@���@�n�@�5?@��@���@���@��@���@�z�@�r�@�1@��@K�@+@�@�@�@�@~��@~@|�j@{@z��@z��@z~�@z^5@z^5@zM�@z-@z�@y�#@yG�@xĜ@xb@w�w@w|�@w;d@v�@vff@vE�@v$�@u�@u��@u`B@t��@t�/@t�D@st�@r�!@q�#@qhs@q7L@p�`@p�u@pbN@p �@o�w@n�@m�-@m?}@l9X@k�F@kt�@k33@j�\@jn�@j=q@ix�@iG�@i&�@i�@hbN@g��@f�R@f$�@e@e`B@d�j@dI�@c��@cS�@c33@c"�@co@bn�@b�@bJ@a�#@a�#@a�@a�^@a��@ahs@ahs@aX@`��@`��@`�@`A�@_��@_�P@_+@^ȴ@^��@^E�@]�T@]�T@]��@]��@]�@]/@\��@\�D@\I�@\1@[�m@[�@Z�H@Zn�@Y��@Y�#@Y�7@XĜ@W�@W|�@WK�@V��@V�@V�R@V�R@V�R@V�R@Vv�@U�T@U�h@U�@T��@Tj@T9X@T1@S�m@Sƨ@S��@SdZ@So@R��@Q7L@P�u@PbN@PQ�@P1'@Pb@O�P@O
=@N�@N�@Nȴ@N�R@N�+@M�T@M`B@MV@L�@L�@Lz�@L9X@Kƨ@K33@J�@J�\@J=q@I�^@I��@IX@I&�@H��@Hr�@HQ�@H �@H  @G�;@G�P@G\)@G
=@Fv�@E��@E?}@C�
@C�@CC�@Co@B��@B�@A7L@A%@@��@@��@@�u@?�@?\)@>�y@=�T@=O�@<�/@<�@<Z@<�@<1@;�
@;�F@;�F@;�F@;�F@;��@;��@;dZ@:M�@9�#@9�#@9��@9��@9��@9�^@9��@9x�@9G�@9&�@9%@8��@8��@8Q�@7�@7l�@7K�@6��@6�+@6V@65?@6$�@6$�@6{@6@6{@5�@5�T@5`B@4��@4��@4I�@49X@41@3�m@3�
@3ƨ@3��@3�@3dZ@333@2�H@2n�@1��@1X@0�u@0�@0 �@/�w@/l�@.��@.ff@.{@.@-��@-�-@-�@-`B@-?}@,�@,��@,��@,��@,j@,Z@,I�@,9X@+��@+�
@+t�@+"�@*�@*��@*��@*��@*�\@*^5@*-@)�@)��@)��@)&�@(�9@(�9@(�u@(�@(bN@(Q�@(b@'�@'l�@'K�@'�@&�@&�R@&��@&�+@&V@&5?@&{@%�@%@%�@%`B@%O�@%?}@%/@%V@$�@$9X@#��@#C�@"��@!��@!��@!��@!hs@!%@ Ĝ@ �u@ r�@ r�@ bN@ bN@ bN@ bN@ 1'@�@��@\)@�@v�@@{@@��@(�@��@t�@C�@@��@~�@�\@��@��@�!@�\@�@�@�^@��@��@��@�7@�7@X@%@��@�u@�@�@r�@bN@A�@��@��@|�@l�@\)@K�@;d@+@��@v�@$�@@��@�@p�@`B@O�@?}@/@�@V@�/@��@��@��@�j@�@�@�D@j@9X@��@dZ@C�@"�@@�H@��@-@hs@Ĝ@�@�@�@1'@�w@l�@+@�y@ȴ@��@v�@V@E�@5?@{@@�-@�h@��@��@�@p�@p�@?}@��@�D@��@��@��@�@S�@o@
�@
�@
�@
�H@
�!@
n�@
=q@
-@
J@	��@	X@	G�@	7L@	&�@�`@��@��@Ĝ@�@bN@bN@bN@1'@  @�;@��@��@l�@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A֟�A֬A֮Aְ!A֧�A֏\A�v�A�E�A�$�A��A��A��A��A� �A�"�A�"�A��A�bA�
=A�
=A�JA�JA�VA�VA�JA�%A�
=A�1A�%A�1A�1A�1A��A՗�A�\)A�=qA���A�33A�bA�C�A��TA��AŸRA���A�(�AÏ\A�jA�=qA��A°!A��A�C�A�A�A��yA��7A�1A���A�dZA�?}A�VA��A�+A���A���A�O�A� �A���A�x�A�9XA��DA�K�A��A���A���A�7LA���A�M�A��9A��uA��\A��wA�bA�G�A�VA���A��A�ZA���A�^5A��A��A���A��uA�v�A���A�ȴA�  A��^A�=qA�7LA��A�(�A�I�A��HA��\A�1'A���A�XA�t�A��yA�+A���A��A���A��+A�ffA�oA��!A�r�A� �A��^A�S�A�l�A��A���A��A�{A���A�A�A���A��uA�I�A���A�JA��A���A�33A~��A|M�Az-Ay��AyC�Ax��AxI�Au�;Ar�Ap�uAo��Ao7LAml�Aj5?AfĜAct�Aa&�A`1A_
=A^1A[��AZv�AY��AY|�AX~�AXbAW�TAV9XAS�TAQ��AO��AN=qAM��AMS�AM%ALn�AJ��AI��AGdZAE��AE��AE&�AD=qABȴAA��A@�A<ȴA;��A;K�A;"�A:�DA9t�A8��A7�FA7&�A5
=A1�#A0A.��A.E�A-x�A+�A+A)��A'��A&jA%A#�
A#`BA"�A!�-A�Av�A|�Av�AƨA��AK�A$�A�A��A
=A-A�mA|�A�Ap�A�9AbNA�A��AK�A�yAA�A\)A
��A
~�A
1A	�mA	�^A	A1'A�;AA�A|�A&�A �uA jA M�A A�@���@��@���@�&�@��\@��u@�R@�$�@���@�t�@�h@���@���@��@���@���@� �@�
=@�@�j@���@�V@��T@��D@�dZ@�;d@��@�n�@ݲ-@�z�@�\)@ڇ+@��@ٙ�@�X@��@؋D@�5?@�`B@�/@��`@ԛ�@�I�@Ұ!@�`B@�Ĝ@�z�@�b@�+@͑h@��@�~�@��T@�X@�Ĝ@��
@�@�E�@��`@���@��
@þw@Õ�@�S�@�+@�
=@��@��@��@�I�@��F@���@��@�ff@��/@��w@���@�ff@�@�/@�A�@�v�@�Ĝ@�|�@���@�v�@���@��@���@�Z@��@��F@�v�@�p�@���@�Z@��@�\)@��@�v�@��@�7L@�/@��@���@�Q�@�1@��
@�\)@��R@�5?@��@�V@���@���@�Q�@�1'@�1'@� �@�1@�  @�  @�  @���@�"�@���@�&�@���@�V@�5?@�(�@�hs@�ƨ@�l�@�t�@�K�@�V@��@��w@�dZ@�+@��@��y@��y@��@�o@�+@�@�S�@�|�@�X@�S�@��w@���@�  @���@�@��9@�K�@��R@��+@�V@���@�V@�r�@�1'@��w@�t�@�\)@��@��@���@�5?@�-@�$�@�J@���@���@���@���@���@��@��@��T@��^@�hs@�G�@�%@���@��9@�bN@�9X@��@��@��w@�\)@�;d@�+@��@�o@�
=@��@��y@�v�@���@�?}@�V@��@�I�@�9X@�(�@�b@���@��m@��m@�|�@��@���@�ȴ@��!@�$�@��h@�&�@���@��@��@�Z@�(�@��;@��@���@���@�t�@�l�@�t�@�t�@�l�@�l�@�l�@�S�@�C�@�;d@�C�@�33@�"�@�"�@�"�@�
=@��@���@�ff@�{@�@���@�x�@�hs@�O�@�7L@�7L@�%@��D@�r�@�I�@�(�@��@��
@���@�K�@��y@���@�n�@�5?@��@���@���@��@���@�z�@�r�@�1@��@K�@+@�@�@�@�@~��@~@|�j@{@z��@z��@z~�@z^5@z^5@zM�@z-@z�@y�#@yG�@xĜ@xb@w�w@w|�@w;d@v�@vff@vE�@v$�@u�@u��@u`B@t��@t�/@t�D@st�@r�!@q�#@qhs@q7L@p�`@p�u@pbN@p �@o�w@n�@m�-@m?}@l9X@k�F@kt�@k33@j�\@jn�@j=q@ix�@iG�@i&�@i�@hbN@g��@f�R@f$�@e@e`B@d�j@dI�@c��@cS�@c33@c"�@co@bn�@b�@bJ@a�#@a�#@a�@a�^@a��@ahs@ahs@aX@`��@`��@`�@`A�@_��@_�P@_+@^ȴ@^��@^E�@]�T@]�T@]��@]��@]�@]/@\��@\�D@\I�@\1@[�m@[�@Z�H@Zn�@Y��@Y�#@Y�7@XĜ@W�@W|�@WK�@V��@V�@V�R@V�R@V�R@V�R@Vv�@U�T@U�h@U�@T��@Tj@T9X@T1@S�m@Sƨ@S��@SdZ@So@R��@Q7L@P�u@PbN@PQ�@P1'@Pb@O�P@O
=@N�@N�@Nȴ@N�R@N�+@M�T@M`B@MV@L�@L�@Lz�@L9X@Kƨ@K33@J�@J�\@J=q@I�^@I��@IX@I&�@H��@Hr�@HQ�@H �@H  @G�;@G�P@G\)@G
=@Fv�@E��@E?}@C�
@C�@CC�@Co@B��@B�@A7L@A%@@��@@��@@�u@?�@?\)@>�y@=�T@=O�@<�/@<�@<Z@<�@<1@;�
@;�F@;�F@;�F@;�F@;��@;��@;dZ@:M�@9�#@9�#@9��@9��@9��@9�^@9��@9x�@9G�@9&�@9%@8��@8��@8Q�@7�@7l�@7K�@6��@6�+@6V@65?@6$�@6$�@6{@6@6{@5�@5�T@5`B@4��@4��@4I�@49X@41@3�m@3�
@3ƨ@3��@3�@3dZ@333@2�H@2n�@1��@1X@0�u@0�@0 �@/�w@/l�@.��@.ff@.{@.@-��@-�-@-�@-`B@-?}@,�@,��@,��@,��@,j@,Z@,I�@,9X@+��@+�
@+t�@+"�@*�@*��@*��@*��@*�\@*^5@*-@)�@)��@)��@)&�@(�9@(�9@(�u@(�@(bN@(Q�@(b@'�@'l�@'K�@'�@&�@&�R@&��@&�+@&V@&5?@&{@%�@%@%�@%`B@%O�@%?}@%/@%V@$�@$9X@#��@#C�@"��@!��@!��@!��@!hs@!%@ Ĝ@ �u@ r�@ r�@ bN@ bN@ bN@ bN@ 1'@�@��@\)@�@v�@@{@@��@(�@��@t�@C�@@��@~�@�\@��@��@�!@�\@�@�@�^@��@��@��@�7@�7@X@%@��@�u@�@�@r�@bN@A�@��@��@|�@l�@\)@K�@;d@+@��@v�@$�@@��@�@p�@`B@O�@?}@/@�@V@�/@��@��@��@�j@�@�@�D@j@9X@��@dZ@C�@"�@@�H@��@-@hs@Ĝ@�@�@�@1'@�w@l�@+@�y@ȴ@��@v�@V@E�@5?@{@@�-@�h@��@��@�@p�@p�@?}@��@�D@��@��@��@�@S�@o@
�@
�@
�@
�H@
�!@
n�@
=q@
-@
J@	��@	X@	G�@	7L@	&�@�`@��@��@Ĝ@�@bN@bN@bN@1'@  @�;@��@��@l�@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��BB  B  BBBBBB%B%B%B+B+B%B%B+B+B+B+B1B	7B
=B
=B
=B
=B
=B
=B%B��B��B��B�bB�B|�Bx�Bu�Bp�BjBjBjBl�Bl�Bl�Bl�Bk�BjBk�Bl�Bl�Bl�Bl�Bl�Bk�Bk�Bk�Bk�Bk�Bl�Bm�Bn�Bm�Bn�Bn�Bn�Bo�Bn�Bn�Bm�Bl�Bk�BhsBaHB\)BT�BG�B@�B49B#�BVB��B�B�ZB�#B��BƨB�!B��B��B�1B|�Bl�B^5BYBO�B>wB,B�BDBB��B�B��B��B��B�hB�%B}�Bp�Bl�BjBhsBbNB[#BH�BB�B9XB#�B�BPB+B
��B
�yB
ɺB
�?B
�!B
��B
��B
��B
�\B
}�B
aHB
S�B
I�B
;dB
-B
(�B
%�B
!�B
�B
JB	��B	�B	�ZB	�BB	��B	��B	��B	�{B	�+B	� B	z�B	s�B	gmB	`BB	\)B	[#B	W
B	S�B	P�B	H�B	=qB	2-B	'�B	!�B	�B	�B	�B	�B	VB	
=B	  B��B��B��B�B�B�mB�BB�B��B��B��B��BɺBŢBB�wB�RB�!B�B��B��B��B��B��B��B��B��B�{B�oB�hB�\B�JB�+B�B�B~�B|�Bz�Bw�Bu�Bt�Br�Bq�Bp�Bo�Bn�Bl�Bl�Bk�Bk�BjBjBiyBiyBhsBgmBffBffBffBe`BdZBcTBbNB`BB]/B\)B\)B\)B[#B[#BZBW
BT�BVBXB\)B\)B[#B[#B\)B^5BcTBgmBhsBjBl�Bk�BiyBjBm�Bn�Bn�Bn�Bn�Bp�Br�Br�Bq�Br�Bs�Bv�Bx�Bz�B|�B}�B~�B~�B~�B�B�B�B�%B�%B�%B�7B�JB�PB�PB�VB�bB�uB��B��B��B��B��B��B��B�B�-B�^B�dB�qB�}B�}B�}B�wB��B��BBÖBĜBĜBĜBŢB��B��B��B��B�
B�B�/B�NB�ZB�sB�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B	  B	1B	uB	�B	!�B	$�B	!�B	�B	�B	 �B	$�B	%�B	$�B	"�B	%�B	(�B	,B	.B	/B	/B	0!B	0!B	1'B	1'B	33B	49B	?}B	K�B	O�B	O�B	R�B	R�B	T�B	R�B	M�B	L�B	L�B	K�B	N�B	Q�B	S�B	VB	YB	ZB	[#B	[#B	\)B	_;B	e`B	ffB	ffB	hsB	hsB	hsB	hsB	hsB	hsB	iyB	iyB	jB	l�B	q�B	s�B	u�B	x�B	y�B	}�B	~�B	� B	�B	�B	�%B	�+B	�1B	�=B	�JB	�PB	�\B	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�'B	�'B	�'B	�FB	�jB	�}B	��B	��B	��B	��B	B	ÖB	ÖB	ÖB	ÖB	ĜB	ŢB	ŢB	ŢB	ŢB	ƨB	ƨB	ƨB	ƨB	ƨB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�5B	�;B	�BB	�HB	�NB	�ZB	�`B	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
1B
	7B
	7B
	7B

=B

=B

=B

=B
DB
PB
PB
VB
\B
\B
bB
bB
bB
bB
hB
hB
hB
hB
oB
oB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
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
"�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
&�B
'�B
'�B
'�B
'�B
'�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
,B
,B
,B
,B
,B
-B
-B
.B
.B
.B
/B
/B
/B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
33B
33B
5?B
5?B
5?B
5?B
6FB
6FB
7LB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
J�B
J�B
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
K�B
K�B
K�B
K�B
L�B
L�B
L�B
M�B
M�B
L�B
M�B
M�B
M�B
M�B
N�B
N�B
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
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
W
B
W
B
W
B
W
B
W
B
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
YB
YB
YB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
\)B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
cTB
cTB
dZB
e`B
ffB
ffB
ffB
ffB
ffB
gmB
gmB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
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
o�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B�B�B�BB��B�BB �BOBEB.BB6BCBRBkB�BlBLB@BNBDBNBVBqB	PB
mB
iB
XB
`B
eB
�B�BB�UB��B��B�uB�BzmByBz BnYBm\BmGBmYBmzBn$Bm�Bn�Bs�Bp�BnrBn�BoZBoBm�BlwBl�Bm�Bn>Bn;Bp�BpBo�Bo�Bo�Bo�BquBp�BpBomBn�Bn7Bm�Bm�Bc�B`MB[�BJ�BFB:=B*�BDB��B�@B�KB�{B�}B̓B�B��B�B�VB��Bo�B_�B[uBT�BC�B0�B�B`B�B�B��B�5B�bB�vB�QB��B�oBr#Bm6BkGBjBdRB`�BJRBDzB?9B'�B�B�B	�B�B
��B
��B
��B
�2B
��B
��B
��B
��B
��B
e�B
X�B
O�B
A2B
.�B
*'B
&�B
$*B
#�B
�B	�-B	��B	�[B	�B	�,B	�KB	��B	��B	�3B	��B	}gB	yB	jB	bB	\�B	]^B	XB	T�B	TtB	M�B	A�B	7YB	++B	#^B	�B	�B	cB	B	bB	B	-B��B�aB��B��B�B��B�BٕB� BѮB��B�NB̕BȂB��B�aB�B��B��B��B��B��B��B�B�dB��B�TB��B��B�B��B��B��B�CB�_B��BUB~&BzvBx�BwBt�Bs�BqwBp�Bp�BotBn�Bl�BldBkBllBj�Bk6Bj�Bh�Bg�Bg�Bf�BfBfQBezBcbBduBdBb)B]�B\�B[�B[�B]�B\BU�BV�B[�B_B^�B\'B\�B^�B`�Bf�Bg�Bh�Bj�Bn<BnBk;Bl�Bo<Bp;Bp�Bo�Bp�Br�Bs&Bs&Br�BtBu�Bx�BzFB{�B}�B~�B�B�.B��B��B��B��B��B��B��B�YB�SB��B�+B��B��B��B�B��B��B��B�bB�UB�6B�-B��B��B��B��B�B��B��B�DB�]B�ZB��B�rB��B�dBũBǵB�pB�B��BյB�?B�dB߀B�B�0B�tB�5B��B�B�vB�%B�,B�^B�vB�CB��B��B��B��B��B��B�WB�nB�B�6B��B��B�yB�bB��B�B��B�"B��B�1B�cB��B�NB�$B�=B�IB�.B�(B�,B�vB�B��B	�B	B	B	"4B	'XB	%B	�B	EB	 �B	%KB	'9B	'9B	#�B	&�B	)wB	,�B	.YB	/UB	/JB	00B	0?B	1�B	0�B	3*B	2B	=QB	K�B	Q�B	SuB	T�B	S�B	V�B	T�B	N�B	MMB	MPB	L�B	O�B	R�B	T�B	V�B	Y�B	Z|B	[�B	[�B	\�B	`B	e�B	f�B	f�B	h�B	h�B	h�B	h�B	h�B	h�B	i�B	i�B	j�B	m.B	rB	tHB	vAB	y>B	z�B	~gB	gB	�sB	��B	��B	��B	�B	��B	��B	��B	��B	��B	�OB	��B	��B	�[B	��B	�B	�IB	�QB	�`B	�[B	�UB	�EB	��B	�B	��B	�sB	��B	�?B	�VB	�EB	�/B	��B	��B	� B	�B	�:B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�B	��B	��B	�0B	�:B	ώB	ЋB	ѨB	ҢB	�cB	�sB	�VB	�`B	�^B	�CB	֘B	� B	�{B	ڛB	ەB	ܺB	ݐB	޸B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�IB	� B	�B	��B	��B	��B	��B	��B	�PB	�]B	��B	�1B	�gB	�%B	�9B	�AB	�,B	�7B	�@B	�5B	�]B	��B	��B	��B
 {B
vB
rB
�B
�B
aB
dB
oB
lB
�B
�B
sB
�B
+B
B
	B
�B
	�B
	�B
	�B

�B

�B

�B
2B
cB
�B
UB
�B
�B
�B
B
�B
�B
<B
�B
�B
�B
<B
EB
bB
*B

B
B
EB
B
TB
�B
�B
�B
�B
?B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
B
B
$B
B
#B
B
�B
%B
&B
�B
�B
B
 B
"B
.B
B
B
$B
B
6B
dB
 JB
 DB
!B
!:B
!�B
"�B
"SB
#/B
#BB
#&B
# B
#B
#B
#B
#8B
#mB
$IB
$bB
$eB
%:B
%9B
%9B
%/B
%0B
&AB
&?B
&WB
&oB
'B
'�B
(LB
(7B
(CB
(DB
(�B
)�B
*ZB
*9B
*CB
*DB
*]B
*�B
+�B
,}B
,\B
,uB
,iB
,yB
-�B
-�B
.�B
.�B
.�B
/�B
/pB
/�B
0�B
0�B
0�B
0tB
1�B
1yB
1�B
1�B
1�B
1�B
2�B
2�B
3�B
4lB
5�B
5�B
5�B
5�B
6�B
7 B
7�B
8�B
8�B
8�B
8�B
9�B
9�B
:7B
:�B
:�B
;�B
;�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
=^B
=�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
?�B
@+B
@�B
@�B
@�B
AB
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B"B
B	B
CB
CB
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
DB
E+B
E/B
ELB
FgB
G�B
H2B
H6B
I.B
IEB
I`B
J1B
KB
K$B
KB
K!B
KB
KB
K9B
LB
LB
L'B
L)B
LB
LB
LB
L1B
LB
LLB
MAB
M/B
MB
NB
NB
M.B
N1B
N0B
N>B
N'B
O:B
OmB
OiB
PB
P2B
P(B
P2B
P'B
PNB
PcB
QQB
Q<B
QFB
QUB
Q<B
Q1B
Q=B
QHB
RBB
RBB
RCB
RQB
S`B
SHB
S;B
S;B
S>B
TQB
T�B
T�B
T�B
U�B
U�B
VB
UUB
UIB
UiB
U�B
UqB
UdB
V^B
WIB
WVB
WHB
WHB
WJB
WrB
W�B
W�B
W�B
X�B
X�B
X�B
YMB
Y�B
ZB
[B
Z�B
Z�B
[�B
[�B
[�B
[�B
\WB
]_B
]lB
]cB
]�B
]�B
^�B
^�B
^�B
^�B
^tB
^�B
^wB
^�B
^�B
_�B
_�B
_�B
_xB
_�B
_�B
_�B
_�B
`�B
`�B
`�B
`�B
`�B
`�B
`�B
`�B
aB
a�B
a�B
a�B
a�B
a�B
b�B
b�B
b�B
b�B
b�B
b�B
b�B
b�B
b�B
b�B
b�B
b�B
b�B
b�B
b�B
c�B
c�B
d"B
c�B
c�B
d�B
d�B
c�B
d1B
eXB
fAB
f�B
f�B
f�B
f�B
gB
g�B
g�B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
i�B
i�B
jB
j�B
j�B
j�B
j�B
j�B
j�B
j�B
kB
k�B
l2B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
n(B
oB
n�B
n�B
n�B
oB
o�B
o�B
o�B
pB
o�B
o�B
o�B
qB
qB
p�B
p�B
p�B
qB
rB
q�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<b:e<?0�<#�
<#�
<#�
<#�
<#�
<#�
<*�K<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<(d�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<2�l<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<FF�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<8��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<?ϝ<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<)��<#�
<#�
<#�
<#�
<(��<0��<$P�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<+�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<03�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =0.46 dbar                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CTM alpha = 0.141 & tau = 6.68 s with error equal to the correction                                                                                                                                                                                             Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  201603101135082016031011350820160310113508  AO  ARCAADJP                                                                    20140721233925    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721233925  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721233925  QCF$                G�O�G�O�G�O�0                                                                                                                                   G�O�G�O�G�O�                PM  ARSQPADJV1.1                                                                20160310113508  QC  PRES            @���D�@ G�O�                PM  ARSQCTM V1.1                                                                20160310113508  QC  PSAL            @���D�@ G�O�                PM  ARSQOWGUV1.0WOD + Argo                                                      20170523133339  IP                  G�O�G�O�G�O�                