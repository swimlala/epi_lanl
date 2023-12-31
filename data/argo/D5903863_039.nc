CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       d2014-07-21T23:39:08Z creation; 2014-07-21T23:39:08Z updated; 2015-09-28T12:13:12Z converted from 3.0   
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
_FillValue                 �  IP   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M<   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  px   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �H   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ʄ   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �p   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �H   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �H   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �H   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �H   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �t   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �x   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �|   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20140721233908  20170523133332  5903863 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               'A   AO  4298_0127_039                   2C  D   NAVIS_A                         0127                            120111                          863 @ց^�O`1   @ց_5���@4��
=p��dl�1&�1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      'A   A   A   @�  @�33A   A   A@  A`  A�  A�  A�  A�  A�33A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0y�D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @b�\@�z�@�G�A��A8��AX��Ax��A�Q�A�Q�A�Q�A��A�Q�A��A�Q�A�Q�B(�B(�B(�B(�B&(�B.(�B6(�B>(�BF(�BN(�BV(�B^(�Bf(�Bn(�Bv(�B~(�B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{C�=C�=C�=C�=C	�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C!�=C#�=C%�=C'�=C)�=C+�=C-�=C/�=C1�=C3�=C5�=C7�=C9�=C;�=C=�=C?�=CA�=CC�=CE�=CG�=CI�=CK�=CM�=CO�=CQ�=CS�=CU�=CW�=CY�=C[�=C]�=C_�=Ca�=Cc�=Ce�=Cg�=Ci�=Ck�=Cm�=Co�=Cq�=Cs�=Cu�=Cw�=Cy�=C{�=C}�=C�=C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D b�D �Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�D	b�D	�D
b�D
�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�D b�D �D!b�D!�D"b�D"�D#b�D#�D$b�D$�D%b�D%�D&b�D&�D'b�D'�D(b�D(�D)b�D)�D*b�D*�D+b�D+�D,b�D,�D-b�D-�D.b�D.�D/b�D/�D0\)D0�D1b�D1�D2b�D2�D3b�D3�D4b�D4�D5b�D5�D6b�D6�D7b�D7�D8b�D8�D9b�D9�D:b�D:�D;b�D;�D<b�D<�D=b�D=�D>b�D>�D?b�D?�D@b�D@�DAb�DA�DBb�DB�DCb�DC�DDb�DD�DEb�DE�DFb�DF�DGb�DG�DHb�DH�DIb�DI�DJb�DJ�DKb�DK�DLb�DL�DMb�DM�DNb�DN�DOb�DO�DPb�DP�DQb�DQ�DRb�DR�DSb�DS�DTb�DT�DUb�DU�DVb�DV�DWb�DW�DXb�DX�DYb�DY�DZb�DZ�D[b�D[�D\b�D\�D]b�D]�D^b�D^�D_b�D_�D`b�D`�Dab�Da�Dbb�Db�Dcb�Dc�Ddb�Dd�Deb�De�Dfb�Df�Dgb�Dg�Dhb�Dh�Dib�Di�Djb�Dj�Dkb�Dk�Dlb�Dl�Dmb�Dm�Dnb�Dn�Dob�Do�Dpb�Dp�Dqb�Dq�Drb�Dr�Dsb�Ds�Dtb�Dt�Dub�Du�Dvb�Dv�Dwb�Dw�Dxb�Dx�Dyb�Dy�Dzb�Dz�D{b�D{�D|b�D|�D}b�D}�D~b�D~�Db�D�D�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD±HD��HD�1HD�qHDñHD��HD�1HD�qHDıHD��HD�1HD�qHDűHD��HD�1HD�qHDƱHD��HD�1HD�qHDǱHD��HD�1HD�qHDȱHD��HD�1HD�qHDɱHD��HD�1HD�qHDʱHD��HD�1HD�qHD˱HD��HD�1HD�qHḎHD��HD�1HD�qHDͱHD��HD�1HD�qHDαHD��HD�1HD�qHDϱHD��HD�1HD�qHDбHD��HD�1HD�qHDѱHD��HD�1HD�qHDұHD��HD�1HD�qHDӱHD��HD�1HD�qHDԱHD��HD�1HD�qHDձHD��HD�1HD�qHDֱHD��HD�1HD�qHDױHD��HD�1HD�qHDرHD��HD�1HD�qHDٱHD��HD�1HD�qHDڱHD��HD�1HD�qHD۱HD��HD�1HD�qHDܱHD��HD�1HD�qHDݱHD��HD�1HD�qHDޱHD��HD�1HD�qHD߱HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�D��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD��HD��HD�1HD�qHD�D��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�t{D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��APA\A�A�AuA�AuA�A�A�A�A�A�A¡�A£�A£�A£�A£�A¥�A§�A©�A¬A¬A¬A¬A¬A®A®A°!A°!A®A°!A¬A§�A£�A�A�AuAuA�jA�C�A�$�A��A�$�A�-A��A�`BA���A���A�
=A�JA��jA�`BA�/A�G�A�v�A�^5A��A�jA��;A�33A���A���A���A���A�^5A��A�=qA���A���A��DA���A��A�hsA��\A��A�$�A�VA���A��hA��RA�5?A��
A��A���A�bNA��`A���A�1A�;dA�`BA�\)A�  A�A�oA���A���A���A�|�A�A�A��A|ȴAy�AuAo�AkAg��Af(�Ad�!AbA`�yA`Q�A^ĜA^ffA\A�AW��AS��AS+APr�AOt�AN�ANz�AM`BAKƨAK?}AJĜAIƨAH��AEl�ABr�A?��A>�yA>�/A=�mA<n�A:5?A7�A6�+A5��A4 �A1K�A0�uA.�DA,VA++A)�#A'�-A%�FA$A"ZA!C�A ��A ��A r�A�hAȴA�mAE�AZAdZA�AffA��A��A�^A��A��A�!A/A{A;dAQ�A�;A��A7LA�9A�uAz�A1'A�AoAA�AA	ƨA	t�A	�A�jA��A�A��A�AoA�RA�hA�A5?A��A?}A ~�@�t�@�V@���@���@�  @��\@�&�@�9X@�w@�!@���@�X@���@�@�?}@�@���@��@�ff@�(�@柾@�{@�D@���@�Ĝ@߅@�O�@�Q�@ۅ@�V@ץ�@�C�@�$�@���@�I�@���@с@�Ĝ@϶F@�K�@�;d@ΰ!@�{@��
@�dZ@ʗ�@ɺ^@ɉ7@�&�@���@��@ŉ7@�X@�hs@��`@�ƨ@�n�@��@��h@��9@�r�@�ƨ@�K�@�ȴ@�V@�/@�ƨ@�M�@���@��P@���@�-@���@��`@�K�@�o@��\@�V@��
@��@��-@��@�^5@�7L@��u@�b@�M�@�M�@�ff@�v�@���@�{@�-@��T@���@�-@�E�@�X@�bN@�A�@�1@�@�7L@�Q�@�S�@��H@�E�@���@��@�bN@�  @��;@�l�@��@��P@���@�b@�Q�@��@�t�@�~�@�-@�{@��^@��h@�&�@��@��#@�33@��
@��@��w@�+@�?}@�j@�b@���@�  @�1@�b@�  @��@��m@��@��9@�`B@�^5@�M�@���@��@���@��/@���@���@�bN@�Z@��u@�bN@�I�@�j@�I�@�  @��m@�ƨ@��
@��F@���@���@���@�S�@��@���@���@�o@�+@�33@��y@�5?@��-@���@�`B@�?}@��@�V@��@�V@���@��`@��9@��@�Q�@�A�@�9X@�1'@� �@��@�ƨ@���@�t�@��@��\@�v�@�E�@�{@�@���@��@��-@��@�`B@�V@��@���@���@��/@���@�Z@�(�@���@��;@�ƨ@��@�dZ@�K�@�33@�o@���@���@���@�V@�$�@��@���@���@��h@�hs@�?}@�&�@�V@���@��@�j@�9X@�b@��
@��@�t�@�33@�
=@���@��+@�v�@�E�@��@��h@�p�@�X@�X@��@��@�Z@�1'@�b@���@��m@���@�C�@�+@�@���@��\@�ff@�=q@�@��T@���@��-@��h@�`B@�?}@��@��/@�Ĝ@���@��D@�z�@�9X@��m@��@�\)@��@��@���@�ff@�V@�E�@�5?@�$�@�@��T@���@��^@��-@���@��h@�?}@�Ĝ@��9@���@��D@�Q�@�9X@� �@��@�  @��@|�@;d@~�R@~E�@}��@|��@|�j@|�D@|(�@{�m@{dZ@{o@z�\@zM�@z�@y��@y&�@xr�@w�;@w��@w|�@w+@v��@v��@vff@vE�@u�@u@u�-@u��@up�@uV@tz�@s�m@sƨ@s��@st�@st�@st�@st�@s33@r�!@r^5@r-@q�@q��@qx�@q�@pĜ@pQ�@p �@o|�@o
=@n�R@nE�@n@m�h@m�@l�j@l(�@l1@l1@l1@k��@kt�@ko@j��@jn�@i�#@ix�@h��@h�9@hb@g��@gK�@g�@f��@f�R@fv�@f@d��@dj@d9X@d�@cC�@c@b�@b�!@b~�@b-@a��@a��@ahs@`��@_��@_��@_�w@_��@^�@^{@^@^{@]@]/@\�@\Z@\�@[dZ@[o@Z�!@ZM�@Y�#@YX@X��@X �@W�;@W��@WK�@W
=@Vȴ@V{@V{@V@U��@U��@T��@T9X@S��@St�@So@R��@RM�@Q��@QG�@Q&�@Q�@P��@P��@P��@PĜ@Pb@O��@O;d@N�R@NV@M��@M�h@M/@L�j@Lj@L�@K�m@K��@K33@K"�@K"�@Ko@J�@J��@I��@I�^@I�^@I��@I�7@IG�@I�@I�@H��@H�u@HA�@H �@G�P@G+@F��@F�@Fff@E�T@E��@E�h@Ep�@E?}@D��@D��@DZ@D9X@D�@Cƨ@CdZ@C"�@B�H@BM�@B�@A�@A�#@A��@A�7@A7L@@  @?��@?K�@>�y@>�y@>�R@>��@>V@>@=��@=p�@<�@<��@<��@<�D@<Z@;��@;�
@;ƨ@;�F@;t�@;"�@:��@:~�@:J@9�^@97L@9%@8��@8��@8r�@81'@7�@7�P@7;d@6��@65?@5`B@4�@4z�@4I�@4�@3ƨ@3S�@2~�@1x�@1&�@1%@0��@01'@/�;@/�@/��@/\)@/
=@.�R@.V@.{@-��@-p�@-V@,I�@,1@+�F@+��@+t�@+dZ@+@*�H@*�\@*=q@)��@)X@)%@(�`@(�9@(bN@(A�@(b@'��@'K�@&��@&��@&ff@&V@&5?@%�@%@%��@%�h@%`B@%O�@%�@$�/@$z�@#��@#o@"�\@"M�@!�@!��@!&�@ ��@ ��@ �9@ �9@ �u@ Q�@  �@�;@l�@+@
=@ȴ@��@V@{@�T@�h@`B@O�@��@�/@��@�@j@9X@(�@��@�m@�F@t�@"�@�@��@��@^5@=q@-@�@J@��@x�@7L@�@�`@�u@�@�@�u@1'@�@�@|�@l�@K�@��@�y@�y@�@��@�+@ff@5?@$�@{@@�@�T@��@�-@�h@O�@/@V@��@�@�@I�@9X@�@�
@��@�@�@t�@�@t�@C�@@�H@��@n�@-@�@J@��@X@&�@�@�`@�9@�9@�9@Ĝ@Ĝ@��@ �@ �@b@��@K�@
=@��@�y@�@ȴ@�R@��@��@v�@V@E�@5?@�T@�@`B@/@�@V@V@��@�/@�@�D@z�@Z@�@�
@�F@�F@�F@�F@��@�@t�@C�@"�@
��@
��@
��@
��@
^5@
M�@
M�@
-@	��@	hs@	G�@	&�@��@Ĝ@�@A�@�@�w@�@��@|�@\)@K�@;d@
=@��@�y@ȴ@��@�+@v�@v�@v�@ff@{@�-@p�@`B@O�@?}@?}@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  APA\A�A�AuA�AuA�A�A�A�A�A�A¡�A£�A£�A£�A£�A¥�A§�A©�A¬A¬A¬A¬A¬A®A®A°!A°!A®A°!A¬A§�A£�A�A�AuAuA�jA�C�A�$�A��A�$�A�-A��A�`BA���A���A�
=A�JA��jA�`BA�/A�G�A�v�A�^5A��A�jA��;A�33A���A���A���A���A�^5A��A�=qA���A���A��DA���A��A�hsA��\A��A�$�A�VA���A��hA��RA�5?A��
A��A���A�bNA��`A���A�1A�;dA�`BA�\)A�  A�A�oA���A���A���A�|�A�A�A��A|ȴAy�AuAo�AkAg��Af(�Ad�!AbA`�yA`Q�A^ĜA^ffA\A�AW��AS��AS+APr�AOt�AN�ANz�AM`BAKƨAK?}AJĜAIƨAH��AEl�ABr�A?��A>�yA>�/A=�mA<n�A:5?A7�A6�+A5��A4 �A1K�A0�uA.�DA,VA++A)�#A'�-A%�FA$A"ZA!C�A ��A ��A r�A�hAȴA�mAE�AZAdZA�AffA��A��A�^A��A��A�!A/A{A;dAQ�A�;A��A7LA�9A�uAz�A1'A�AoAA�AA	ƨA	t�A	�A�jA��A�A��A�AoA�RA�hA�A5?A��A?}A ~�@�t�@�V@���@���@�  @��\@�&�@�9X@�w@�!@���@�X@���@�@�?}@�@���@��@�ff@�(�@柾@�{@�D@���@�Ĝ@߅@�O�@�Q�@ۅ@�V@ץ�@�C�@�$�@���@�I�@���@с@�Ĝ@϶F@�K�@�;d@ΰ!@�{@��
@�dZ@ʗ�@ɺ^@ɉ7@�&�@���@��@ŉ7@�X@�hs@��`@�ƨ@�n�@��@��h@��9@�r�@�ƨ@�K�@�ȴ@�V@�/@�ƨ@�M�@���@��P@���@�-@���@��`@�K�@�o@��\@�V@��
@��@��-@��@�^5@�7L@��u@�b@�M�@�M�@�ff@�v�@���@�{@�-@��T@���@�-@�E�@�X@�bN@�A�@�1@�@�7L@�Q�@�S�@��H@�E�@���@��@�bN@�  @��;@�l�@��@��P@���@�b@�Q�@��@�t�@�~�@�-@�{@��^@��h@�&�@��@��#@�33@��
@��@��w@�+@�?}@�j@�b@���@�  @�1@�b@�  @��@��m@��@��9@�`B@�^5@�M�@���@��@���@��/@���@���@�bN@�Z@��u@�bN@�I�@�j@�I�@�  @��m@�ƨ@��
@��F@���@���@���@�S�@��@���@���@�o@�+@�33@��y@�5?@��-@���@�`B@�?}@��@�V@��@�V@���@��`@��9@��@�Q�@�A�@�9X@�1'@� �@��@�ƨ@���@�t�@��@��\@�v�@�E�@�{@�@���@��@��-@��@�`B@�V@��@���@���@��/@���@�Z@�(�@���@��;@�ƨ@��@�dZ@�K�@�33@�o@���@���@���@�V@�$�@��@���@���@��h@�hs@�?}@�&�@�V@���@��@�j@�9X@�b@��
@��@�t�@�33@�
=@���@��+@�v�@�E�@��@��h@�p�@�X@�X@��@��@�Z@�1'@�b@���@��m@���@�C�@�+@�@���@��\@�ff@�=q@�@��T@���@��-@��h@�`B@�?}@��@��/@�Ĝ@���@��D@�z�@�9X@��m@��@�\)@��@��@���@�ff@�V@�E�@�5?@�$�@�@��T@���@��^@��-@���@��h@�?}@�Ĝ@��9@���@��D@�Q�@�9X@� �@��@�  @��@|�@;d@~�R@~E�@}��@|��@|�j@|�D@|(�@{�m@{dZ@{o@z�\@zM�@z�@y��@y&�@xr�@w�;@w��@w|�@w+@v��@v��@vff@vE�@u�@u@u�-@u��@up�@uV@tz�@s�m@sƨ@s��@st�@st�@st�@st�@s33@r�!@r^5@r-@q�@q��@qx�@q�@pĜ@pQ�@p �@o|�@o
=@n�R@nE�@n@m�h@m�@l�j@l(�@l1@l1@l1@k��@kt�@ko@j��@jn�@i�#@ix�@h��@h�9@hb@g��@gK�@g�@f��@f�R@fv�@f@d��@dj@d9X@d�@cC�@c@b�@b�!@b~�@b-@a��@a��@ahs@`��@_��@_��@_�w@_��@^�@^{@^@^{@]@]/@\�@\Z@\�@[dZ@[o@Z�!@ZM�@Y�#@YX@X��@X �@W�;@W��@WK�@W
=@Vȴ@V{@V{@V@U��@U��@T��@T9X@S��@St�@So@R��@RM�@Q��@QG�@Q&�@Q�@P��@P��@P��@PĜ@Pb@O��@O;d@N�R@NV@M��@M�h@M/@L�j@Lj@L�@K�m@K��@K33@K"�@K"�@Ko@J�@J��@I��@I�^@I�^@I��@I�7@IG�@I�@I�@H��@H�u@HA�@H �@G�P@G+@F��@F�@Fff@E�T@E��@E�h@Ep�@E?}@D��@D��@DZ@D9X@D�@Cƨ@CdZ@C"�@B�H@BM�@B�@A�@A�#@A��@A�7@A7L@@  @?��@?K�@>�y@>�y@>�R@>��@>V@>@=��@=p�@<�@<��@<��@<�D@<Z@;��@;�
@;ƨ@;�F@;t�@;"�@:��@:~�@:J@9�^@97L@9%@8��@8��@8r�@81'@7�@7�P@7;d@6��@65?@5`B@4�@4z�@4I�@4�@3ƨ@3S�@2~�@1x�@1&�@1%@0��@01'@/�;@/�@/��@/\)@/
=@.�R@.V@.{@-��@-p�@-V@,I�@,1@+�F@+��@+t�@+dZ@+@*�H@*�\@*=q@)��@)X@)%@(�`@(�9@(bN@(A�@(b@'��@'K�@&��@&��@&ff@&V@&5?@%�@%@%��@%�h@%`B@%O�@%�@$�/@$z�@#��@#o@"�\@"M�@!�@!��@!&�@ ��@ ��@ �9@ �9@ �u@ Q�@  �@�;@l�@+@
=@ȴ@��@V@{@�T@�h@`B@O�@��@�/@��@�@j@9X@(�@��@�m@�F@t�@"�@�@��@��@^5@=q@-@�@J@��@x�@7L@�@�`@�u@�@�@�u@1'@�@�@|�@l�@K�@��@�y@�y@�@��@�+@ff@5?@$�@{@@�@�T@��@�-@�h@O�@/@V@��@�@�@I�@9X@�@�
@��@�@�@t�@�@t�@C�@@�H@��@n�@-@�@J@��@X@&�@�@�`@�9@�9@�9@Ĝ@Ĝ@��@ �@ �@b@��@K�@
=@��@�y@�@ȴ@�R@��@��@v�@V@E�@5?@�T@�@`B@/@�@V@V@��@�/@�@�D@z�@Z@�@�
@�F@�F@�F@�F@��@�@t�@C�@"�@
��@
��@
��@
��@
^5@
M�@
M�@
-@	��@	hs@	G�@	&�@��@Ĝ@�@A�@�@�w@�@��@|�@\)@K�@;d@
=@��@�y@ȴ@��@�+@v�@v�@v�@ff@{@�-@p�@`B@O�@?}@?}@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBcTBcTBcTBcTBcTBcTBcTBcTBcTBcTBcTBcTBcTBcTBcTBcTBcTBcTBcTBcTBcTBcTBcTBbNBcTBcTBcTBcTBcTBcTBcTBcTBcTBcTBcTBbNBbNBbNBbNB`BB_;B\)BW
BG�B;dB0!B)�B"�BJB��B�fB�/B�
B��B��B��B�
B�NB�sB�`B�B��BŢB�dB�3B�B��Bz�BM�B?}B8RB.B�B%B��B�B�)BǮB�'Bx�BW
BM�BF�BC�B@�B:^B#�BoB%B
��B
�sB
�
B
��B
�^B
��B
y�B
dZB
O�B
=qB
(�B
�B	��B	�5B	�wB	��B	� B	iyB	^5B	T�B	F�B	?}B	;dB	2-B	-B	 �B	JB��B��B�B�B�B�yB�sB�`B�NB�HB�NB�B��BƨB�dB�LB��B�}B�RB�3B�'B�B��B��B��B�oB�=B�VB�uB�\B�DB�B�B~�Bz�By�Bz�Bz�Bz�Bu�Bq�Bp�Bn�Bn�Bm�Bl�BjBgmBe`Be`Be`BdZBaHB^5BYBW
BW
BW
BVBW
BYB[#B]/B\)B`BBaHBaHB]/B]/B]/B]/B\)B\)BW
BR�BS�BT�BT�BVBVBT�BT�BW
BZBZBZB[#B^5B_;BaHBaHBaHBbNBaHBaHBdZBe`Be`Be`BffBgmBgmBjBk�Bk�Bp�Bl�BjBhsBgmBiyBhsBffBhsBhsBgmBjBl�Bp�Br�Bs�Bu�Bu�Bu�Bu�Bu�By�Bz�Bz�By�By�Bz�B� B�B�B�B�%B�%B�+B�=B�JB�VB�hB�hB�hB�{B��B��B��B��B��B��B��B��B��B�!B�9B�FB�XB�jB��B�jB��B��B��BɺBƨBƨBƨBŢBȴBɺB��B��B��B�
B�B��B��B��B��B��B��B�
B�B�B�#B�)B�)B�5B�;B�BB�ZB�fB�mB�sB�B�B��B��B��B��B��B��B	  B	B	B	B	1B	JB	uB	�B	$�B	)�B	.B	-B	'�B	&�B	'�B	+B	/B	0!B	2-B	49B	49B	5?B	7LB	>wB	D�B	L�B	T�B	T�B	VB	W
B	ZB	_;B	aHB	cTB	ffB	jB	n�B	r�B	u�B	v�B	x�B	|�B	�B	�B	�B	�+B	�1B	�7B	�JB	�JB	�PB	�\B	�bB	�hB	�hB	�hB	�bB	�bB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�FB	�RB	�^B	�dB	�qB	�wB	�wB	�wB	��B	B	ÖB	ŢB	ƨB	ǮB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�)B	�/B	�5B	�;B	�BB	�HB	�TB	�ZB	�ZB	�`B	�fB	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
+B
+B
1B
	7B
	7B

=B
DB
DB
JB
JB
PB
PB
VB
VB
VB
\B
\B
bB
bB
hB
bB
hB
hB
hB
hB
hB
oB
oB
oB
oB
oB
oB
oB
uB
uB
uB
uB
uB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
"�B
"�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
)�B
)�B
+B
+B
,B
,B
+B
,B
,B
,B
,B
,B
,B
-B
.B
.B
/B
.B
/B
/B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
2-B
2-B
33B
33B
33B
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
:^B
;dB
;dB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
?}B
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
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
K�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
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
YB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
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
_;B
^5B
^5B
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
aHB
aHB
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
cTB
bNB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
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
gmB
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
hsB
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
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
o�B
o�B
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
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
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
u�B
u�B
u�B
u�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BcpBc^Bc�Bc~BctBc�BcpBc\Bc�BctBc�BciBcqBcsBcBc}Bc{BcuBcqBcnBcnBcuBc{BbwBcwBcqBcwBcqBc{Bc�BcqBc�Bc�Bc�Bc�Bb�Bb�Bb|Bc&BaB_�B_
B\�BK�B?�B3B+�B*�B�B��B�wB�{BߟB�jB�'B�`B�JB�B��B�BةB�5B�hB��B�FB�B�4B��BQ�BA6B:�B3RB�B	zB�pB�B�kB�B��B�#BY$BOqBGzBD&BA�B@*B(WB�B	�B
��B
��B
��B
�2B
��B
�#B
WB
jIB
UB
C2B
.EB
tB
B	�xB	�UB	�~B	��B	nB	a�B	[GB	IcB	A4B	?nB	3�B	3B	,�B	�B��B��B�B�bB�VB�B�B�DB�.B��B�B�xBؓB�RB�ZB��B� BĩB��B��B��B�.B� B�SB�EB��B��B��B�vB�TB��B�nB�B��B|BzxB{~B|�B|�Bw�BusBt�Bp�Bo�Bn�Bn�BmBBi�Be�Be�BhBh�Bd�B`�B[�BXvBXBXUBW�BW�BY�B\-B^@B_Bc*BeIBeB^bB^�B^~B]�B^=B`GB[BT�BU�BX�BWMBX�BW�BV�BW�BY�B\B\{B]�B_�B`�Ba~Bb�Bb@BcBc�Bb�BeBf6Be�BffBf`Bg�BhEBjBlfBlbBmvBs�Bn5Bl,BkEBh�Bj�Bk�Bh|Bi=Bj4BiEBk�Bn�Br�BtBu{Bv�BvBv�BwBy5Bz�B|_B|jBziBz�B}4B��B�aB��B�PB�=B�,B�sB�:B�-B��B�B��B�iB��B��B��B�3B�B�FB��B�|B��B�B��B��B��B�PB�&B�^B�dB�hB��B�?B˰B��BǹBɐB��B��B��BʠB�B�B��B��B��B��BՑBӐB�YBәB��B� BٛBܼB��B�B�7B�B�TB�B��B�2B�B�5B��B��B��B�]B��B�TB��B	 _B	�B	�B	�B	pB	sB	�B	B	%OB	*-B	/4B	0B	)RB	'�B	(OB	+4B	/LB	0RB	2~B	4�B	4�B	5fB	6[B	=�B	CfB	M-B	VGB	V?B	V�B	V�B	Z1B	`B	a�B	c�B	fRB	kB	n�B	r�B	v2B	wsB	y3B	}YB	�3B	��B	�sB	��B	�sB	��B	�;B	��B	�8B	�uB	�~B	��B	�
B	��B	�+B	��B	��B	��B	��B	��B	��B	��B	�B	� B	�JB	�LB	�bB	�LB	�HB	�NB	�ZB	��B	��B	��B	��B	�3B	��B	��B	��B	��B	��B	��B	��B	�B	�B	� B	�EB	�B	��B	��B	�B	�LB	�ZB	�=B	�LB	�)B	�6B	�rB	�FB	�BB	�CB	�VB	�KB	�jB	�wB	ԵB	ՌB	֏B	׀B	׃B	�tB	؊B	؍B	�zB	�|B	ڄB	��B	��B	ݹB	޷B	��B	��B	��B	��B	��B	�*B	��B	��B	��B	�1B	�RB	��B	��B	��B	�uB	�B	�#B	�'B	�B	�B	�	B	�TB	�[B	�B	�%B	�@B	�5B	�,B	�3B	�IB	�,B	�B	�/B	�5B	�EB	�<B	�EB	�_B	�;B	�QB	�2B	�5B	�vB	��B	��B	�iB	��B	�nB	��B
 zB
 YB
 VB
XB
XB
pB
pB
aB
iB
UB
bB
dB
�B
�B
uB
�B
xB
�B
�B
�B
sB
�B
�B
�B
�B
�B
�B
	�B
	�B

�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
B
�B
B
�B
<B
B
B
!B
	B
)B
(B
#B
AB
�B
�B
�B
�B
?B
-B
B
2B
RB
4B
JB
(B
fB
DB
-B
 B
 B
 )B
!1B
!VB
"�B
"pB
#1B
#'B
#�B
$@B
%&B
%JB
%BB
&_B
&cB
&<B
&TB
&|B
&�B
&@B
&	B
'!B
'�B
'�B
'4B
'B
(lB
(�B
(aB
(�B
(fB
(�B
(jB
(|B
(xB
(�B
)�B
*�B
*�B
+sB
+nB
,xB
,mB
+gB
,�B
,CB
,LB
,cB
,cB
,�B
-�B
.~B
.�B
/�B
.�B
/�B
/�B
0�B
0rB
0hB
0tB
0wB
0`B
0mB
0�B
1�B
1�B
2�B
2�B
3�B
3�B
3�B
4�B
4�B
4�B
5�B
5�B
5�B
5�B
5{B
5�B
5�B
5�B
6B
6�B
6�B
6�B
6�B
6�B
7�B
7�B
7�B
7�B
7�B
7�B
7�B
8�B
8�B
8�B
8�B
9�B
9�B
9�B
9�B
:�B
:�B
:�B
:�B
:�B
:�B
:�B
;�B
;�B
<�B
=B
<�B
<�B
=�B
=�B
=�B
=�B
>�B
>�B
>�B
>�B
?�B
?�B
?�B
?�B
?�B
?�B
@B
A$B
A�B
A�B
A�B
A�B
BB
A�B
A�B
A�B
A�B
BB
BB
C	B
CB
DB
D7B
D�B
D�B
D�B
D�B
FB
FB
F,B
G!B
GSB
HGB
H�B
IwB
JB
JB
JB
J9B
JRB
K�B
K�B
L=B
M#B
M4B
M�B
NIB
N2B
NB
NBB
OPB
OTB
O^B
OLB
PnB
PGB
PgB
P�B
QXB
QaB
R>B
RAB
R6B
RrB
RAB
RmB
StB
S�B
TrB
TyB
TLB
TWB
TgB
UMB
UXB
UcB
U�B
VrB
VtB
VjB
WMB
WYB
WpB
WeB
WZB
WRB
WhB
XUB
XlB
XwB
X�B
X�B
Y�B
Z�B
Z�B
Z�B
Z�B
Z�B
[�B
[vB
[vB
[^B
[vB
[�B
\�B
\�B
\�B
\�B
]�B
]�B
]�B
]�B
]�B
^�B
^�B
^�B
^B
^�B
^�B
_�B
^�B
^�B
_�B
_�B
_�B
_�B
_�B
_�B
`�B
`�B
`�B
`�B
`�B
a�B
a�B
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
c�B
b�B
c�B
c�B
c�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
f�B
f�B
f�B
f�B
f�B
f�B
g�B
g�B
g�B
g�B
g�B
g�B
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
h�B
jB
i�B
i�B
i�B
j�B
j�B
j�B
j�B
j�B
j�B
k/B
k�B
k�B
lB
l2B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
nB
n#B
m�B
m�B
n�B
n�B
o�B
o�B
n�B
pB
o�B
o�B
o�B
pB
pB
p�B
p�B
p�B
p�B
p�B
p�B
p�B
qB
p�B
qB
q�B
q�B
q�B
rB
q�B
q�B
q�B
r(B
s.B
s B
tB
tB
tB
t B
t B
t,B
uB
uB
uB
uB
uB
uB
uB
uB
vB
vB
vB
vB
vB
v	B
u�B
v B
v	B
v8B
wDB
w0B
wB
xB
xB
x	B
x:B
x111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<Q`�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<Wn�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<,��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<)R�<@4�<12�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<R�<G��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<8�?</ގ<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =0.46 dbar                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CTM alpha = 0.141 & tau = 6.68 s with error equal to the correction                                                                                                                                                                                             Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  201603101134542016031011345420160310113454  AO  ARCAADJP                                                                    20140721233908    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721233908  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721233908  QCF$                G�O�G�O�G�O�0                                                                                                                                   G�O�G�O�G�O�                PM  ARSQPADJV1.1                                                                20160310113454  QC  PRES            @�  D���G�O�                PM  ARSQCTM V1.1                                                                20160310113454  QC  PSAL            @�  D���G�O�                PM  ARSQOWGUV1.0WOD + Argo                                                      20170523133332  IP                  G�O�G�O�G�O�                