CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       d2014-07-21T23:39:21Z creation; 2014-07-21T23:39:21Z updated; 2015-09-28T12:13:19Z converted from 3.0   
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
resolution        =���   axis      Z        t  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     t  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \p   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     t  `P   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     t  o�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  8   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     t  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     t  �l   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     t  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     t  �4   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Ȩ   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     t  ̈   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �,   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �,   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �,   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �,   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �X   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �\   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �`   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �d   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �h   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20140721233921  20170523133337  5903863 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               :A   AO  4298_0127_058                   2C  D   NAVIS_A                         0127                            120111                          863 @ְ���@1   @ְ�G�@6�+�d�
=p��1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      :A   A   A   @�33@�  A   A   AA��A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf�Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @h��@�G�@�G�A��A:=qAX��Ax��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B(�B(�B(�B(�B&(�B.(�B6(�B>(�BF(�BN(�BV(�B^(�Bf(�Bn(�Bv(�B~(�B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{C�=C�=C�=C�=C	�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C!�=C#�=C%�=C'�=C)�=C+�=C-�=C/�=C1�=C3�=C5�=C7�=C9�=C;�=C=�=C?�=CA�=CC�=CE�=CG�=CI�=CK�=CM�=CO�=CQ�=CS�=CU�=CW�=CY�=C[�=C]�=C_�=Ca�=Cc�=Ce��Cg�=Ci�=Ck�=Cm�=Co�=Cq�=Cs�=Cu�=Cw�=Cy�=C{�=C}�=C�=C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D b�D �Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�D	b�D	�D
b�D
�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�Db�D�D b�D �D!b�D!�D"b�D"�D#b�D#�D$b�D$�D%b�D%�D&b�D&�D'b�D'�D(b�D(�D)b�D)�D*b�D*�D+b�D+�D,b�D,�D-b�D-�D.b�D.�D/b�D/�D0b�D0�D1b�D1�D2b�D2�D3b�D3�D4b�D4�D5b�D5�D6b�D6�D7b�D7�D8b�D8�D9b�D9�D:b�D:�D;b�D;�D<b�D<�D=b�D=�D>b�D>�D?b�D?�D@b�D@�DAb�DA�DBb�DB�DCb�DC�DDb�DD�DEb�DE�DFb�DF�DGb�DG�DHb�DH�DIb�DI�DJb�DJ�DKb�DK�DLb�DL�DMb�DM�DNb�DN�DOb�DO�DPb�DP�DQb�DQ�DRb�DR�DSb�DS�DTb�DT�DUb�DU�DVb�DV�DWb�DW�DXb�DX�DYb�DY�DZb�DZ�D[b�D[�D\b�D\�D]b�D]�D^b�D^�D_b�D_�D`b�D`�Dab�Da�Dbb�Db�Dcb�Dc�Ddb�Dd�Deb�De�Dfb�Df�Dgb�Dg�Dhb�Dh�Dib�Di�Djb�Dj�Dkb�Dk�Dlb�Dl�Dmb�Dm�Dnb�Dn�Dob�Do�Dpb�Dp�Dqb�Dq�Drb�Dr�Dsb�Ds�Dtb�Dt�Dub�Du�Dvb�Dv�Dwb�Dw�Dxb�Dx�Dyb�Dy�Dzb�Dz�D{b�D{�D|b�D|�D}b�D}�D~b�D~�Db�D�D�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD±HD��HD�1HD�qHDñHD��HD�1HD�qHDıHD��HD�1HD�qHDűHD��HD�1HD�qHDƱHD��HD�1HD�qHDǱHD��HD�1HD�qHDȱHD��HD�1HD�qHDɱHD��HD�1HD�qHDʱHD��HD�1HD�qHD˱HD��HD�1HD�qHḎHD��HD�1HD�qHDͱHD��HD�1HD�qHDαHD��HD�1HD�qHDϱHD��HD�1HD�qHDбHD��HD�1HD�qHDѱHD��HD�1HD�qHDұHD��HD�1HD�qHDӱHD��HD�1HD�qHDԱHD��HD�1HD�qHDձHD��HD�1HD�qHDֱHD��HD�1HD�qHDױHD��HD�1HD�qHDرHD��HD�1HD�qHDٱHD��HD�1HD�qHDڱHD��HD�1HD�qHD۱HD��HD�1HD�qHDܱHD��HD�1HD�qHDݱHD��HD�1HD�qHDޱHD��HD�1HD�qHD߱HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD��HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD�HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�1HD�qHD��HD��HD�4{D�n11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�1A�oA�oA�{A�oA�{A��A�VA�
=A��A��A�oA�bA��A��A��A��A�"�A�"�A�"�A�"�A�$�A�"�A�$�A�$�A�&�A��A�E�A��A��A�ȴA��A���A��A�S�A�oAŗ�A��yAğ�A�n�A�|�A���A�G�A���A�`BA���A�+A��+A�1'A�n�A�=qA��RA��A�?}A�/A�(�A���A���A���A�G�A��A��jA��A�Q�A�VA���A�
=A��jA���A�hsA�ƨA�"�A�ȴA��7A���A��FA�p�A��A�oA�l�A��DA��TA��uA�
=A���A�9XA�z�A��+A�C�A�5?A�=qA��A�;dA�\)A��A��`A��/A�VA��A��PA�ȴA��;A�v�A���A��A�(�A�x�A�%A��A��A�
=A�I�A�ƨA�K�A�1'A�/A�ffA��#A��`A�dZA~�`A|I�AzbNAy�Ax�Aw`BAvbNAs�TApĜAl~�Aj��Ai�;Ai|�Ai�Ah�/Ag�Act�A` �A^�A^ �A\v�AY�AWXAV�AU|�AT�`AT��ATjAS�hARZANffAL��AK�AK�PAK�AI�7AG��AD��ACXABbA@��A?p�A>��A>A�A>1'A>1'A> �A=XA;��A:��A9�A7/A5��A4�A3+A2=qA1|�A0=qA/�^A/7LA-��A+�^A*n�A)hsA'�A&I�A#;dA!�A ZA�A��AA�AC�AM�A�hA�/A��A�HA$�AA��A�A��AbNA��A��A�At�A
��A
�A�A��A�A9XAbA�AAp�A��A7LA�yA�/Av�A�;A ��@�
=@�/@���@��@�5?@���@�@�;d@��@�@�^5@�{@�Z@�x�@웦@�@�dZ@�ȴ@�~�@�V@��#@�Ĝ@�|�@�!@�5?@�&�@��m@�\)@�^5@��@��@ޗ�@�O�@���@�Ĝ@�(�@��@ٲ-@�z�@�S�@��@�-@�A�@��@с@�p�@Ѓ@��@�p�@��/@��
@�-@ɉ7@ɡ�@ɺ^@ɺ^@ɩ�@��`@�A�@ǶF@�S�@�ff@�G�@ģ�@öF@�"�@\@��@�V@�ƨ@��R@�X@���@��j@��@��+@���@��@�  @���@�;d@���@��@� �@���@��H@��!@�ff@�p�@��j@�Q�@��@��@�C�@��@�
=@���@���@�~�@�n�@�E�@�{@�@��h@�G�@�G�@�%@���@���@���@���@�?}@���@�`B@��D@��P@�33@���@�ȴ@���@��R@���@�n�@�p�@�%@��`@��/@��j@�  @���@�l�@�\)@�"�@���@�{@���@��u@�z�@�b@���@��@�33@��@�K�@�33@�5?@�%@��9@�I�@�1@��@�G�@�Ĝ@�r�@���@���@� �@�C�@��@�v�@�E�@�@�hs@�/@�&�@�V@��@��@�A�@� �@��@��F@�t�@�C�@�ȴ@���@�@��@�G�@��@���@��@�;d@�ȴ@���@���@���@��@�+@��H@�n�@�~�@�ff@�E�@�V@�E�@�5?@�ff@�hs@���@�Q�@��@�ƨ@��@��@��P@�l�@�\)@�C�@�33@�"�@��@��@��!@�~�@�V@�{@�@��@��T@�@��^@���@��7@��@��7@�hs@�/@���@� �@��m@���@�+@��w@���@��@��+@�M�@�-@�{@���@��T@���@���@���@���@��-@��^@���@��T@��T@���@��#@���@���@��h@�hs@�7L@�%@��/@��9@���@��@�bN@� �@�ƨ@��@�l�@�33@�33@�33@�33@�;d@�;d@�o@���@�ȴ@��!@�v�@�E�@��-@�x�@�O�@�&�@���@��/@��@��D@�Q�@�  @~V@}�@|��@|1@{@z�\@z�@yx�@yG�@x�`@x�u@w|�@v�y@vȴ@v�R@v�R@v�R@v�R@v��@v��@vv�@vv�@vV@v@u�@u�-@up�@uO�@u?}@u/@t��@t9X@s�@r�@r�!@rn�@r�\@r��@r��@r�@qx�@q�#@q��@q�7@qhs@p��@pQ�@p1'@p�u@p�@o�;@o|�@oK�@n��@n��@m@l��@l�@k�
@kt�@k@k@k@j�@j�!@j=q@jJ@i�^@ihs@i7L@h�`@h��@h1'@h  @g�@g|�@f��@f��@f��@fv�@fV@f{@e�@e�T@e�-@eO�@e/@e�@d��@d�j@dj@dI�@d9X@d1@cƨ@ct�@cC�@co@b�H@b=q@a��@a�7@a�7@a�7@ax�@ahs@a&�@a�@`��@`bN@_�w@_\)@^��@^�@^��@^E�@^@]�T@]�-@]�@]p�@]?}@]/@]V@\�/@\z�@[��@["�@[@Z�H@Z~�@Z�@Y�@Yx�@XĜ@XbN@XA�@X  @W�w@W��@W\)@W+@W
=@V�y@V��@VE�@U�@U�h@U`B@U�@T9X@Sƨ@SS�@S@R~�@R^5@RJ@QX@P��@P�@P �@O�w@O\)@N��@Nȴ@NV@N5?@N@M��@M��@M`B@M/@MV@L��@LI�@Kƨ@Kt�@Ko@J��@J�\@J~�@JM�@I��@I��@Ix�@I&�@HĜ@Hr�@H �@G|�@G;d@F��@F�R@F�+@F�+@FV@F@E@Ep�@E�@D�@D�/@D��@D�D@DI�@D9X@D9X@D�@C�m@Ct�@B��@A��@A��@@�`@@r�@@  @?�P@?l�@?K�@?+@>��@>��@=�-@=O�@=/@=V@<��@<�@<�@<Z@<(�@<1@;�m@;��@;dZ@;"�@;o@:�@:��@:��@:�\@:~�@:^5@9��@9�^@9��@9x�@9X@9G�@9%@8��@8�u@8bN@8A�@8A�@8A�@81'@81'@81'@8b@7;d@7
=@7
=@6�y@6V@6$�@5�@5�-@5?}@4�@4�/@4�j@4Z@4�@3��@3�
@3��@333@3@2�!@2��@2-@1�#@1�^@1�^@1�^@1x�@1hs@17L@0�9@0�@0A�@01'@0 �@0b@/�@/\)@/K�@/;d@/+@.�@.V@.{@-@-`B@-�@,��@+�F@+t�@+S�@+"�@+o@*�@*��@*��@*n�@)�#@)7L@)%@(��@(��@(��@(Ĝ@(bN@'��@'\)@';d@'
=@&��@&V@&E�@&$�@&@%�@%��@%��@%p�@%?}@$�@$�/@$�j@$��@$z�@$9X@#�m@#�@#t�@#C�@"��@"M�@!��@!�7@ �`@ �u@ r�@ A�@  �@   @��@�y@v�@�T@p�@`B@O�@V@�@�j@��@�D@I�@1@�m@ƨ@��@dZ@S�@S�@"�@~�@M�@J@��@�#@x�@&�@%@��@�9@��@A�@1'@1'@b@�@�w@+@�y@�+@$�@�T@�-@�h@p�@�@��@�@��@�j@�j@�@�D@Z@9X@(�@1@�m@�F@dZ@S�@"�@�@��@��@�\@~�@^5@-@�@J@�@��@�^@�^@��@x�@X@G�@%@Ĝ@A�@ �@ �@ �@1'@1'@ �@ �@b@�@�@|�@�@�y@ȴ@��@��@��@��@v�@E�@$�@{@�@�-@�@p�@`B@`B@`B@`B@O�@?}@/11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�1A�oA�oA�{A�oA�{A��A�VA�
=A��A��A�oA�bA��A��A��A��A�"�A�"�A�"�A�"�A�$�A�"�A�$�A�$�A�&�A��A�E�A��A��A�ȴA��A���A��A�S�A�oAŗ�A��yAğ�A�n�A�|�A���A�G�A���A�`BA���A�+A��+A�1'A�n�A�=qA��RA��A�?}A�/A�(�A���A���A���A�G�A��A��jA��A�Q�A�VA���A�
=A��jA���A�hsA�ƨA�"�A�ȴA��7A���A��FA�p�A��A�oA�l�A��DA��TA��uA�
=A���A�9XA�z�A��+A�C�A�5?A�=qA��A�;dA�\)A��A��`A��/A�VA��A��PA�ȴA��;A�v�A���A��A�(�A�x�A�%A��A��A�
=A�I�A�ƨA�K�A�1'A�/A�ffA��#A��`A�dZA~�`A|I�AzbNAy�Ax�Aw`BAvbNAs�TApĜAl~�Aj��Ai�;Ai|�Ai�Ah�/Ag�Act�A` �A^�A^ �A\v�AY�AWXAV�AU|�AT�`AT��ATjAS�hARZANffAL��AK�AK�PAK�AI�7AG��AD��ACXABbA@��A?p�A>��A>A�A>1'A>1'A> �A=XA;��A:��A9�A7/A5��A4�A3+A2=qA1|�A0=qA/�^A/7LA-��A+�^A*n�A)hsA'�A&I�A#;dA!�A ZA�A��AA�AC�AM�A�hA�/A��A�HA$�AA��A�A��AbNA��A��A�At�A
��A
�A�A��A�A9XAbA�AAp�A��A7LA�yA�/Av�A�;A ��@�
=@�/@���@��@�5?@���@�@�;d@��@�@�^5@�{@�Z@�x�@웦@�@�dZ@�ȴ@�~�@�V@��#@�Ĝ@�|�@�!@�5?@�&�@��m@�\)@�^5@��@��@ޗ�@�O�@���@�Ĝ@�(�@��@ٲ-@�z�@�S�@��@�-@�A�@��@с@�p�@Ѓ@��@�p�@��/@��
@�-@ɉ7@ɡ�@ɺ^@ɺ^@ɩ�@��`@�A�@ǶF@�S�@�ff@�G�@ģ�@öF@�"�@\@��@�V@�ƨ@��R@�X@���@��j@��@��+@���@��@�  @���@�;d@���@��@� �@���@��H@��!@�ff@�p�@��j@�Q�@��@��@�C�@��@�
=@���@���@�~�@�n�@�E�@�{@�@��h@�G�@�G�@�%@���@���@���@���@�?}@���@�`B@��D@��P@�33@���@�ȴ@���@��R@���@�n�@�p�@�%@��`@��/@��j@�  @���@�l�@�\)@�"�@���@�{@���@��u@�z�@�b@���@��@�33@��@�K�@�33@�5?@�%@��9@�I�@�1@��@�G�@�Ĝ@�r�@���@���@� �@�C�@��@�v�@�E�@�@�hs@�/@�&�@�V@��@��@�A�@� �@��@��F@�t�@�C�@�ȴ@���@�@��@�G�@��@���@��@�;d@�ȴ@���@���@���@��@�+@��H@�n�@�~�@�ff@�E�@�V@�E�@�5?@�ff@�hs@���@�Q�@��@�ƨ@��@��@��P@�l�@�\)@�C�@�33@�"�@��@��@��!@�~�@�V@�{@�@��@��T@�@��^@���@��7@��@��7@�hs@�/@���@� �@��m@���@�+@��w@���@��@��+@�M�@�-@�{@���@��T@���@���@���@���@��-@��^@���@��T@��T@���@��#@���@���@��h@�hs@�7L@�%@��/@��9@���@��@�bN@� �@�ƨ@��@�l�@�33@�33@�33@�33@�;d@�;d@�o@���@�ȴ@��!@�v�@�E�@��-@�x�@�O�@�&�@���@��/@��@��D@�Q�@�  @~V@}�@|��@|1@{@z�\@z�@yx�@yG�@x�`@x�u@w|�@v�y@vȴ@v�R@v�R@v�R@v�R@v��@v��@vv�@vv�@vV@v@u�@u�-@up�@uO�@u?}@u/@t��@t9X@s�@r�@r�!@rn�@r�\@r��@r��@r�@qx�@q�#@q��@q�7@qhs@p��@pQ�@p1'@p�u@p�@o�;@o|�@oK�@n��@n��@m@l��@l�@k�
@kt�@k@k@k@j�@j�!@j=q@jJ@i�^@ihs@i7L@h�`@h��@h1'@h  @g�@g|�@f��@f��@f��@fv�@fV@f{@e�@e�T@e�-@eO�@e/@e�@d��@d�j@dj@dI�@d9X@d1@cƨ@ct�@cC�@co@b�H@b=q@a��@a�7@a�7@a�7@ax�@ahs@a&�@a�@`��@`bN@_�w@_\)@^��@^�@^��@^E�@^@]�T@]�-@]�@]p�@]?}@]/@]V@\�/@\z�@[��@["�@[@Z�H@Z~�@Z�@Y�@Yx�@XĜ@XbN@XA�@X  @W�w@W��@W\)@W+@W
=@V�y@V��@VE�@U�@U�h@U`B@U�@T9X@Sƨ@SS�@S@R~�@R^5@RJ@QX@P��@P�@P �@O�w@O\)@N��@Nȴ@NV@N5?@N@M��@M��@M`B@M/@MV@L��@LI�@Kƨ@Kt�@Ko@J��@J�\@J~�@JM�@I��@I��@Ix�@I&�@HĜ@Hr�@H �@G|�@G;d@F��@F�R@F�+@F�+@FV@F@E@Ep�@E�@D�@D�/@D��@D�D@DI�@D9X@D9X@D�@C�m@Ct�@B��@A��@A��@@�`@@r�@@  @?�P@?l�@?K�@?+@>��@>��@=�-@=O�@=/@=V@<��@<�@<�@<Z@<(�@<1@;�m@;��@;dZ@;"�@;o@:�@:��@:��@:�\@:~�@:^5@9��@9�^@9��@9x�@9X@9G�@9%@8��@8�u@8bN@8A�@8A�@8A�@81'@81'@81'@8b@7;d@7
=@7
=@6�y@6V@6$�@5�@5�-@5?}@4�@4�/@4�j@4Z@4�@3��@3�
@3��@333@3@2�!@2��@2-@1�#@1�^@1�^@1�^@1x�@1hs@17L@0�9@0�@0A�@01'@0 �@0b@/�@/\)@/K�@/;d@/+@.�@.V@.{@-@-`B@-�@,��@+�F@+t�@+S�@+"�@+o@*�@*��@*��@*n�@)�#@)7L@)%@(��@(��@(��@(Ĝ@(bN@'��@'\)@';d@'
=@&��@&V@&E�@&$�@&@%�@%��@%��@%p�@%?}@$�@$�/@$�j@$��@$z�@$9X@#�m@#�@#t�@#C�@"��@"M�@!��@!�7@ �`@ �u@ r�@ A�@  �@   @��@�y@v�@�T@p�@`B@O�@V@�@�j@��@�D@I�@1@�m@ƨ@��@dZ@S�@S�@"�@~�@M�@J@��@�#@x�@&�@%@��@�9@��@A�@1'@1'@b@�@�w@+@�y@�+@$�@�T@�-@�h@p�@�@��@�@��@�j@�j@�@�D@Z@9X@(�@1@�m@�F@dZ@S�@"�@�@��@��@�\@~�@^5@-@�@J@�@��@�^@�^@��@x�@X@G�@%@Ĝ@A�@ �@ �@ �@1'@1'@ �@ �@b@�@�@|�@�@�y@ȴ@��@��@��@��@v�@E�@$�@{@�@�-@�@p�@`B@`B@`B@`B@O�@?}@/11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BH�BG�BE�B>wB�B\BbBPBuB�B�B �B&�B)�B+B33B=qBB�BH�BL�BQ�B[#B\)BZB`BB_;B[#BW
BXBJ�BI�BI�BH�BF�BB�B?}B=qB;dB8RB49B.B&�B�BhBDBB  B��B��B�B�HB�/B�B��BŢB�!B��B��B��B��B�hB�+Bz�Bk�B]/BO�B<jB'�BbBB��B�yB�/B��B�?B��B��B�Br�BP�B)�B�B�BuB\BB
��B
�B
�BB
��B
�B
�B
t�B
cTB
=qB
#�B
uB
+B
B	��B	�B	�B	�)B	��B	�XB	�-B	�B	�B	��B	��B	��B	�=B	y�B	r�B	m�B	dZB	XB	N�B	I�B	G�B	E�B	D�B	B�B	>wB	8RB	)�B	#�B	 �B	�B	�B	�B	VB	B��B��B�B�B�B�sB�sB�sB�mB�TB�5B�B��B��BǮBĜB��B�qB�dB�XB�LB�9B�B��B��B��B��B��B�oB�PB�DB�7B�+B�B�B� B}�B|�By�By�Bv�Bt�Bp�Bl�BjBgmBe`BdZBcTBbNB`BB]/B[#BYBYBXBXBXBXBXBVBW
BVBT�BS�BT�BT�BQ�BQ�BS�BS�BQ�BP�BQ�BR�BR�BR�BR�BQ�BP�BP�BP�BQ�BP�BP�BP�BP�BO�BO�BP�BQ�BP�BP�BP�BP�BQ�BR�BR�BS�BS�BS�BS�BS�BT�BT�BVBW
BW
BVBT�BVBW
BXBW
BW
BYBYB\)BaHBffBm�Bp�Bp�Bp�Bu�Bw�By�Bz�B|�B� B�B�B�B�+B�=B�PB�bB�oB��B��B��B��B�oB�VB�VB�VB�\B�VB�VB�VB�VB�\B�oB��B��B��B��B��B��B��B�B�'B�XB�wB��BÖBĜBƨBȴB��B��B�#B�5B�HB�TB�`B�mB�yB�B�B��B��B��B	  B	B	1B	
=B	
=B	DB	JB	hB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	#�B	$�B	$�B	$�B	)�B	/B	33B	2-B	9XB	;dB	=qB	A�B	C�B	>wB	>wB	@�B	E�B	H�B	H�B	H�B	N�B	Q�B	S�B	W
B	ZB	]/B	_;B	`BB	aHB	dZB	e`B	e`B	hsB	n�B	p�B	p�B	q�B	r�B	u�B	w�B	x�B	w�B	v�B	u�B	v�B	w�B	x�B	z�B	|�B	� B	�B	�B	�%B	�7B	�DB	�DB	�JB	�VB	�bB	�uB	�oB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�'B	�3B	�?B	�jB	�jB	�dB	�dB	�dB	�qB	�}B	��B	ŢB	ƨB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�)B	�5B	�;B	�;B	�;B	�HB	�TB	�ZB	�`B	�`B	�fB	�fB	�fB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
%B
DB
DB
JB
JB
PB
PB
VB
\B
bB
hB
bB
bB
hB
hB
hB
bB
bB
oB
{B
{B
{B
uB
oB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
 �B
 �B
 �B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
#�B
"�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
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
(�B
(�B
(�B
(�B
)�B
(�B
(�B
(�B
)�B
)�B
+B
+B
+B
,B
,B
,B
-B
-B
.B
.B
.B
.B
/B
/B
/B
/B
/B
/B
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
2-B
33B
33B
49B
49B
49B
49B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
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
A�B
A�B
B�B
B�B
B�B
A�B
B�B
C�B
D�B
D�B
E�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
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
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
L�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
O�B
O�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
P�B
Q�B
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
VB
VB
VB
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
XB
XB
XB
XB
YB
YB
YB
YB
YB
YB
YB
YB
YB
YB
YB
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
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
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
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
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
hsB
hsB
hsB
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
iyB
iyB
iyB
iyB
iyB
jB
jB
k�B
k�B
k�B
jB
jB
jB
k�B
jB
jB
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
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BJBF�BJ�B/,BB�B�BpB�BmB#GB((B*�B.�B9pB?�BD�BJ�BONBX�B]�B]�B]�BaDBa�B]�BZ�B\�BOSBK}BKBL�BH�BDIB@�B>�B<�B9�B6�B1>B-MB�B|B�B�BB��B��B�B��B��B�eB�}B��B�QB��B�CB��B�TB�8B��B;BoPB`�BUmBA�B.�B�B}B��B��B�BЁB��B�B��B�4B{�B[�B-RB�B.B�B�B�B
�sB
�2B
�B
ׅB
�B
�(B
zB
piB
G�B
+B
�B
	�B

B	�4B	�B	�B	��B	�rB	��B	�5B	�7B	�.B	��B	�^B	�pB	�5B	|�B	tkB	q?B	j�B	]qB	Q�B	KDB	I!B	FhB	EHB	D�B	AoB	@�B	.B	%�B	!�B	 	B	CB	�B	�B	�B	 B��B��B��B�B��B�B��B��B�BB�B��B�CB�B�B�RB�-B��B��B��B��B��B�WB��B�B��B�B��B��B�bB�lB�	B�0B�8B�@B��B�B_B|B{�ByhBw�Bv4BpBmeBk_BhBg@BfBd
BbZBa�B]�B\:BZBX�BX�BX�BY1BY�BZ�BXBVcBVLBU�BX�BX5BT�BS�BUzBUDBW�BS�BR�BS�BS�BS~BS�BTfBTwBR9BR=BR�BQ�BQxBQVBQ�BQ�BQ�BR$BR�BR�BR�BQ�BRqBS)BTaBVhBU�BT�BT�BUBU�BWOBWBW�BW�BXYBYBX,BWBWmBY�BY�BY�BZDBZ�B^�BbvBf|Bm�Bp�BqBrBv�Bx�Bz�B|�B~�B�6B��B�)B�?B�UB��B�wB�9B��B�JB�/B��B�yB��B�+B��B��B�B�[B��B�7B�<B�oB��B�2B�B��B��B��B�~B��B��B�(B��B��B��B��B�B�"B�_B�BB�mB�bB��B�B�B�B�B�RB�IB�ZB�!B�XB��B	 �B	hB	vB	
�B	
�B	�B	�B	4B	�B	�B	�B	�B	RB	B	�B	/B	rB	�B	}B	�B	 #B	"�B	$yB	%�B	%�B	%�B	)�B	/�B	4�B	4?B	:	B	<.B	=�B	BkB	F\B	?MB	?B	@�B	E�B	I�B	I�B	I{B	O�B	RkB	T�B	W�B	Z�B	]wB	_�B	`�B	a�B	eB	e�B	e�B	h�B	o/B	qB	q�B	r�B	s9B	vVB	x\B	y�B	x�B	w�B	v�B	w�B	xB	x�B	{B	|�B	�,B	��B	��B	�KB	��B	��B	�hB	��B	��B	�hB	�B	��B	�B	�	B	�.B	��B	��B	�B	�B	�	B	�B	�B	�B	�B	�CB	�pB	�_B	�]B	��B	�NB	�\B	�PB	�tB	�gB	�B	��B	�zB	�uB	��B	�B	�7B	��B	�B	�(B	�NB	��B	�B	��B	�|B	�?B	�'B	�"B	�'B	�/B	�-B	�,B	�'B	�+B	�KB	�B	�B	�5B	�TB	�oB	�[B	މB	ߨB	ߝB	߷B	��B	��B	��B	��B	��B	��B	��B	�B	�)B	��B	�B	�B	��B	��B	��B	��B	��B	�B	�4B	��B	�B	�6B	�8B	��B	�UB	�9B	�FB	�DB	�8B	�LB	�9B	�[B	��B	�DB	��B	�]B	��B	��B	�ZB	�\B	��B	�?B	�bB	�YB	��B	��B	�GB	�@B
 ;B
 <B
 :B
 HB
NB
WB
DB
aB
�B
mB
�B
�B
�B
�B
�B
�B
!B
B
 B
�B
�B
�B
�B
�B
 B
B
ZB
�B
�B
�B
B
+B
�B
lB
�B
7B
B
�B
B
B
jB
�B
cB
B
/B
=B
�B
�B
�B
B
?B
B
(B
)B
B
,B
$B
NB
B
B
QB
_B
 IB
�B
 B
 B
 /B
!B
!B
!'B
"TB
""B
"B
"(B
"?B
"IB
"%B
"B
#3B
#DB
#OB
$=B
#5B
$=B
$�B
%�B
%9B
%B
%B
%&B
%)B
%PB
&-B
&UB
&B
&�B
'uB
(vB
(IB
(bB
(oB
(aB
)KB
)\B
)YB
)@B
)YB
*FB
)OB
)ZB
)�B
*�B
*�B
+[B
+^B
+�B
,�B
,pB
,�B
-�B
-�B
.gB
.zB
.xB
.iB
/�B
/tB
/eB
/lB
/�B
/�B
0�B
0�B
0|B
0�B
0�B
1�B
1�B
2�B
2�B
3�B
2�B
3�B
3�B
4�B
4�B
4�B
4�B
5�B
5�B
5�B
6�B
6�B
6�B
6�B
6�B
6�B
7�B
7�B
7�B
7�B
8�B
8�B
8�B
8�B
8�B
9�B
9�B
9�B
9�B
9�B
9�B
:�B
:�B
;B
;�B
;�B
;�B
<�B
<�B
<�B
<�B
<�B
<�B
=�B
=�B
=�B
=�B
=�B
=�B
>�B
>�B
>�B
>�B
?B
?DB
?MB
@B
@7B
AB
AB
BB
A�B
B�B
B�B
B�B
BB
CZB
DB
D�B
D�B
E�B
D�B
F	B
FB
FB
E�B
F�B
GB
GB
GB
F�B
F�B
F�B
GB
F�B
G�B
HB
H4B
HB
G�B
IB
IB
JB
J)B
J>B
JB
JB
JB
I�B
I�B
J B
I�B
I�B
JB
J�B
K%B
KB
KB
KlB
L*B
L*B
L;B
MeB
MKB
MB
M'B
MVB
M?B
N)B
N)B
NGB
NZB
M1B
MKB
MB
N�B
NMB
N+B
OB
OB
OHB
P)B
PBB
O}B
O?B
PLB
P+B
P'B
P+B
P:B
Q�B
R7B
R5B
R4B
RlB
R�B
RYB
ReB
SxB
SfB
T�B
T�B
UkB
UUB
U_B
UEB
UPB
UWB
U`B
UbB
U�B
V�B
VcB
VKB
VAB
V[B
VOB
V�B
V�B
W�B
W^B
WlB
X�B
X�B
XYB
X`B
YlB
Y`B
YlB
Y|B
YyB
YyB
Y�B
Y`B
YjB
YiB
YkB
Y�B
Z�B
Z�B
ZhB
Z}B
Z�B
[�B
[�B
[�B
[�B
\�B
\}B
\�B
\|B
\�B
\�B
\�B
]�B
]�B
^�B
^~B
_�B
_�B
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
`�B
`�B
`�B
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
c�B
c�B
c�B
c�B
c�B
c�B
d�B
d�B
d�B
d�B
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
h�B
h�B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
kB
j�B
k�B
k�B
k�B
j�B
j�B
j�B
k�B
j�B
j�B
k�B
lB
k�B
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
nB
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<V��<���<#�
<-��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<'M�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<(;<?�7<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<>�<o�<#�
<#�
<`Ź<6�M<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<K��<#�
<#�
<#�
<#�
<#�
<#�
</mT<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =0.46 dbar                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CTM alpha = 0.141 & tau = 6.68 s with error equal to the correction                                                                                                                                                                                             Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  201603101135042016031011350420160310113504  AO  ARCAADJP                                                                    20140721233921    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721233921  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721233921  QCF$                G�O�G�O�G�O�0                                                                                                                                   G�O�G�O�G�O�                PM  ARSQPADJV1.1                                                                20160310113504  QC  PRES            @�33D�|�G�O�                PM  ARSQCTM V1.1                                                                20160310113504  QC  PSAL            @�33D�|�G�O�                PM  ARSQOWGUV1.0WOD + Argo                                                      20170523133337  IP                  G�O�G�O�G�O�                