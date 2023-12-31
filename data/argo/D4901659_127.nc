CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-01-20T18:03:44Z creation      
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
resolution        >�E�vQ�   
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
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
resolution        =���   axis      Z        l  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  `    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  ol   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  �h   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ۔   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ޔ   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
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
_FillValue                    �@        �@Argo profile    3.1 1.2 19500101000000  20180120180344  20230721230915  4901659 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               A   AO  5382                            2C  D   NAVIS_A                         0371                            082713                          863 @�F`Æ1   @�F��ɀ@<?|�h�c�-V1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      A   A   A   @�33@�  A   A   AA��A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DOy�DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dvy�Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @w�@���@���AQ�A=�A\Q�A|Q�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B{B{B{B{B'{B/{B7{B?{BG{BO{BW{B_{Bg{Bo{Bw{B{B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=BÊ=BǊ=Bˊ=Bϊ=Bӊ=B׊=Bۊ=Bߊ=B�=B�=B�=B�=B�=B��=B��=B��=C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��C��C��C��C��\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D qHD �HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HD	qHD	�HD
qHD
�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD��DqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HD qHD �HD!qHD!�HD"qHD"�HD#qHD#�HD$qHD$�HD%qHD%�HD&qHD&�HD'qHD'�HD(qHD(�HD)qHD)�HD*qHD*�HD+qHD+�HD,qHD,�HD-qHD-�HD.qHD.�HD/qHD/�HD0qHD0�HD1qHD1�HD2qHD2�HD3qHD3�HD4qHD4�HD5qHD5�HD6qHD6�HD7qHD7�HD8qHD8�HD9qHD9�HD:qHD:�HD;qHD;�HD<qHD<�HD=qHD=�HD>qHD>�HD?qHD?�HD@qHD@�HDAqHDA�HDBqHDB�HDCqHDC�HDDqHDD�HDEqHDE�HDFqHDF�HDGqHDG�HDHqHDH�HDIqHDI�HDJqHDJ�HDKqHDK�HDLqHDL�HDMqHDM�HDNqHDN�HDOj�DO�HDPqHDP�HDQqHDQ�HDRqHDR�HDSqHDS�HDTqHDT�HDUqHDU�HDVqHDV�HDWqHDW�HDXqHDX�HDYqHDY�HDZqHDZ�HD[qHD[�HD\qHD\�HD]qHD]�HD^qHD^�HD_qHD_�HD`qHD`�HDaqHDa�HDbqHDb�HDcqHDc�HDdqHDd�HDeqHDe�HDfqHDf�HDgqHDg�HDhqHDh�HDiqHDi�HDjqHDj�HDkqHDk�HDlqHDl�HDmqHDm�HDnqHDn�HDoqHDo�HDpqHDp�HDqqHDq�HDrqHDr�HDsqHDs�HDtqHDt�HDuqHDu�HDvj�Dv�HDwqHDw�HDxqHDx�HDyqHDy�HDzqHDz�HD{qHD{�HD|qHD|�HD}qHD}�HD~qHD~�HDqHD�HD�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D��qD���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D¸�D���D�8�D�x�Dø�D���D�8�D�x�Dĸ�D���D�8�D�x�DŸ�D���D�8�D�x�DƸ�D���D�8�D�x�DǸ�D���D�8�D�x�Dȸ�D���D�8�D�x�Dɸ�D���D�8�D�x�Dʸ�D���D�8�D�x�D˸�D���D�8�D�x�D̸�D���D�8�D�x�D͸�D���D�8�D�x�Dθ�D���D�8�D�x�Dϸ�D���D�8�D�x�Dи�D���D�8�D�x�DѸ�D���D�8�D�x�DҸ�D���D�8�D�x�DӸ�D���D�8�D�x�DԸ�D���D�8�D�x�Dո�D���D�8�D�x�Dָ�D���D�8�D�x�D׸�D���D�8�D�x�Dظ�D���D�8�D�x�Dٸ�D���D�8�D�x�Dڸ�D���D�8�D�x�D۸�D���D�8�D�x�Dܸ�D���D�8�D�x�Dݸ�D���D�8�D�x�D޸�D���D�8�D�x�D߸�D���D�8�D�x�DฤD���D�8�D�x�DḤD���D�8�D�x�D⸤D���D�8�D�x�D㸤D���D�8�D�x�D两D���D�8�D�x�D帤D���D�8�D�x�D渤D���D�8�D�x�D縤D���D�8�D�x�D踤D���D�8�D�x�D鸤D���D�8�D�x�D긤D���D�8�D�x�D븤D���D�8�D�x�D츤D���D�8�D�x�D���D���D�8�D�x�DD���D�8�D�x�D︤D���D�8�D�x�D�D���D�8�D�x�D�D���D�8�D�x�D�D���D�8�D�x�D�qD���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�{�D���D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A��A��A��A��A���A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��hA��DA�z�A�
=A�^5A�ĜA��PA��jA�;dA��jA��A�C�A��A�A��
A�-A�=qA�bNA�\)A�S�A�=qA�`BA���A�=qA���A�A�A�x�A��`A��A}�;A|�Az��Ay`BAw�-Av�At�9At �As�-Ar1Ap5?AoK�Am�Am�AmVAl(�Ak+Aj�Ai�Ai\)Ah~�Ag�
Ag�PAgG�Ad��Ac?}Abn�AahsA`�\A_��A]�mA]\)A\=qAZ�`AY��AY�AX-AWl�AV�HAU�AT��ATM�AS�7AS�ASAR=qAQ��AQO�AP��AP�AO7LANQ�AM��AL~�AJ�`AI�;AI�AH�jAHM�AHbAGXAGAF5?AD��AC�PABĜABE�ABAA+A?�mA>~�A=��A<bNA;VA:��A:z�A:9XA9��A9C�A8�uA7�A7%A6A�A5ƨA5`BA4��A4�A4��A4~�A4Q�A3��A2��A21A1x�A17LA0�HA/�^A.n�A-��A,�!A+XA*�DA*^5A)��A(�DA'��A&�uA& �A%�TA%�A%
=A$bNA$�A#�mA#�wA#�PA#`BA"��A"�A"��A"E�A!��A VA��AC�A�A(�At�A�/A�
A%A$�A�7AG�A7LA�AM�AoA��A(�A��A�\A{A�`A��A�A��A  AA�!A1A�mA�A�FAt�AhsAG�AoA�DA
=A
{A	�wA	�A�uA�7A�AZA��Ax�A�A(�A�A�PA?}A�AbNAƨAoA 1'@��D@�E�@���@�9X@�l�@�E�@�%@��@��D@��@�  @�l�@��@��@�@���@�!@�J@ᙚ@�G�@�%@�Q�@�dZ@�^5@ܼj@ۅ@�hs@�7L@�(�@և+@ӕ�@с@Ϯ@�ff@�O�@� �@�dZ@��@�$�@�p�@ȓu@ǝ�@�=q@ċD@å�@�\)@��@�^5@���@��@�z�@��!@�@��u@�\)@�O�@�ƨ@��R@���@�&�@��u@���@�"�@�=q@���@�X@�/@�V@���@�n�@�-@��-@��@���@�A�@��m@���@��/@��@���@�v�@��@��T@��^@��-@�X@��@��j@��D@�C�@��@��@�G�@��@�;d@���@�=q@�{@��@��^@���@���@�J@��7@�/@���@�z�@���@�
=@��@�%@���@�j@�b@�ƨ@��@�C�@���@�E�@��7@��/@��D@��@��F@�|�@�33@�
=@��y@���@��+@�n�@�M�@�`B@��u@�r�@�A�@�  @�C�@��@��\@���@�X@�&�@���@��@���@��@��D@�bN@�A�@��@���@�J@��@�/@�%@��@�Ĝ@��D@�1@��@��@��y@�{@��-@�?}@�&�@��@��@�j@��@��@�w@\)@~��@~�+@~V@}�@|�@|�j@|�@|��@|��@|��@|(�@{�@z^5@yx�@yG�@y�@x�`@x�9@xb@w�@w��@w�P@wl�@w\)@w+@vȴ@u��@up�@u?}@u/@u�@t�/@t�D@t�D@t�D@t�@u�@t��@tI�@r�@r�!@s"�@s�@sƨ@t1@t9X@t9X@s��@so@r��@r-@rJ@qx�@q&�@p��@p1'@pr�@q��@q��@rJ@rM�@r~�@r�!@r^5@q��@p��@p�9@pQ�@o�@o\)@oK�@o+@n��@n��@nff@n5?@m�T@mO�@l��@l��@lZ@l1@k�
@k�m@k�
@k�@kC�@ko@j�@j��@j~�@j�@jJ@i��@iG�@hr�@h  @g�@g�P@g\)@fȴ@fV@e@d��@dz�@d9X@d1@cƨ@c��@b�@b��@bM�@a��@ahs@`�9@`bN@`A�@_�w@_+@^�@^�R@^�R@^�R@^�+@^V@^$�@]��@]p�@\��@\Z@\�@\1@[�
@[��@[t�@["�@Z��@ZM�@Z-@Y��@Y��@Yx�@Y7L@X��@X��@X�9@Xr�@X��@X�u@XA�@Xb@W�;@W��@W\)@V��@Vv�@V5?@U��@Up�@U�@T��@TZ@TI�@T1@SS�@R�!@R~�@Q�^@Q%@PĜ@P��@P�@P�@P�@P�@Pr�@PbN@PbN@PQ�@PA�@P1'@Pb@O��@O�w@O��@O�;@O�w@O;d@N�y@N��@Nff@M�@Mp�@M/@L�@L�D@Lj@L(�@K�F@Kt�@K33@K"�@Ko@J�H@J��@J��@J��@J��@J��@J��@JM�@J=q@J-@J-@J�@I�@I�#@I��@I%@H�u@G|�@G
=@F�@FE�@E��@EO�@D�/@DI�@D�@D1@C��@C�m@C�
@C�@B�@Bn�@BM�@B-@B�@A��@A�^@A�7@Ahs@A�@A%@@�`@@��@@�`@@��@@Ĝ@@1'@?��@?|�@?;d@>��@>$�@=�T@=�-@=�@=p�@=`B@=`B@=`B@=`B@=`B@=O�@=�@<�@<I�@;�m@;t�@:~�@9��@9�7@9&�@8��@8��@7��@7�@7�@7|�@7|�@7\)@7;d@7�@7�@6ff@6@5@5p�@5?}@4�@4�@4�D@49X@41@3�
@3��@3dZ@333@3@2��@2~�@2^5@2J@1��@1&�@0��@0��@0r�@0Q�@0  @/�;@/��@.�y@.��@.v�@.V@.E�@.E�@.{@-�-@-`B@,��@,�D@,j@,Z@,I�@,I�@,I�@,9X@+�F@+33@*�!@*�\@*=q@)��@)&�@(�`@(Ĝ@(�@(r�@(bN@(bN@(Q�@(1'@( �@( �@(b@'��@'K�@'+@'+@'�@&�y@&��@&v�@&@%�@%�@%p�@%�@%p�@%O�@$�/@$�D@$�@#�F@#�@#�@#t�@#dZ@#dZ@#C�@#33@#o@"�@"�H@"�\@!��@!�#@!�^@!x�@!G�@!&�@!&�@!�@ �`@ r�@ b@   @   @�;@�@l�@+@�@ȴ@��@v�@ff@ff@ff@ff@V@V@5?@5?@@�@�@�@�@"�@�@�!@�\@~�@n�@M�@=q@�@��@�@�@Ĝ@�9@��@bN@1'@�@�w@l�@K�@+@�@
=@�@��@E�@5?@5?@{@@�T@��@�-@�@V@�@z�@j@j@Z@Z@1@��@��@C�@�@�H@��@��@�\@n�@M�@=q@��@��@J@J@��@�#@��@x�@�@��@�9@��@Q�@ �@��@��@�y@��@v�@v�@ff@V@E�@E�@E�@E�@5?@{@�-@?}@/@�@V@�@��@�@1@��@�m@�
@�F@�@S�@33@
��@
��@
�!@
��@
~�@
n�@
^5@
=q@
-@
J@	�#@	�#@	�^@	��@	��@	x�@	G�@	�@��@��@�u@r�@r�@r�@bN@A�@ �@b@  @�;@��@�w@��@|�@
=@�y@�y@�y@�y@ȴ@�R@�R@��@��@��@ff@{@@�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A��A��A��A��A���A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��hA��DA�z�A�
=A�^5A�ĜA��PA��jA�;dA��jA��A�C�A��A�A��
A�-A�=qA�bNA�\)A�S�A�=qA�`BA���A�=qA���A�A�A�x�A��`A��A}�;A|�Az��Ay`BAw�-Av�At�9At �As�-Ar1Ap5?AoK�Am�Am�AmVAl(�Ak+Aj�Ai�Ai\)Ah~�Ag�
Ag�PAgG�Ad��Ac?}Abn�AahsA`�\A_��A]�mA]\)A\=qAZ�`AY��AY�AX-AWl�AV�HAU�AT��ATM�AS�7AS�ASAR=qAQ��AQO�AP��AP�AO7LANQ�AM��AL~�AJ�`AI�;AI�AH�jAHM�AHbAGXAGAF5?AD��AC�PABĜABE�ABAA+A?�mA>~�A=��A<bNA;VA:��A:z�A:9XA9��A9C�A8�uA7�A7%A6A�A5ƨA5`BA4��A4�A4��A4~�A4Q�A3��A2��A21A1x�A17LA0�HA/�^A.n�A-��A,�!A+XA*�DA*^5A)��A(�DA'��A&�uA& �A%�TA%�A%
=A$bNA$�A#�mA#�wA#�PA#`BA"��A"�A"��A"E�A!��A VA��AC�A�A(�At�A�/A�
A%A$�A�7AG�A7LA�AM�AoA��A(�A��A�\A{A�`A��A�A��A  AA�!A1A�mA�A�FAt�AhsAG�AoA�DA
=A
{A	�wA	�A�uA�7A�AZA��Ax�A�A(�A�A�PA?}A�AbNAƨAoA 1'@��D@�E�@���@�9X@�l�@�E�@�%@��@��D@��@�  @�l�@��@��@�@���@�!@�J@ᙚ@�G�@�%@�Q�@�dZ@�^5@ܼj@ۅ@�hs@�7L@�(�@և+@ӕ�@с@Ϯ@�ff@�O�@� �@�dZ@��@�$�@�p�@ȓu@ǝ�@�=q@ċD@å�@�\)@��@�^5@���@��@�z�@��!@�@��u@�\)@�O�@�ƨ@��R@���@�&�@��u@���@�"�@�=q@���@�X@�/@�V@���@�n�@�-@��-@��@���@�A�@��m@���@��/@��@���@�v�@��@��T@��^@��-@�X@��@��j@��D@�C�@��@��@�G�@��@�;d@���@�=q@�{@��@��^@���@���@�J@��7@�/@���@�z�@���@�
=@��@�%@���@�j@�b@�ƨ@��@�C�@���@�E�@��7@��/@��D@��@��F@�|�@�33@�
=@��y@���@��+@�n�@�M�@�`B@��u@�r�@�A�@�  @�C�@��@��\@���@�X@�&�@���@��@���@��@��D@�bN@�A�@��@���@�J@��@�/@�%@��@�Ĝ@��D@�1@��@��@��y@�{@��-@�?}@�&�@��@��@�j@��@��@�w@\)@~��@~�+@~V@}�@|�@|�j@|�@|��@|��@|��@|(�@{�@z^5@yx�@yG�@y�@x�`@x�9@xb@w�@w��@w�P@wl�@w\)@w+@vȴ@u��@up�@u?}@u/@u�@t�/@t�D@t�D@t�D@t�@u�@t��@tI�@r�@r�!@s"�@s�@sƨ@t1@t9X@t9X@s��@so@r��@r-@rJ@qx�@q&�@p��@p1'@pr�@q��@q��@rJ@rM�@r~�@r�!@r^5@q��@p��@p�9@pQ�@o�@o\)@oK�@o+@n��@n��@nff@n5?@m�T@mO�@l��@l��@lZ@l1@k�
@k�m@k�
@k�@kC�@ko@j�@j��@j~�@j�@jJ@i��@iG�@hr�@h  @g�@g�P@g\)@fȴ@fV@e@d��@dz�@d9X@d1@cƨ@c��@b�@b��@bM�@a��@ahs@`�9@`bN@`A�@_�w@_+@^�@^�R@^�R@^�R@^�+@^V@^$�@]��@]p�@\��@\Z@\�@\1@[�
@[��@[t�@["�@Z��@ZM�@Z-@Y��@Y��@Yx�@Y7L@X��@X��@X�9@Xr�@X��@X�u@XA�@Xb@W�;@W��@W\)@V��@Vv�@V5?@U��@Up�@U�@T��@TZ@TI�@T1@SS�@R�!@R~�@Q�^@Q%@PĜ@P��@P�@P�@P�@P�@Pr�@PbN@PbN@PQ�@PA�@P1'@Pb@O��@O�w@O��@O�;@O�w@O;d@N�y@N��@Nff@M�@Mp�@M/@L�@L�D@Lj@L(�@K�F@Kt�@K33@K"�@Ko@J�H@J��@J��@J��@J��@J��@J��@JM�@J=q@J-@J-@J�@I�@I�#@I��@I%@H�u@G|�@G
=@F�@FE�@E��@EO�@D�/@DI�@D�@D1@C��@C�m@C�
@C�@B�@Bn�@BM�@B-@B�@A��@A�^@A�7@Ahs@A�@A%@@�`@@��@@�`@@��@@Ĝ@@1'@?��@?|�@?;d@>��@>$�@=�T@=�-@=�@=p�@=`B@=`B@=`B@=`B@=`B@=O�@=�@<�@<I�@;�m@;t�@:~�@9��@9�7@9&�@8��@8��@7��@7�@7�@7|�@7|�@7\)@7;d@7�@7�@6ff@6@5@5p�@5?}@4�@4�@4�D@49X@41@3�
@3��@3dZ@333@3@2��@2~�@2^5@2J@1��@1&�@0��@0��@0r�@0Q�@0  @/�;@/��@.�y@.��@.v�@.V@.E�@.E�@.{@-�-@-`B@,��@,�D@,j@,Z@,I�@,I�@,I�@,9X@+�F@+33@*�!@*�\@*=q@)��@)&�@(�`@(Ĝ@(�@(r�@(bN@(bN@(Q�@(1'@( �@( �@(b@'��@'K�@'+@'+@'�@&�y@&��@&v�@&@%�@%�@%p�@%�@%p�@%O�@$�/@$�D@$�@#�F@#�@#�@#t�@#dZ@#dZ@#C�@#33@#o@"�@"�H@"�\@!��@!�#@!�^@!x�@!G�@!&�@!&�@!�@ �`@ r�@ b@   @   @�;@�@l�@+@�@ȴ@��@v�@ff@ff@ff@ff@V@V@5?@5?@@�@�@�@�@"�@�@�!@�\@~�@n�@M�@=q@�@��@�@�@Ĝ@�9@��@bN@1'@�@�w@l�@K�@+@�@
=@�@��@E�@5?@5?@{@@�T@��@�-@�@V@�@z�@j@j@Z@Z@1@��@��@C�@�@�H@��@��@�\@n�@M�@=q@��@��@J@J@��@�#@��@x�@�@��@�9@��@Q�@ �@��@��@�y@��@v�@v�@ff@V@E�@E�@E�@E�@5?@{@�-@?}@/@�@V@�@��@�@1@��@�m@�
@�F@�@S�@33@
��@
��@
�!@
��@
~�@
n�@
^5@
=q@
-@
J@	�#@	�#@	�^@	��@	��@	x�@	G�@	�@��@��@�u@r�@r�@r�@bN@A�@ �@b@  @�;@��@�w@��@|�@
=@�y@�y@�y@�y@ȴ@�R@�R@��@��@��@ff@{@@�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB%B+B%B+B+B+B%B%B%B%B%B%B%B%B%B%B%B%B%B%B%B%BBBBBBBBBBBBBBBBB+B+B+B+B+B1B1B+BB��B�NB��Bk�BA�B!�BB�B�B��B�HB�XB�7Be`BA�B �BbB
��B
�`B
�#B
��B
��B
ŢB
�RB
��B
��B
�+B
{�B
r�B
hsB
`BB
VB
M�B
I�B
E�B
:^B
+B
&�B
�B
�B
�B
oB
PB
	7B
B
B	��B	��B	�B	�B	�HB	�
B	��B	��B	ƨB	�}B	�?B	�!B	��B	��B	��B	�uB	�PB	�1B	�B	}�B	x�B	u�B	q�B	p�B	m�B	hsB	dZB	aHB	^5B	[#B	VB	P�B	M�B	I�B	B�B	=qB	:^B	7LB	49B	33B	/B	-B	'�B	�B	�B	�B	{B	oB	PB	+B	B��B��B�B�B�B�B�B�yB�fB�TB�;B�)B�B�B�B��B��B��B��B��B��BɺBǮBƨBÖB�}B�dB�RB�?B�'B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�bB�VB�PB�DB�7B�+B�B�B� B}�B|�B{�Bz�By�Bu�Bq�Bn�Bk�BhsBffBdZBaHB^5B\)B[#BYBW
BVBT�BT�BS�BS�BR�BR�BQ�BP�BN�BL�BJ�BI�BH�BF�BE�BD�BC�BB�BA�B@�B@�B?}B>wB=qB<jB;dB9XB6FB49B2-B1'B0!B.B.B-B,B+B)�B)�B(�B&�B$�B �B�B�B�B�B�B�B�B�B�B�B�B{BuBoBhBbB\BbBbBbBbBbBbBbBbBbBbBbBhBoBoBoBoBoBuBoBoB{B{B�B�B�B�B�B�B�B�B"�B#�B%�B&�B'�B'�B&�B+B,B,B-B.B.B/B/B1'B5?B7LB7LB<jB=qB>wB>wB>wB?}B@�BA�BA�BE�BK�BN�BN�BQ�BVBVBW
BYBZB^5BcTBdZBjBk�Bl�Bo�Bq�Bu�Bw�Bz�B}�B~�B�B�B�%B�1B�1B�DB�PB�bB�oB�uB��B��B��B��B��B��B��B��B��B��B��B�-B�?B�LB�XB�}BBÖBŢBƨBƨBǮBǮBǮBȴBȴB��B��B��B�B�/B�BB�TB�`B�fB�mB�sB�B�B�B�B��B��B��B	  B	  B	B	%B		7B	DB	DB	PB	bB	bB	hB	uB	�B	�B	�B	�B	�B	�B	�B	�B	$�B	(�B	)�B	+B	,B	-B	0!B	2-B	33B	33B	49B	5?B	5?B	7LB	=qB	>wB	@�B	@�B	A�B	D�B	J�B	L�B	N�B	Q�B	W
B	W
B	XB	YB	\)B	^5B	aHB	bNB	e`B	gmB	hsB	hsB	hsB	hsB	k�B	n�B	q�B	r�B	u�B	x�B	|�B	�B	�B	�B	�+B	�7B	�DB	�VB	�bB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�FB	�FB	�LB	�RB	�XB	�^B	�jB	�qB	�qB	�wB	��B	ÖB	ĜB	ĜB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�5B	�;B	�BB	�HB	�NB	�TB	�ZB	�`B	�fB	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
+B
+B
%B
%B
+B
+B
+B
+B
+B
+B
+B
1B

=B

=B
DB
JB
PB
PB
PB
VB
\B
bB
hB
hB
hB
oB
oB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
$�B
$�B
%�B
&�B
&�B
%�B
%�B
&�B
'�B
'�B
+B
+B
+B
,B
,B
,B
,B
,B
,B
.B
/B
/B
/B
0!B
1'B
2-B
2-B
2-B
2-B
33B
33B
49B
49B
49B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
7LB
7LB
7LB
8RB
8RB
8RB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
;dB
<jB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
=qB
>wB
?}B
@�B
@�B
A�B
A�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
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
J�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
O�B
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
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
S�B
T�B
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
XB
XB
XB
XB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
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
_;B
`BB
`BB
`BB
`BB
`BB
`BB
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
cTB
cTB
cTB
dZB
dZB
e`B
e`B
e`B
e`B
ffB
ffB
ffB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
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
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BBB<BEB7B<B5BB9B9B1B;B@B1B1B@B1B4B;B;B;B@B[B"BVBGB"B1BKBCB3B*B*B(B*BB3B*B@BFB>BAB<BJBqBkB�B�B��B�:B��BrjBGB'IB	/B�KB�|B�B��B�QB�LBm+BJ@B%ZBBTB
��B
�3B
��B
��B
əB
��B
��B
�UB
�pB
zB
v�B
l�B
d0B
Y�B
OKB
K
B
I�B
>�B
-XB
*<B
qB
CB
B
B
B

�B
�B
HB	��B	��B	��B	�:B	�B	�~B	��B	�TB	ɘB	�tB	�6B	��B	�B	��B	�B	�^B	��B	�B	�*B	�B	{"B	xB	q�B	rbB	o�B	j{B	evB	cuB	`*B	]�B	X�B	S-B	Q"B	NYB	E�B	>�B	<�B	8�B	5B	5kB	05B	/�B	,UB	"�B	�B	�B	^B	�B	�B	
;B	�B	B��B�B�B�>B�B�B�B��B�sB�B�TB� B٣B�MB�KB�UBԊB��BѺB��B�,B�jBǱBƁB��B��B��B��B�TB��B�+B��B�4B�$B� B��B��B��B��B��B�ZB�8B�QB�FB��B�B�=B��B��B��B�GB��B��B��B�6B��B�B�aB��B�B}�B|6B{�B{�ByEBsBpBn�Bi�Bg�Bg�BeB_�B]�B]0B\BX-BXBUuBUBT�BT�BS4BSsBR�BR�BS5BO�BK�BK�BJ^BI�BGBFyBE�BCRBCyBB�BAWB@�B?zB>�B=�B=JB;�B9pB:B5�B2ZB2PB/sB/�B/B.jB/�B*�B*@B)�B)�B(�B''B �B#B�BGBBB�B�B�B�BB�B�B�B�B�B�B�BB�B�BhBB}BaB�B�BMB�B�B�B@B6BVBB�B�B�BKBhB�B�BHBEB�B�B!8B#�B%LB&�B'yB(@B(:B)UB+�B,nB,�B-�B.�B.�B/�B0�B3gB6JB7�B9B<�B=�B>�B>�B?B@"B@�BA�BCFBGaBLoBO@BO�BS�BV�BV�BWZBY0BZ�B_>Bd�Bf�BkJBlBm&Bp.Br�Bv�By{B|*B~^B�B��B��B��B��B��B�'B��B�wB�B�<B�(B�B�B�B��B�B�"B��B�B�=B�6B�~B��B��B��B�9B�*B�B�*B�B�B��B��B�B�B�B�B�B�]B�B�-B��B�B�B��B��B�\B�-B��B�B�B��B��B�>B	 >B	 �B	�B	�B		�B	pB	�B	�B	�B	�B	�B	JB	�B	�B	�B	�B	�B	B	)B	�B	%B	)/B	*;B	+@B	,IB	-�B	0{B	2WB	3`B	3gB	4bB	5�B	5�B	8&B	=�B	>�B	@�B	@�B	A�B	D�B	J�B	L�B	N�B	Q�B	WAB	W�B	YB	Y]B	\B	^B	a=B	bCB	eaB	g�B	h�B	h�B	h�B	h�B	k�B	oB	q�B	sB	v*B	x�B	|GB	�B	�B	�B	�+B	�5B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�7B	�5B	��B	�B	�!B	�B	��B	��B	�-B	�%B	�!B	�B	�$B	�8B	�KB	� B	�DB	�iB	��B	�sB	�_B	�GB	�ZB	��B	��B	��B	�B	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�+B	�B	��B	��B	��B	�B	�B	�B	�"B	�5B	�JB	�sB	�;B	�B	�7B	�HB	�?B	�`B	�B	�jB	�NB	�xB	�]B	�\B	�wB	�sB	�jB	�rB	��B	�FB	�{B	�B	�B	�B	�B	�B	��B	��B	��B	�B	��B	��B	�B	��B	�B	��B	�>B	�3B	��B	�_B	�\B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	��B	��B	��B	�B	�`B	�@B	�8B	�:B	��B	�!B
YB
�B
JB
LB
gB
�B
cB
sB
RB
VB
pB
wB
HB
FB
GB
JB
JB
�B
WB
WB
HB
VB
iB
XB
lB
�B

�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
=B
B
B
 B
 BB
!?B
"B
"B
"B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#B
$MB
%@B
%FB
&\B
'�B
'�B
&B
&CB
'AB
(2B
(�B
+B
+NB
+DB
,+B
,?B
,?B
,>B
,,B
,�B
.{B
/lB
/wB
/_B
0�B
1sB
2iB
2�B
2qB
2tB
3xB
3�B
4zB
4xB
4B
5�B
5nB
5�B
5�B
6�B
6�B
6�B
7�B
7�B
7�B
8�B
8�B
8�B
9�B
:�B
:�B
:�B
:{B
:�B
:�B
;�B
<�B
=�B
=�B
>�B
>�B
>�B
>�B
>�B
=�B
>�B
?�B
@�B
@�B
A�B
BB
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
EB
E�B
E�B
E�B
E�B
E�B
F�B
GB
G$B
G�B
G�B
G�B
G�B
G�B
H(B
IB
I(B
J&B
KB
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
L,B
LWB
MB
M	B
MB
NB
N	B
M�B
NB
NB
OTB
OHB
PB
O�B
PB
P&B
P3B
Q;B
QHB
RB
R(B
R/B
RB
R
B
RB
RB
RB
RB
R%B
RB
R7B
RtB
S�B
T�B
U�B
VpB
WRB
W]B
WBB
X>B
X=B
XHB
X<B
XIB
XIB
XDB
X�B
Z|B
ZHB
ZIB
ZnB
ZcB
[uB
[pB
[�B
\_B
\aB
\VB
\WB
\mB
\sB
]�B
]WB
]MB
]gB
^dB
^qB
^bB
^qB
^|B
^�B
_�B
_wB
`iB
``B
`hB
`_B
`�B
`�B
agB
a�B
a�B
byB
b�B
b�B
bwB
b�B
b�B
c{B
c�B
csB
cfB
csB
c|B
c�B
cB
c�B
d�B
d�B
e�B
e�B
e�B
e�B
f�B
f�B
f�B
g�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
i�B
j�B
j�B
j�B
j�B
j�B
j�B
k�B
k�B
k�B
k�B
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
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
s B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t
B
t�B
t�B
t�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<O��<02<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<A�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =0.23 dbar                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CTM alpha = 0.141 & tau = 6.68 s with error equal to the correction                                                                                                                                                                                             Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  201810310930122018103109301220181031093012  AO  ARCAADJP                                                                    20180120180344    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20180120180344  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20180120180344  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20181031093012  QC  PRES            @�33D�� G�O�                PM  ARSQCTM V1.1                                                                20181031093012  QC  PSAL            @�33D�� G�O�                PM  ARSQCOWGV1.1CTD_2021v2 + Argo_2021v03                                       20230721230915  IP                  G�O�G�O�G�O�                