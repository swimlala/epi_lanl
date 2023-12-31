CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:40:37Z creation;2022-06-04T17:40:37Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޼   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604174037  20220610131507  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               dA   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @ٟ��M��1   @ٟ��?V@/n��O��cM&�x��1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @   @�  @�33A   A   A@  A`  A�  A�  A�  A�  A�  A���AᙚA���A�ffB  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B���B�  B�ffB�  B�  B���B�  B�  B�  B�  B�  B�33B�ffB���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8�C:�C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\33C]��C`  Cb�Cd  Ce�fCg�fCi�fCl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt�fDu  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�C3Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @G�@qG�@��
@���AQ�A<Q�A\Q�A|Q�A�(�A�(�A�(�A�(�A���A�A���A��\B{B{B{B{B'{B/{B7{B?{BG{BO{BW{B_{Bg{Bo{Bw{B{B��=B��=B��=B��=B�W
B��=B��B��=B��=B�W
B��=B��=B��=B��=B��=B��pB��B�W
B�W
Bϊ=Bӊ=B׊=Bۊ=Bߊ=B�=B�=B�=B�=B�=B��=B��=B��=C�C޹C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7޹C9޹C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�RC]^�C_�Ca޹Cc�Ce��Cg��Ci��Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��\C��\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C���C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��\C��\C��C��C��C��C��C��C��C��C��C��C��C��C��C��D qHD �HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HD	qHD	�HD
qHD
�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HD qHD �HD!qHD!�HD"qHD"�HD#qHD#�HD$qHD$�HD%qHD%�HD&qHD&�HD'qHD'�HD(qHD(�HD)qHD)�HD*qHD*�HD+qHD+�HD,qHD,�HD-qHD-�HD.qHD.�HD/qHD/�HD0qHD0�HD1qHD1�HD2qHD2�HD3qHD3�HD4qHD4�HD5qHD5�HD6qHD6�HD7qHD7�HD8qHD8�HD9qHD9�HD:qHD:�HD;qHD;�HD<qHD<�HD=qHD=�HD>qHD>�HD?qHD?�HD@qHD@�HDAqHDA�HDBqHDB�HDCqHDC�HDDqHDD�HDEqHDE�HDFqHDF�HDGqHDG�HDHqHDH�HDIqHDI�HDJqHDJ�HDKqHDK�HDLqHDL�HDMqHDM�HDNqHDN�HDOqHDO�HDPqHDP�HDQqHDQ�HDRqHDR�HDSqHDS�HDTqHDT�HDUqHDU�HDVqHDV�HDWqHDW�HDXqHDX�HDYqHDY�HDZqHDZ�HD[qHD[�HD\qHD\�HD]qHD]�HD^qHD^�HD_qHD_�HD`qHD`�HDaqHDa�HDbqHDb�HDcqHDc�HDdqHDd�HDeqHDe�HDfqHDf�HDgqHDg�HDhqHDh�HDiqHDi�HDjqHDj�HDkqHDk�HDlqHDl�HDmqHDm�HDnqHDn�HDoqHDo�HDpqHDp�HDqqHDq�HDrqHDr�HDsqHDs�HDtw�Dt�HDuqHDu�HDvqHDv�HDwqHDw�HDxqHDx�HDyqHDy�HDzqHDz�HD{qHD{�HD|qHD|�HD}qHD}�HD~qHD~�HDqHD�HD�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D¸�D���D�8�D�x�Dø�D���D�8�D�x�Dĸ�D���D�8�D�x�DŸ�D���D�8�D�x�DƸ�D���D�8�D�x�DǸ�D���D�8�D�x�Dȸ�D���D�8�D�x�Dɸ�D���D�8�D�x�Dʸ�D���D�8�D�x�D˸�D���D�8�D�x�D̸�D���D�8�D�x�D͸�D���D�8�D�x�Dθ�D���D�8�D�x�Dϸ�D���D�8�D�x�Dи�D���D�8�D�x�DѸ�D���D�8�D�x�DҸ�D���D�8�D�x�DӸ�D���D�8�D�x�DԸ�D���D�8�D�x�Dո�D���D�;�D�x�Dָ�D���D�8�D�x�D׸�D���D�8�D�x�Dظ�D���D�8�D�x�Dٸ�D���D�8�D�x�Dڸ�D���D�8�D�x�D۸�D���D�8�D�x�Dܸ�D���D�8�D�x�Dݸ�D���D�8�D�x�D޸�D���D�8�D�x�D߸�D���D�8�D�x�DฤD���D�8�D�x�DḤD���D�8�D�x�D⸤D���D�8�D�x�D㸤D���D�8�D�x�D两D���D�8�D�x�D帤D���D�8�D�x�D渤D���D�8�D�x�D縤D���D�8�D�x�D踤D���D�8�D�x�D鸤D���D�8�D�x�D긤D���D�8�D�x�D븤D���D�8�D�x�D츤D���D�8�D�x�D���D���D�8�D�x�DD���D�8�D�x�D︤D���D�8�D�x�D�D���D�8�D�x�D�D���D�8�D�x�D�D���D�8�D�x�D�D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D��q11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�E9A�B[A�CaA�IA���A�'�A��fA��mA�ȀAո�Aհ�Aը$Aՙ�AՐbA�s�A�R�A��Aѿ�Aљ�AыxA�.A�n/A�\)A�MjA�=<A�C�A�@OA�D�A�A�A�=<A�.�A�A�A��A��2Aк*A���A�.Aņ�A��A�!A��A���A�VA���A���A��A�>�A�lWA�=A�r�A�\)A��7A��hA��hA�l"A�͟A�FA��A��A�$�A�.�A�J�A�h
A���A�A��A��A��cA���A�P�A��KA�1�A���A���A�՛A�"�A���A��kA�N<A���A��YA��A�/�A�	lA���A�t�A��A��A��1A��?A�bA~[�AzJ�Av�AuxAu-wAr�?Aq>BAn�Ae�A]��AX	�AR|AL�AHHAD�TAD�bAE�AD��AB�ABVAAw�A@�6A;�dA9��A8xlA9	lA9N<A8��A8hsA7g�A6�A6M�A6{A5�A5��A5iDA5�_A5$A3ѷA1�eA0��A0tTA/ĜA.��A.OA-)�A,�oA+�'A*�^A)a�A(,�A%5?A 2�Ar�A��A�A�A7A�'A`�A:�A��A2�AqA��A�A�A��AJ�A�DAA�A_A��AOvA�AGA
�MAE�A��A�xA�6A�@A��AXA
�rA
5?A	��A	c�A��AhsA�A�?As�AXyA!�A��A��A�xAe,A&�A�8A�1A��AMjA�A�
A�HA�A��A��A�1A��A�A�
AJA ��@�&@�d�@�8�@��6@���@��@�C-@���@��"@�o@�4�@���@���@�4�@�Ɇ@��}@���@���@���@�c @�&@�O@�`B@�,�@�c@���@�
=@��b@��@��@�-�@�n@�@O@�ȴ@�@��@�YK@���@��p@�E�@���@��@�V�@�$t@�H@�A�@�e@炪@��c@�7�@��@��@��@�~@�a�@��#@俱@�,�@���@�U2@�g8@��@�/�@��_@�@��?@�Xy@�~@߷�@���@߆�@ߵt@��@ރ�@�"h@ݣn@݋�@��|@�n/@�kQ@�]d@َ"@�+@�=�@دO@�\�@�@�@ӟV@�|�@�A @�*0@қ�@�!�@��@�j@�@@�Ĝ@�L0@��Q@Ϟ�@�9�@�S@��"@�V@��@͓@��H@�7@ˉ7@�֡@�@�@�J@���@�y�@�a�@�'�@��]@�H@�|@�)_@��@ǜ�@ǣn@�u�@ƦL@ƞ@�i�@���@�N<@ŀ4@Ğ@�u�@�@��`@®}@r@�ff@��@�7L@�֡@�oi@��K@��@�RT@��@�m�@���@�*0@���@�ȴ@�Q�@�ݘ@��k@���@�x�@��@�d�@��@�q@�Q@��@��V@�0�@��+@�d�@�I�@��@��@��^@���@���@�y>@���@�`B@�/�@��@�Xy@�
�@��0@���@�<6@�@��@�~(@�;�@�  @�ԕ@���@�.I@��@���@�!�@��;@�~�@��@�C-@��@��@���@��z@��b@�Z�@��@�҉@���@�h�@�4n@��@��@�	l@��u@��@�\�@��@���@��u@�!�@�J@��@���@���@��@���@��@���@�q�@�PH@��@��@�j�@�0�@���@���@�1'@��@���@�-w@�(@���@��K@���@��@���@�a|@��
@���@���@�~�@�e�@�)_@���@�h
@�{@���@��@��0@�j�@�֡@���@�i�@�D�@�$@��N@�t�@�+�@��@�3�@���@���@�c@�J�@��v@���@�g8@�;�@�
�@��V@�N<@�V@��/@�xl@�	@��@���@���@��-@��f@�c�@��c@��@�/�@�ϫ@�t�@�A�@���@���@�!�@���@���@�4@��@��X@��r@���@�y>@�kQ@�:�@���@��@���@�a�@�@O@�'�@��@��z@�Ta@�1@��@�;d@��$@�r�@�~@�خ@��t@�x@�RT@�(@��X@���@�Ft@��@���@��H@�/�@��@�~�@�z@�h
@�1�@�@��&@��^@�s@���@��@���@��w@���@���@�f�@�G�@�+@��@��@�ی@���@�w�@���@�p�@�@��I@�y>@�K^@�@��@!-@~&�@}�N@}B�@|��@| �@{S�@z�c@z�}@z�@y��@ys�@y�@x��@xM@w��@v�"@v� @v=q@u�D@uw2@u�@t�?@t9X@tb@s�
@s�*@sO@s�@rp;@r!�@q�@q�"@q�@pM@o�k@o�@n��@n)�@m�@mG�@l�@lU2@k�+@k��@kqv@kMj@j��@j;�@jJ@i�@ic�@i�@h7�@g�*@g33@fff@f-@f�@e�@ef�@d�5@d��@d��@dD�@d/�@d@c�w@c�{@c@O@b�,@b��@b��@b($@a�@aa�@a�@`��@`'R@_�@_�*@^��@^xl@^�@][W@\~@[��@[S@Zc @Z($@Z4@Y�#@Y��@Y�"@Y|@Yk�@YT�@Yq@X�@X�e@XZ@X	�@W�a@W�:@W��@Wl�@V�8@V��@V
�@U��@UG�@T�@S�g@Ss@S(@R($@Q��@P�	@P�U@P�Y@O��@O]�@O)_@N�y@NkQ@M��@Me,@L�`@L��@L��@LXy@K{J@K�@Jp;@J;�@I��@I�N@I!�@H�@H$@G�6@G��@GiD@G�@F��@F}V@F)�@E��@Em]@EL�@D��@D�z@C��@C]�@CC@Bz@B+k@B�@A��@A�@A`B@A�@@��@@�@@2�@?��@?�&@?�K@?�:@?a@>�@>E�@=��@=��@=j@=(�@<�o@<4n@;��@;�	@;9�@;�@;�@:��@:v�@:0U@9��@9�3@9��@9��@8��@8|�@8S�@8*�@7��@7��@7�{@7'�@6��@6~�@66�@5�Z@5��@5��@5a�@5*0@4�	@4|�@4 �@3�A@3ݘ@3�K@3�:@3b�@3�@2͟@2�m@2�@2�b@2}V@2_�@2H�@1��@1��@1?}@0��@0�_@0�@0|�@0  @/��@/O@/�@.�@.��@.Q@-�@-�@-|@-S&@-!�@,��@,��@,u�@,K^@, �@+��@+��@+\)@+�@*�X@*��@*5?@)��@)@)��@)m]@)F@)�@(�|@(��@(u�@(�@'��@'�:@'�	@'qv@';d@&ں@&��@&5?@%�@%��@%c@%c�@%5�@%�@$�@$��@$��@$`�@$ �@#� @#�@#_p@#9�@#o@"��@"��@"YK@"@�@";�@"($@" �@!�@!f�@ �K@ ��@ M@ 1@�Q@��@j�@�,@{�@_�@�@�o@ϫ@��@hs@��@Ɇ@��@��@M@	�@�k@X�@H�@o@�]@�R@u%@;�@�@�@��@�-@hs@/@�@�e@��@r�@6@@ݘ@�k@1�@�@�2@R�@�@@	@��@��@e,@?}@�@��@��@�I@��@]d@>B@M@خ@�*@o�@]�@/�@�s@YK@)�@�@[W@V@��@��@�@r�@"h@�@��@��@s@_p@Mj@�@�@�X@v�@W�@�@��@��@f�@-w@�@��@��@�@�@r�@K^@2�@"h@��@ݘ@��@X�@+@"�@�@
�]@
��@
J�@
	@	�.@	��@	u�@	\�@	4@	�@��@��@V�@2�@@�m@�F@��@��@�P@v`@S�@�@�,@�}@�}11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�E9A�B[A�CaA�IA���A�'�A��fA��mA�ȀAո�Aհ�Aը$Aՙ�AՐbA�s�A�R�A��Aѿ�Aљ�AыxA�.A�n/A�\)A�MjA�=<A�C�A�@OA�D�A�A�A�=<A�.�A�A�A��A��2Aк*A���A�.Aņ�A��A�!A��A���A�VA���A���A��A�>�A�lWA�=A�r�A�\)A��7A��hA��hA�l"A�͟A�FA��A��A�$�A�.�A�J�A�h
A���A�A��A��A��cA���A�P�A��KA�1�A���A���A�՛A�"�A���A��kA�N<A���A��YA��A�/�A�	lA���A�t�A��A��A��1A��?A�bA~[�AzJ�Av�AuxAu-wAr�?Aq>BAn�Ae�A]��AX	�AR|AL�AHHAD�TAD�bAE�AD��AB�ABVAAw�A@�6A;�dA9��A8xlA9	lA9N<A8��A8hsA7g�A6�A6M�A6{A5�A5��A5iDA5�_A5$A3ѷA1�eA0��A0tTA/ĜA.��A.OA-)�A,�oA+�'A*�^A)a�A(,�A%5?A 2�Ar�A��A�A�A7A�'A`�A:�A��A2�AqA��A�A�A��AJ�A�DAA�A_A��AOvA�AGA
�MAE�A��A�xA�6A�@A��AXA
�rA
5?A	��A	c�A��AhsA�A�?As�AXyA!�A��A��A�xAe,A&�A�8A�1A��AMjA�A�
A�HA�A��A��A�1A��A�A�
AJA ��@�&@�d�@�8�@��6@���@��@�C-@���@��"@�o@�4�@���@���@�4�@�Ɇ@��}@���@���@���@�c @�&@�O@�`B@�,�@�c@���@�
=@��b@��@��@�-�@�n@�@O@�ȴ@�@��@�YK@���@��p@�E�@���@��@�V�@�$t@�H@�A�@�e@炪@��c@�7�@��@��@��@�~@�a�@��#@俱@�,�@���@�U2@�g8@��@�/�@��_@�@��?@�Xy@�~@߷�@���@߆�@ߵt@��@ރ�@�"h@ݣn@݋�@��|@�n/@�kQ@�]d@َ"@�+@�=�@دO@�\�@�@�@ӟV@�|�@�A @�*0@қ�@�!�@��@�j@�@@�Ĝ@�L0@��Q@Ϟ�@�9�@�S@��"@�V@��@͓@��H@�7@ˉ7@�֡@�@�@�J@���@�y�@�a�@�'�@��]@�H@�|@�)_@��@ǜ�@ǣn@�u�@ƦL@ƞ@�i�@���@�N<@ŀ4@Ğ@�u�@�@��`@®}@r@�ff@��@�7L@�֡@�oi@��K@��@�RT@��@�m�@���@�*0@���@�ȴ@�Q�@�ݘ@��k@���@�x�@��@�d�@��@�q@�Q@��@��V@�0�@��+@�d�@�I�@��@��@��^@���@���@�y>@���@�`B@�/�@��@�Xy@�
�@��0@���@�<6@�@��@�~(@�;�@�  @�ԕ@���@�.I@��@���@�!�@��;@�~�@��@�C-@��@��@���@��z@��b@�Z�@��@�҉@���@�h�@�4n@��@��@�	l@��u@��@�\�@��@���@��u@�!�@�J@��@���@���@��@���@��@���@�q�@�PH@��@��@�j�@�0�@���@���@�1'@��@���@�-w@�(@���@��K@���@��@���@�a|@��
@���@���@�~�@�e�@�)_@���@�h
@�{@���@��@��0@�j�@�֡@���@�i�@�D�@�$@��N@�t�@�+�@��@�3�@���@���@�c@�J�@��v@���@�g8@�;�@�
�@��V@�N<@�V@��/@�xl@�	@��@���@���@��-@��f@�c�@��c@��@�/�@�ϫ@�t�@�A�@���@���@�!�@���@���@�4@��@��X@��r@���@�y>@�kQ@�:�@���@��@���@�a�@�@O@�'�@��@��z@�Ta@�1@��@�;d@��$@�r�@�~@�خ@��t@�x@�RT@�(@��X@���@�Ft@��@���@��H@�/�@��@�~�@�z@�h
@�1�@�@��&@��^@�s@���@��@���@��w@���@���@�f�@�G�@�+@��@��@�ی@���@�w�@���@�p�@�@��I@�y>@�K^@�@��@!-@~&�@}�N@}B�@|��@| �@{S�@z�c@z�}@z�@y��@ys�@y�@x��@xM@w��@v�"@v� @v=q@u�D@uw2@u�@t�?@t9X@tb@s�
@s�*@sO@s�@rp;@r!�@q�@q�"@q�@pM@o�k@o�@n��@n)�@m�@mG�@l�@lU2@k�+@k��@kqv@kMj@j��@j;�@jJ@i�@ic�@i�@h7�@g�*@g33@fff@f-@f�@e�@ef�@d�5@d��@d��@dD�@d/�@d@c�w@c�{@c@O@b�,@b��@b��@b($@a�@aa�@a�@`��@`'R@_�@_�*@^��@^xl@^�@][W@\~@[��@[S@Zc @Z($@Z4@Y�#@Y��@Y�"@Y|@Yk�@YT�@Yq@X�@X�e@XZ@X	�@W�a@W�:@W��@Wl�@V�8@V��@V
�@U��@UG�@T�@S�g@Ss@S(@R($@Q��@P�	@P�U@P�Y@O��@O]�@O)_@N�y@NkQ@M��@Me,@L�`@L��@L��@LXy@K{J@K�@Jp;@J;�@I��@I�N@I!�@H�@H$@G�6@G��@GiD@G�@F��@F}V@F)�@E��@Em]@EL�@D��@D�z@C��@C]�@CC@Bz@B+k@B�@A��@A�@A`B@A�@@��@@�@@2�@?��@?�&@?�K@?�:@?a@>�@>E�@=��@=��@=j@=(�@<�o@<4n@;��@;�	@;9�@;�@;�@:��@:v�@:0U@9��@9�3@9��@9��@8��@8|�@8S�@8*�@7��@7��@7�{@7'�@6��@6~�@66�@5�Z@5��@5��@5a�@5*0@4�	@4|�@4 �@3�A@3ݘ@3�K@3�:@3b�@3�@2͟@2�m@2�@2�b@2}V@2_�@2H�@1��@1��@1?}@0��@0�_@0�@0|�@0  @/��@/O@/�@.�@.��@.Q@-�@-�@-|@-S&@-!�@,��@,��@,u�@,K^@, �@+��@+��@+\)@+�@*�X@*��@*5?@)��@)@)��@)m]@)F@)�@(�|@(��@(u�@(�@'��@'�:@'�	@'qv@';d@&ں@&��@&5?@%�@%��@%c@%c�@%5�@%�@$�@$��@$��@$`�@$ �@#� @#�@#_p@#9�@#o@"��@"��@"YK@"@�@";�@"($@" �@!�@!f�@ �K@ ��@ M@ 1@�Q@��@j�@�,@{�@_�@�@�o@ϫ@��@hs@��@Ɇ@��@��@M@	�@�k@X�@H�@o@�]@�R@u%@;�@�@�@��@�-@hs@/@�@�e@��@r�@6@@ݘ@�k@1�@�@�2@R�@�@@	@��@��@e,@?}@�@��@��@�I@��@]d@>B@M@خ@�*@o�@]�@/�@�s@YK@)�@�@[W@V@��@��@�@r�@"h@�@��@��@s@_p@Mj@�@�@�X@v�@W�@�@��@��@f�@-w@�@��@��@�@�@r�@K^@2�@"h@��@ݘ@��@X�@+@"�@�@
�]@
��@
J�@
	@	�.@	��@	u�@	\�@	4@	�@��@��@V�@2�@@�m@�F@��@��@�P@v`@S�@�@�,@�}@�}11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�LB��B��B��B�FB�]B�B?B�B#�B*BE�Bj�B|�B�6B
��BhsBr�BsMBs�Bs�Bs�BtBs�BtBzDB|PB�B��B�{B�-B�AB��B��B� B�Bt�B,�B'mB&fB-�BC�BtB�B��B��BʌB�2BݲB�B��B�B�B�PB�B=BNB BBKB�B �B��B�B��B�aB�B�B�SBȀB�dB�wB�UB��Bu%Bm]Bl�BezBS�B=�B#TB,B?B
� B
��B
ƨB
��B
��B
o5B
H�B
-)B
�B
�B	�B	��B	�vB	�JB	� B	��B	��B	p�B	@OB	(>B	�B��B�|B�XB�8B	�B	_B	�B	�B	2B	!�B	4�B	J�B	^�B	y>B	�3B	�%B	��B	�gB	�|B	��B	��B	�B	ÖB	ѷB	�xB	�]B	��B	��B	��B	ٚB	�	B	��B	ٚB	��B	�B	�`B	�9B	��B	�IB	ބB	�;B	{B	o�B	kB	u?B	p�B	h
B	y�B	�NB	��B	h�B	a�B	f�B	u?B	w�B	y	B	y�B	}B	|PB	B	��B	�iB	.B	�OB	��B	�UB	�=B	��B	�xB	�5B	��B	��B	�hB	��B	�>B	��B	�yB	��B	�&B	�TB	��B	�,B	��B	��B	��B	�XB	��B	�kB	��B	�B	�KB	�
B	�B	�ZB	��B	�LB	�*B	�6B	��B	�kB	��B	�qB	�CB	��B	�8B	�NB	��B	�_B	�B	�)B	�wB	�B	�XB	�$B	��B	�>B	�hB	ּB	�_B	ؓB	�EB	خB	�QB	چB	��B	�uB	��B	�}B	�bB	�FB	��B	�OB	��B	�B	�B	�B	ܬB	ۦB	�QB	��B	�+B	خB	�_B	�YB	�$B	��B	�SB	�B	��B	��B	ңB	�@B	҉B	��B	ѝB	� B	� B	� B	ӏB	�B	��B	��B	��B	�B	�B	�ZB	�tB	�@B	��B	�2B	�XB	�$B	�>B	�B	�WB	��B	�B	�UB	�B	��B	�B	�cB	�B	��B	�2B	��B	�B	��B	��B	�B	�B	ߊB	ڠB	�QB	ڠB	�QB	ٴB	�eB	�kB	��B	��B	�dB	��B	�jB	��B	�~B	��B	�CB	ܬB	ݘB	ܬB	�)B	�]B	�#B	��B	ڠB	ںB	��B	�	B	�xB	�OB	�jB	޸B	�\B	�pB	ߤB	��B	�B	��B	�B	�>B	�0B	�IB	�WB	�B	�CB	��B	��B	�)B	��B	��B	�B	�B	��B	�B	�B	�'B	��B	�|B	�3B	�B	�B	�aB	�B	�-B	�|B	�B	��B	��B	��B	��B	��B	��B	�RB	�XB	�B	�0B	�B	��B	�qB	��B	��B	�B	�BB	�wB	�BB
 B
 �B
[B
�B
{B
�B
�B
�B
�B
B
�B
�B
�B
�B
B
%B
%B
?B
?B
�B
�B
�B
�B
�B
{B
�B
 �B
 iB
 �B
�B
-B
�B
oB	�.B	�B	�}B	�cB	��B	�HB	��B	��B	��B	��B	�(B	�]B	��B	��B	��B	��B	��B	�B	��B	�.B	�cB	��B
 4B
 �B
�B
�B
�B
�B
'B
AB
�B
�B
�B
�B
�B
�B
B
9B
mB
mB
�B
�B
�B
�B
�B
�B
EB
�B
1B
B
�B
B
�B
B
B
�B
�B
	7B
	RB
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
)B
�B
DB
JB
�B
�B
�B
"B
B
}B
�B
�B
�B
B
�B
�B
yB
�B
#B
�B
	B
�B
CB
dB
�B
�B
B
�B
�B
�B
 B
 �B
 �B
 �B
!-B
!bB
!�B
!�B
!�B
!�B
"B
"�B
"�B
#:B
#�B
$B
#�B
$@B
$tB
$�B
$�B
$�B
$�B
$�B
%�B
%`B
%zB
%zB
%�B
%�B
%�B
&�B
'�B
'�B
)_B
*KB
+B
+B
+QB
+�B
+B
,�B
.B
-�B
./B
.IB
.}B
.�B
.�B
/ B
/ B
/OB
/5B
/iB
/�B
0�B
0�B
1�B
2GB
2aB
2�B
2�B
2�B
3B
3MB
3MB
3hB
3�B
3�B
4B
49B
4B
4nB
4nB
4TB
4�B
5%B
6+B
6�B
6�B
6�B
7�B
8B
8RB
8�B
8�B
9XB
9rB
9�B
9�B
9�B
:B
:�B
:�B
:�B
:�B
;B
;B
;�B
<B
<B
<6B
<�B
<�B
<�B
=B
=VB
=qB
=qB
=qB
=�B
>(B
>BB
>BB
>�B
>�B
?HB
?cB
?}B
@OB
@OB
@OB
@�B
@�B
A;B
A;B
A�B
A�B
A�B
A�B
A�B
A�B
B'B
BuB
B[B
BAB
B�B
B�B
CGB
C�B
D3B
D3B
D3B
DMB
EB
D�B
EB
E�B
F�B
F�B
GzB
G�B
G�B
HB
HKB
HKB
HfB
H�B
HfB
H�B
H�B
H�B
H�B
I7B
IlB
I�B
I�B
I�B
I�B
J	B
JXB
J�B
J�B
KB
K^B
LB
K�B
LdB
MB
MB
M�B
M�B
M�B
N�B
N�B
N�B
OB
OvB
O�B
P.B
P}B
P}B
PbB
P�B
QNB
QhB
Q�B
Q�B
Q�B
Q�B
RTB
R�B
SB
S[B
S�B
S�B
S�B
S�B
TB
T,B
TaB
TFB
T,B
TaB
TaB
T�B
T�B
T�B
T�B
T�B
U2B
UMB
T�B
T�B
T�B
UB
U�B
UgB
U�B
U�B
U�B
U�B
V9B
VB
U�B
VB
U�B
U�B
V9B
V�B
V�B
V�B
W$B
WsB
W�B
W�B
W�B
X_B
X_B
XyB
X�B
X_B
X�B
Y�B
Y�B
Y�B
Y�B
ZQB
Z�B
Z�B
[#B
[qB
[�B
[�B
\)B
\xB
\�B
]B
]B
]B
]�B
]�B
]�B
]�B
]�B
^B
^B
^5B
^�B
^�B
^�B
^�B
^�B
^�B
^�B
_!B
^�B
_pB
_�B
_�B
_�B
_�B
`BB
`�B
`�B
aB
abB
abB
a�B
b4B
bhB
b�B
b�B
cB
c B
c�B
c�B
c�B
c�B
d�B
d�B
d�B
e,B
e`B
e�B
fB
f2B
ffB
f�B
f�B
f�B
f�B
f�B
gmB
g�B
h
B
hsB
hsB
hsB
h�B
h�B
iDB
i�B
i�B
j0B
j0B
jeB
jeB
j�B
j�B
j�B
j�B
j�B
kkB
k�B
k�B
k�B
l"B
l"B
l=B
lqB
l�B
l�B
mB
l�B
l�B
l�B
m)B
mwB
m�B
m�B
nB
n}B
nIB
ncB
n�B
oOB
oiB
oiB
o�B
o�B
o�B
o�B
o�B
o�B
p!B
pB
p!B
p;B
pUB
p�B
p�B
p�B
q'B
qAB
q�B
q�B
q�B
q�B
q�B
rB
q�B
r|B
r|B
r�B
r�B
r�B
sB
sMB
shB
s�B
s�B
tTB
tTB
tTB
u?B
u%B
u?B
uZB
u?B
u�B
u�B
v+B
vzB
v�B
v�B
v�B
v�B
v�B
wB
wLB
wfB
w�B
w�B
w�B
w�B
xlB
x�B
x�B
y	B
y�B
y�B
y�B
zB
zB
zxB
z�B
z�B
{B
z�B
{dB
{JB
{JB
{�B
{�B
|B
|6B
|6B
|�B
|�B
|�B
}B
}qB
}qB
}qB
}�B
}�B
}�B
}�B
~B
~BB
~BB
~wB
~wB
~�B
HB
�B
�B
�B
�B
�OB
��B
��B
��B
��B
��B
��B
�AB
�uB
�uB
�uB
�B
�GB
�aB
�{B
��B
��B
��B
��B
��B
�3B
��B
��B
��B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�LB��B��B��B�FB�]B�B?B�B#�B*BE�Bj�B|�B�6B
��BhsBr�BsMBs�Bs�Bs�BtBs�BtBzDB|PB�B��B�{B�-B�AB��B��B� B�Bt�B,�B'mB&fB-�BC�BtB�B��B��BʌB�2BݲB�B��B�B�B�PB�B=BNB BBKB�B �B��B�B��B�aB�B�B�SBȀB�dB�wB�UB��Bu%Bm]Bl�BezBS�B=�B#TB,B?B
� B
��B
ƨB
��B
��B
o5B
H�B
-)B
�B
�B	�B	��B	�vB	�JB	� B	��B	��B	p�B	@OB	(>B	�B��B�|B�XB�8B	�B	_B	�B	�B	2B	!�B	4�B	J�B	^�B	y>B	�3B	�%B	��B	�gB	�|B	��B	��B	�B	ÖB	ѷB	�xB	�]B	��B	��B	��B	ٚB	�	B	��B	ٚB	��B	�B	�`B	�9B	��B	�IB	ބB	�;B	{B	o�B	kB	u?B	p�B	h
B	y�B	�NB	��B	h�B	a�B	f�B	u?B	w�B	y	B	y�B	}B	|PB	B	��B	�iB	.B	�OB	��B	�UB	�=B	��B	�xB	�5B	��B	��B	�hB	��B	�>B	��B	�yB	��B	�&B	�TB	��B	�,B	��B	��B	��B	�XB	��B	�kB	��B	�B	�KB	�
B	�B	�ZB	��B	�LB	�*B	�6B	��B	�kB	��B	�qB	�CB	��B	�8B	�NB	��B	�_B	�B	�)B	�wB	�B	�XB	�$B	��B	�>B	�hB	ּB	�_B	ؓB	�EB	خB	�QB	چB	��B	�uB	��B	�}B	�bB	�FB	��B	�OB	��B	�B	�B	�B	ܬB	ۦB	�QB	��B	�+B	خB	�_B	�YB	�$B	��B	�SB	�B	��B	��B	ңB	�@B	҉B	��B	ѝB	� B	� B	� B	ӏB	�B	��B	��B	��B	�B	�B	�ZB	�tB	�@B	��B	�2B	�XB	�$B	�>B	�B	�WB	��B	�B	�UB	�B	��B	�B	�cB	�B	��B	�2B	��B	�B	��B	��B	�B	�B	ߊB	ڠB	�QB	ڠB	�QB	ٴB	�eB	�kB	��B	��B	�dB	��B	�jB	��B	�~B	��B	�CB	ܬB	ݘB	ܬB	�)B	�]B	�#B	��B	ڠB	ںB	��B	�	B	�xB	�OB	�jB	޸B	�\B	�pB	ߤB	��B	�B	��B	�B	�>B	�0B	�IB	�WB	�B	�CB	��B	��B	�)B	��B	��B	�B	�B	��B	�B	�B	�'B	��B	�|B	�3B	�B	�B	�aB	�B	�-B	�|B	�B	��B	��B	��B	��B	��B	��B	�RB	�XB	�B	�0B	�B	��B	�qB	��B	��B	�B	�BB	�wB	�BB
 B
 �B
[B
�B
{B
�B
�B
�B
�B
B
�B
�B
�B
�B
B
%B
%B
?B
?B
�B
�B
�B
�B
�B
{B
�B
 �B
 iB
 �B
�B
-B
�B
oB	�.B	�B	�}B	�cB	��B	�HB	��B	��B	��B	��B	�(B	�]B	��B	��B	��B	��B	��B	�B	��B	�.B	�cB	��B
 4B
 �B
�B
�B
�B
�B
'B
AB
�B
�B
�B
�B
�B
�B
B
9B
mB
mB
�B
�B
�B
�B
�B
�B
EB
�B
1B
B
�B
B
�B
B
B
�B
�B
	7B
	RB
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
)B
�B
DB
JB
�B
�B
�B
"B
B
}B
�B
�B
�B
B
�B
�B
yB
�B
#B
�B
	B
�B
CB
dB
�B
�B
B
�B
�B
�B
 B
 �B
 �B
 �B
!-B
!bB
!�B
!�B
!�B
!�B
"B
"�B
"�B
#:B
#�B
$B
#�B
$@B
$tB
$�B
$�B
$�B
$�B
$�B
%�B
%`B
%zB
%zB
%�B
%�B
%�B
&�B
'�B
'�B
)_B
*KB
+B
+B
+QB
+�B
+B
,�B
.B
-�B
./B
.IB
.}B
.�B
.�B
/ B
/ B
/OB
/5B
/iB
/�B
0�B
0�B
1�B
2GB
2aB
2�B
2�B
2�B
3B
3MB
3MB
3hB
3�B
3�B
4B
49B
4B
4nB
4nB
4TB
4�B
5%B
6+B
6�B
6�B
6�B
7�B
8B
8RB
8�B
8�B
9XB
9rB
9�B
9�B
9�B
:B
:�B
:�B
:�B
:�B
;B
;B
;�B
<B
<B
<6B
<�B
<�B
<�B
=B
=VB
=qB
=qB
=qB
=�B
>(B
>BB
>BB
>�B
>�B
?HB
?cB
?}B
@OB
@OB
@OB
@�B
@�B
A;B
A;B
A�B
A�B
A�B
A�B
A�B
A�B
B'B
BuB
B[B
BAB
B�B
B�B
CGB
C�B
D3B
D3B
D3B
DMB
EB
D�B
EB
E�B
F�B
F�B
GzB
G�B
G�B
HB
HKB
HKB
HfB
H�B
HfB
H�B
H�B
H�B
H�B
I7B
IlB
I�B
I�B
I�B
I�B
J	B
JXB
J�B
J�B
KB
K^B
LB
K�B
LdB
MB
MB
M�B
M�B
M�B
N�B
N�B
N�B
OB
OvB
O�B
P.B
P}B
P}B
PbB
P�B
QNB
QhB
Q�B
Q�B
Q�B
Q�B
RTB
R�B
SB
S[B
S�B
S�B
S�B
S�B
TB
T,B
TaB
TFB
T,B
TaB
TaB
T�B
T�B
T�B
T�B
T�B
U2B
UMB
T�B
T�B
T�B
UB
U�B
UgB
U�B
U�B
U�B
U�B
V9B
VB
U�B
VB
U�B
U�B
V9B
V�B
V�B
V�B
W$B
WsB
W�B
W�B
W�B
X_B
X_B
XyB
X�B
X_B
X�B
Y�B
Y�B
Y�B
Y�B
ZQB
Z�B
Z�B
[#B
[qB
[�B
[�B
\)B
\xB
\�B
]B
]B
]B
]�B
]�B
]�B
]�B
]�B
^B
^B
^5B
^�B
^�B
^�B
^�B
^�B
^�B
^�B
_!B
^�B
_pB
_�B
_�B
_�B
_�B
`BB
`�B
`�B
aB
abB
abB
a�B
b4B
bhB
b�B
b�B
cB
c B
c�B
c�B
c�B
c�B
d�B
d�B
d�B
e,B
e`B
e�B
fB
f2B
ffB
f�B
f�B
f�B
f�B
f�B
gmB
g�B
h
B
hsB
hsB
hsB
h�B
h�B
iDB
i�B
i�B
j0B
j0B
jeB
jeB
j�B
j�B
j�B
j�B
j�B
kkB
k�B
k�B
k�B
l"B
l"B
l=B
lqB
l�B
l�B
mB
l�B
l�B
l�B
m)B
mwB
m�B
m�B
nB
n}B
nIB
ncB
n�B
oOB
oiB
oiB
o�B
o�B
o�B
o�B
o�B
o�B
p!B
pB
p!B
p;B
pUB
p�B
p�B
p�B
q'B
qAB
q�B
q�B
q�B
q�B
q�B
rB
q�B
r|B
r|B
r�B
r�B
r�B
sB
sMB
shB
s�B
s�B
tTB
tTB
tTB
u?B
u%B
u?B
uZB
u?B
u�B
u�B
v+B
vzB
v�B
v�B
v�B
v�B
v�B
wB
wLB
wfB
w�B
w�B
w�B
w�B
xlB
x�B
x�B
y	B
y�B
y�B
y�B
zB
zB
zxB
z�B
z�B
{B
z�B
{dB
{JB
{JB
{�B
{�B
|B
|6B
|6B
|�B
|�B
|�B
}B
}qB
}qB
}qB
}�B
}�B
}�B
}�B
~B
~BB
~BB
~wB
~wB
~�B
HB
�B
�B
�B
�B
�OB
��B
��B
��B
��B
��B
��B
�AB
�uB
�uB
�uB
�B
�GB
�aB
�{B
��B
��B
��B
��B
��B
�3B
��B
��B
��B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104925  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604174037  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604174037  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604174037                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605024045  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605024045  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131507                      G�O�G�O�G�O�                