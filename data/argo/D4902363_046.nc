CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-10-19T03:36:18Z creation;2016-10-19T03:36:21Z conversion to V3.1;2019-12-19T08:28:09Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I    PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `l   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �<   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ܘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �    HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �$   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �(Argo profile    3.1 1.2 19500101000000  20161019033618  20200115111518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               .A   JA  I2_0576_046                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�ѵǮ�1   @�Ѷs�� @;2�W���d�d��7�1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  A   A   A@  A`  A�  A�33A�  A�  A�  A�  A�  A�  B   B  BffB  B   B(  B0  B8  B@  BH  BP  BX  B`ffBh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT�fDU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Dsy�Ds��Dt� Du  Du� Dv  Dvy�Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{fD{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�<�Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�&f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @*�H@qG�@���@���AQ�A<Q�A\Q�A|Q�A�\)A�(�A�(�A�(�A�(�A�(�A�(�A�(�B{Bz�B{B{B'{B/{B7{B?{BG{BO{BW{B_z�Bg{Bo{Bw{B{B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=BÊ=BǊ=Bˊ=Bϊ=Bӊ=B׊=Bۊ=Bߊ=B�=B�=B�=B�=B�=B��=B��=B��=C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D qHD �HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HD	qHD	�HD
qHD
�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HD qHD �HD!qHD!�HD"qHD"�HD#qHD#�HD$qHD$�HD%qHD%�HD&qHD&�HD'qHD'�HD(qHD(�HD)qHD)�HD*qHD*�HD+qHD+�HD,qHD,�HD-qHD-�HD.qHD.�HD/qHD/�HD0qHD0�HD1qHD1�HD2qHD2�HD3qHD3�HD4qHD4�HD5qHD5�HD6qHD6�HD7qHD7�HD8qHD8�HD9qHD9�HD:qHD:�HD;qHD;�HD<qHD<�HD=qHD=�HD>qHD>�HD?qHD?�HD@qHD@�HDAqHDA�HDBqHDB�HDCqHDC�HDDqHDD�HDEqHDE�HDFqHDF�HDGqHDG�HDHqHDH�HDIqHDI�HDJqHDJ�HDKqHDK�HDLqHDL�HDMqHDM�HDNqHDN�HDOqHDO�HDPqHDP�HDQqHDQ�HDRqHDR�HDSqHDS�HDTw�DT�HDUqHDU�HDVqHDV�HDWqHDW�HDXqHDX�HDYqHDY�HDZqHDZ�HD[qHD[�HD\qHD\�HD]qHD]�HD^qHD^�HD_qHD_�HD`qHD`�HDaqHDa�HDbqHDb�HDcqHDc�HDdqHDd�HDeqHDe�HDfqHDf�HDgqHDg�HDhqHDh�HDiqHDi�HDjqHDj�HDkqHDk�HDlqHDl�HDmqHDm�HDnqHDn�HDoqHDo�HDpqHDp�HDqqHDq�HDrqHDr�HDsj�Ds��DtqHDt�HDuqHDu�HDvj�Dv�HDwqHDw�HDxqHDx�HDyqHDy�HDzqHDz��D{qHD{�HD|qHD|�HD}qHD}�HD~qHD~�HDqHD�HD�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�{�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D¸�D���D�8�D�x�Dø�D���D�8�D�x�Dĸ�D���D�8�D�x�DŸ�D���D�8�D�x�DƸ�D���D�8�D�x�DǸ�D���D�8�D�x�Dȸ�D���D�8�D�x�Dɸ�D���D�5qD�x�Dʸ�D���D�8�D�x�D˸�D���D�8�D�x�D̸�D���D�8�D�x�D͸�D���D�8�D�x�Dθ�D���D�8�D�x�Dϸ�D���D�8�D�x�Dи�D���D�8�D�x�DѸ�D���D�8�D�x�DҸ�D���D�8�D�x�DӸ�D���D�8�D�x�DԸ�D���D�8�D�x�Dո�D���D�8�D�x�Dָ�D���D�8�D�x�D׸�D���D�8�D�x�Dظ�D���D�8�D�x�Dٸ�D���D�8�D�x�Dڸ�D���D�8�D�x�D۸�D���D�8�D�x�Dܸ�D���D�8�D�x�Dݸ�D���D�8�D�x�D޸�D���D�8�D�x�D߸�D���D�8�D�x�DฤD���D�8�D�x�DḤD���D�8�D�x�D⸤D���D�8�D�x�D㸤D���D�8�D�x�D两D���D�8�D�x�D帤D���D�8�D�x�D渤D���D�8�D�{�D縤D���D�8�D�x�D踤D���D�8�D�x�D鸤D���D�8�D�x�D긤D���D�8�D�x�D븤D���D�8�D�x�D츤D���D�8�D�x�D���D���D�8�D�x�DD���D�8�D�x�D︤D���D�8�D�x�D�D���D�8�D�x�D�D���D�8�D�x�D�D���D�8�D�x�D�D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�XA�S�A�S�A�XA�XA�VA�VA�VA�VA�S�A�S�A�Q�A�S�A�S�A�O�A�K�A�K�A�K�A�33A��A��mAӋDA��A��/A�&�A���A�~�A��A���A���A�{A�bNA��yA��!A�z�A��-A��/A��A�ffA���A��A�\)A���A�bNA��mA��^A�C�A��FA�"�A��9A�7LA�p�A�A�A�I�A��A��A�\)A�{A�ĜA���A�l�A��jA��
A�~�A��PA���A�hsA�S�A�/A���A�ffA~z�A}+A{��A{G�AzVAx1Av�Av�9Au��AtffAr�9Aq"�ApM�AoƨAo7LAn~�AmAl�yAk�Ah�\Ag�Ae��Ad�Ad9XAc��Ac��Abr�A_��A_A^jA^{A\�HA[�A[�#A[dZAY�-AX��AW��AWK�AV�9AVr�AVVAV �AU�mAU��AU�AT�9ASoARz�AP��AOl�AO"�AN��ANv�AL�\AJ$�AH�/AHI�AH1'AH�AHbAH  AG�TAG��AG+AF�/AF��AF�RAF9XAD�yAC��AC
=AAƨA@��A@I�A?l�A>5?A=\)A<��A<^5A;�A:�`A:�uA9��A8�A7��A61A5�7A5
=A3K�A2ffA0�DA/O�A.�yA.r�A.JA-��A-��A-x�A-l�A-hsA-`BA-\)A-;dA,��A,r�A+��A)�TA(ffA'|�A'%A&z�A%��A$��A#�A"�uA!A!t�A ��A�HA33An�A  A�;A�7Al�A�/A(�A�7AbNA�TA�FA�A�+A�hA;dA"�A��A��A��A�
A33AE�A&�AĜAv�A �A��AK�A�RA��A33A��A�
A��A;dA
��A	�A	p�A	;dA	%A�mA?}A9XAl�AVA�A1'A�-A�hA �!A ^5@�t�@�I�@�ȴ@�=q@��h@�r�@��
@�S�@���@�`B@��j@���@�&�@�b@��
@�F@���@��^@�X@�V@�9X@���@陚@�Z@���@�n�@�z�@㝲@�+@�ȴ@��@���@�X@��;@���@�Ĝ@ղ-@��@�/@д9@�t�@͡�@� �@�dZ@�"�@ʸR@�@�/@�Z@�o@�@ě�@�33@��#@��@���@��@���@�dZ@�S�@�+@�E�@�7L@��j@�"�@�@��^@�G�@�z�@��T@�G�@�(�@��!@�x�@��D@��@�v�@�G�@��@��`@���@��9@���@�Q�@���@�K�@�"�@�n�@�V@�z�@�9X@�9X@�9X@�b@��@��@��@�7L@��u@�K�@���@�r�@�Q�@�(�@�ƨ@��y@�~�@�@�X@��D@���@��j@�r�@� �@���@�o@���@�V@��@��h@�G�@�Z@��@��
@��w@�+@���@���@�-@��@���@�7L@��9@�9X@�  @��
@���@�t�@���@��@�9X@���@�;d@���@��#@���@�@� �@�bN@��
@�E�@��@��#@�@���@�r�@�Q�@��
@�K�@�"�@���@��y@�ȴ@��R@��+@�hs@���@���@���@�bN@�  @��m@�l�@��@��H@���@�ȴ@��!@���@���@��\@��@��-@���@��@�p�@�hs@�`B@�x�@���@��^@��7@��@���@��@�@K�@�@l�@l�@|�@�@}@}�h@}��@}@}�-@}�h@|��@|I�@{ƨ@{��@{t�@{33@z��@xĜ@xQ�@x�9@x��@w|�@v�+@w;d@w��@v{@u�-@u�-@up�@uO�@u/@t�/@tz�@s��@s�@so@r��@r��@r�\@rn�@r�@q�^@q&�@p�u@p�9@p��@p��@p��@q&�@qX@p�`@pQ�@p1'@qhs@q�^@q��@rn�@r�!@r��@r^5@rM�@r-@q�^@q��@qhs@q7L@p��@pbN@pQ�@pA�@o�w@ol�@o\)@o\)@oK�@o+@n��@n�R@nff@m�T@m?}@mV@l�j@lj@l�@kƨ@kC�@j=q@i�^@iX@hr�@g�@g
=@fff@g
=@g��@g�w@g��@g+@e��@eO�@d�@d�@dz�@dz�@d�D@ep�@dj@d9X@c"�@c@b^5@a��@a�#@a��@a��@a�^@ax�@a%@`Ĝ@`r�@_�@_�P@_l�@_+@^�+@^5?@^@]�-@]�@]�@\9X@[��@[�m@[��@[��@[�F@Z�H@Z�!@Z�\@Z�\@ZJ@X��@XbN@X1'@W�@W�P@Wl�@WK�@V�+@V5?@V{@V@U�@U�@U�@U@U`B@U?}@U?}@U/@U/@UV@T�/@T�D@TI�@T9X@T9X@T(�@Sƨ@So@R�\@R^5@R=q@Q��@QX@QG�@P��@PĜ@P�u@P�@PbN@P �@OK�@N�y@N�R@N��@N��@N�+@N�+@N�+@Nff@NV@N5?@N@M?}@MV@L��@L�@L�@L��@L9X@K�
@K��@KdZ@J��@I�#@Ihs@H�`@H�9@Hr�@HA�@H1'@G�;@G|�@G\)@F�y@Fv�@F5?@E/@D�j@D�D@Dj@DZ@C�
@C�@Ct�@CS�@B�H@B^5@A��@A��@A7L@@�`@@�9@@1'@?�@?;d@?�@>��@>�@>�R@>$�@=�@=�T@=��@=@=�-@=@=�-@=�h@=/@<z�@;�F@;t�@;dZ@;C�@;33@;"�@:�@:�!@:n�@:-@9��@9�#@9�^@9�@8Ĝ@8��@8r�@8Q�@8b@7��@7�P@7\)@7K�@7;d@7;d@7�@7
=@6�R@6V@5�@5V@4�@4��@4��@4�D@4Z@3��@3�
@3t�@333@3"�@3o@3o@2�@2��@2�!@2~�@2^5@1��@1�#@1��@1G�@1&�@0�@/�@/�;@/�;@/�P@.��@.�@.�@.��@.��@.ff@.$�@-�-@-/@-V@,�@,z�@,Z@,(�@,1@+�m@+�@+C�@*��@*~�@*M�@*-@*�@*J@)��@)7L@(�9@(�9@(�u@(Q�@(Q�@(A�@(1'@'�@'\)@&v�@&E�@&{@%��@%�@$��@$�j@$�D@$j@$9X@$�@#�
@#�F@#�F@#��@#�@#C�@"�H@"��@"��@"��@"^5@!�#@!�@!%@ �@ bN@ Q�@ A�@ A�@  �@|�@K�@;d@ȴ@��@ff@5?@��@`B@�@��@��@�@�D@(�@��@�m@�
@�F@"�@��@M�@�@��@�^@��@��@x�@G�@�@��@�`@Ĝ@��@�u@�@�@�@�@bN@A�@1'@1'@ �@b@�@|�@�@�y@�R@v�@E�@�@@�-@�h@�@/@V@��@�D@z�@z�@j@9X@�@�m@��@dZ@33@"�@"�@o@��@��@M�@��@��@�@�^@X@%@�`@�9@�@bN@A�@ �@�@�w@\)@
=@��@��@�y@�@�+@5?@�@�h@p�@?}@��@�j@�@z�@�@�
@�F@�F@�F@��@�@
�H@
��@
~�@
^5@
J@	��@	�7@	hs@	G�@	�@��@��@Ĝ@�9@��@r�@A�@1'@ �@b@�@�@K�@��@�R@��@�+@ff@ff@{@�@@�-@�h@�h@�h@�h@�h@�h@�h@�h@�@O�@/@�@��@�@�@�@�@�/@�j@�j@�@��@��@��@�D@j@Z@I�@I�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�XA�S�A�S�A�XA�XA�VA�VA�VA�VA�S�A�S�A�Q�A�S�A�S�A�O�A�K�A�K�A�K�A�33A��A��mAӋDA��A��/A�&�A���A�~�A��A���A���A�{A�bNA��yA��!A�z�A��-A��/A��A�ffA���A��A�\)A���A�bNA��mA��^A�C�A��FA�"�A��9A�7LA�p�A�A�A�I�A��A��A�\)A�{A�ĜA���A�l�A��jA��
A�~�A��PA���A�hsA�S�A�/A���A�ffA~z�A}+A{��A{G�AzVAx1Av�Av�9Au��AtffAr�9Aq"�ApM�AoƨAo7LAn~�AmAl�yAk�Ah�\Ag�Ae��Ad�Ad9XAc��Ac��Abr�A_��A_A^jA^{A\�HA[�A[�#A[dZAY�-AX��AW��AWK�AV�9AVr�AVVAV �AU�mAU��AU�AT�9ASoARz�AP��AOl�AO"�AN��ANv�AL�\AJ$�AH�/AHI�AH1'AH�AHbAH  AG�TAG��AG+AF�/AF��AF�RAF9XAD�yAC��AC
=AAƨA@��A@I�A?l�A>5?A=\)A<��A<^5A;�A:�`A:�uA9��A8�A7��A61A5�7A5
=A3K�A2ffA0�DA/O�A.�yA.r�A.JA-��A-��A-x�A-l�A-hsA-`BA-\)A-;dA,��A,r�A+��A)�TA(ffA'|�A'%A&z�A%��A$��A#�A"�uA!A!t�A ��A�HA33An�A  A�;A�7Al�A�/A(�A�7AbNA�TA�FA�A�+A�hA;dA"�A��A��A��A�
A33AE�A&�AĜAv�A �A��AK�A�RA��A33A��A�
A��A;dA
��A	�A	p�A	;dA	%A�mA?}A9XAl�AVA�A1'A�-A�hA �!A ^5@�t�@�I�@�ȴ@�=q@��h@�r�@��
@�S�@���@�`B@��j@���@�&�@�b@��
@�F@���@��^@�X@�V@�9X@���@陚@�Z@���@�n�@�z�@㝲@�+@�ȴ@��@���@�X@��;@���@�Ĝ@ղ-@��@�/@д9@�t�@͡�@� �@�dZ@�"�@ʸR@�@�/@�Z@�o@�@ě�@�33@��#@��@���@��@���@�dZ@�S�@�+@�E�@�7L@��j@�"�@�@��^@�G�@�z�@��T@�G�@�(�@��!@�x�@��D@��@�v�@�G�@��@��`@���@��9@���@�Q�@���@�K�@�"�@�n�@�V@�z�@�9X@�9X@�9X@�b@��@��@��@�7L@��u@�K�@���@�r�@�Q�@�(�@�ƨ@��y@�~�@�@�X@��D@���@��j@�r�@� �@���@�o@���@�V@��@��h@�G�@�Z@��@��
@��w@�+@���@���@�-@��@���@�7L@��9@�9X@�  @��
@���@�t�@���@��@�9X@���@�;d@���@��#@���@�@� �@�bN@��
@�E�@��@��#@�@���@�r�@�Q�@��
@�K�@�"�@���@��y@�ȴ@��R@��+@�hs@���@���@���@�bN@�  @��m@�l�@��@��H@���@�ȴ@��!@���@���@��\@��@��-@���@��@�p�@�hs@�`B@�x�@���@��^@��7@��@���@��@�@K�@�@l�@l�@|�@�@}@}�h@}��@}@}�-@}�h@|��@|I�@{ƨ@{��@{t�@{33@z��@xĜ@xQ�@x�9@x��@w|�@v�+@w;d@w��@v{@u�-@u�-@up�@uO�@u/@t�/@tz�@s��@s�@so@r��@r��@r�\@rn�@r�@q�^@q&�@p�u@p�9@p��@p��@p��@q&�@qX@p�`@pQ�@p1'@qhs@q�^@q��@rn�@r�!@r��@r^5@rM�@r-@q�^@q��@qhs@q7L@p��@pbN@pQ�@pA�@o�w@ol�@o\)@o\)@oK�@o+@n��@n�R@nff@m�T@m?}@mV@l�j@lj@l�@kƨ@kC�@j=q@i�^@iX@hr�@g�@g
=@fff@g
=@g��@g�w@g��@g+@e��@eO�@d�@d�@dz�@dz�@d�D@ep�@dj@d9X@c"�@c@b^5@a��@a�#@a��@a��@a�^@ax�@a%@`Ĝ@`r�@_�@_�P@_l�@_+@^�+@^5?@^@]�-@]�@]�@\9X@[��@[�m@[��@[��@[�F@Z�H@Z�!@Z�\@Z�\@ZJ@X��@XbN@X1'@W�@W�P@Wl�@WK�@V�+@V5?@V{@V@U�@U�@U�@U@U`B@U?}@U?}@U/@U/@UV@T�/@T�D@TI�@T9X@T9X@T(�@Sƨ@So@R�\@R^5@R=q@Q��@QX@QG�@P��@PĜ@P�u@P�@PbN@P �@OK�@N�y@N�R@N��@N��@N�+@N�+@N�+@Nff@NV@N5?@N@M?}@MV@L��@L�@L�@L��@L9X@K�
@K��@KdZ@J��@I�#@Ihs@H�`@H�9@Hr�@HA�@H1'@G�;@G|�@G\)@F�y@Fv�@F5?@E/@D�j@D�D@Dj@DZ@C�
@C�@Ct�@CS�@B�H@B^5@A��@A��@A7L@@�`@@�9@@1'@?�@?;d@?�@>��@>�@>�R@>$�@=�@=�T@=��@=@=�-@=@=�-@=�h@=/@<z�@;�F@;t�@;dZ@;C�@;33@;"�@:�@:�!@:n�@:-@9��@9�#@9�^@9�@8Ĝ@8��@8r�@8Q�@8b@7��@7�P@7\)@7K�@7;d@7;d@7�@7
=@6�R@6V@5�@5V@4�@4��@4��@4�D@4Z@3��@3�
@3t�@333@3"�@3o@3o@2�@2��@2�!@2~�@2^5@1��@1�#@1��@1G�@1&�@0�@/�@/�;@/�;@/�P@.��@.�@.�@.��@.��@.ff@.$�@-�-@-/@-V@,�@,z�@,Z@,(�@,1@+�m@+�@+C�@*��@*~�@*M�@*-@*�@*J@)��@)7L@(�9@(�9@(�u@(Q�@(Q�@(A�@(1'@'�@'\)@&v�@&E�@&{@%��@%�@$��@$�j@$�D@$j@$9X@$�@#�
@#�F@#�F@#��@#�@#C�@"�H@"��@"��@"��@"^5@!�#@!�@!%@ �@ bN@ Q�@ A�@ A�@  �@|�@K�@;d@ȴ@��@ff@5?@��@`B@�@��@��@�@�D@(�@��@�m@�
@�F@"�@��@M�@�@��@�^@��@��@x�@G�@�@��@�`@Ĝ@��@�u@�@�@�@�@bN@A�@1'@1'@ �@b@�@|�@�@�y@�R@v�@E�@�@@�-@�h@�@/@V@��@�D@z�@z�@j@9X@�@�m@��@dZ@33@"�@"�@o@��@��@M�@��@��@�@�^@X@%@�`@�9@�@bN@A�@ �@�@�w@\)@
=@��@��@�y@�@�+@5?@�@�h@p�@?}@��@�j@�@z�@�@�
@�F@�F@�F@��@�@
�H@
��@
~�@
^5@
J@	��@	�7@	hs@	G�@	�@��@��@Ĝ@�9@��@r�@A�@1'@ �@b@�@�@K�@��@�R@��@�+@ff@ff@{@�@@�-@�h@�h@�h@�h@�h@�h@�h@�h@�@O�@/@�@��@�@�@�@�@�/@�j@�j@�@��@��@��@�D@j@Z@I�@I�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�3B�LB�qB�}B�RB�uBv�BdZB6FB1B�BB��Bk�BR�B6FB"�B�B
=B��B��B�B�BBĜB��B�uB�JB�B}�Bw�Bs�Bn�BgmB_;BR�BG�B5?B-B&�BVBB
��B
�B
�B
��B
ƨB
�XB
�9B
�-B
�!B
�B
��B
�uB
�7B
|�B
x�B
u�B
dZB
\)B
ZB
S�B
F�B
<jB
/B
&�B
"�B
�B
�B
�B
oB
B	�B	�)B	��B	��B	��B	��B	ɺB	ÖB	�9B	�B	��B	��B	��B	��B	��B	��B	�=B	�B	� B	}�B	z�B	y�B	z�B	z�B	z�B	z�B	z�B	x�B	p�B	jB	e`B	W
B	Q�B	O�B	K�B	?}B	33B	.B	+B	)�B	)�B	)�B	(�B	(�B	'�B	&�B	$�B	$�B	#�B	"�B	�B	�B	�B	hB	bB	PB	
=B	%B	B��B��B��B��B��B�B�B�yB�`B�NB�5B�#B��B��BȴBǮBŢBĜBÖBB��B��B��B��B��B��B�}B�qB�XB�-B�B��B��B��B��B��B��B�uB�bB�VB�PB�%B�B~�B|�B|�B{�Bz�By�Bw�Bw�Bt�Br�Bp�Bo�Bn�Bk�BjBiyBiyBhsBgmBgmBdZBbNB_;B^5B]/B[#BZBXBW
BVBS�BS�BQ�BQ�BO�BO�BN�BL�BK�BJ�BJ�BG�BG�BE�BD�BC�BB�BA�B@�B=qB;dB:^B9XB7LB6FB5?B5?B49B33B33B2-B1'B2-B1'B/B.B.B.B-B-B-B-B,B,B,B+B,B,B+B+B+B+B+B,B,B,B+B/B1'B1'B1'B2-B2-B49B49B49B49B49B49B5?B7LB7LB9XB:^B;dB<jB=qB?}B?}B?}B>wB>wB@�B@�B@�BE�BD�BE�BE�BF�BK�BI�BK�BM�BP�BP�BQ�BS�BVBVBW
BW
BW
BW
BXBZBZBZB]/B`BBaHBbNBbNBbNBbNBcTBdZBffBhsBhsBk�Bo�Br�Bt�Bu�Bv�Bx�Bx�By�By�Bz�B{�B~�B�B�B�%B�1B�7B�DB�JB�PB�\B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�3B�3B�?B�LB�jB�}B�}B�}BŢB��B��B��B�B�B�5B�HB�TB�`B�yB�B�B�B�B�B�B�B��B��B��B��B��B��B��B	B	B	B	%B	+B	+B	+B	1B	VB	hB	uB	�B	�B	�B	�B	�B	%�B	'�B	)�B	+B	)�B	-B	1'B	33B	5?B	6FB	;dB	A�B	K�B	N�B	P�B	T�B	XB	YB	YB	ZB	YB	[#B	]/B	_;B	bNB	ffB	e`B	e`B	ffB	iyB	hsB	gmB	iyB	k�B	k�B	l�B	n�B	o�B	o�B	o�B	q�B	r�B	u�B	x�B	z�B	{�B	|�B	|�B	}�B	� B	�B	�B	�B	�%B	�%B	�+B	�7B	�DB	�JB	�VB	�\B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�3B	�3B	�3B	�3B	�9B	�9B	�?B	�LB	�RB	�RB	�RB	�XB	�XB	�XB	�^B	�^B	�dB	�dB	�jB	�wB	�}B	��B	ĜB	ǮB	ȴB	ȴB	ɺB	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�/B	�5B	�BB	�BB	�NB	�TB	�TB	�`B	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
1B
1B
1B
	7B
	7B
	7B
	7B
DB
DB
JB
JB
PB
VB
VB
VB
VB
VB
VB
VB
bB
bB
hB
hB
bB
bB
hB
oB
oB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
"�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
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
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
1'B
2-B
2-B
33B
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
9XB
9XB
9XB
9XB
:^B
;dB
:^B
;dB
;dB
;dB
;dB
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
?}B
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
A�B
A�B
A�B
A�B
B�B
C�B
C�B
C�B
D�B
D�B
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
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
J�B
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
N�B
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
Q�B
Q�B
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
XB
XB
XB
XB
XB
XB
YB
YB
YB
YB
YB
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
]/B
]/B
]/B
]/B
]/B
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
bNB
bNB
bNB
bNB
cTB
cTB
dZB
e`B
ffB
ffB
ffB
ffB
gmB
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
r�B
r�B
r�B
s�B
s�B
r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�
B�
B�
B�
B�
B�
B�
B�
B�
B�
B�
B�
B�$B�$B�
B�*B�_B��B��B�B��B��BªB�B��B|�Bq'BD�B�B�qB��Br�BYKB:*B%�B�B�B�(B��B�[B�B�B�TB�B��B��BcBy$Bu?Bp�BjeBa�BV�BK�B7�B0�B+�BNB�B
��B
�qB
ڠB
�2B
ȀB
��B
��B
��B
�'B
��B
�sB
�MB
��B
}�B
zxB
xB
e�B
\�B
[�B
U�B
H�B
>(B
0!B
'�B
#�B
�B
�B
+B
�B
�B	�wB	ݲB	�SB	��B	�pB	��B	ˬB	�?B	�tB	� B	��B	�_B	��B	�+B	��B	��B	��B	�%B	�B	~�B	{JB	zB	{JB	{JB	{JB	{�B	|PB	z�B	q�B	l�B	f�B	W�B	R�B	QNB	NVB	BAB	4�B	.�B	+QB	*KB	*0B	*KB	)_B	)yB	(�B	'mB	%FB	%`B	$�B	$tB	 'B	B	+B	�B	�B	�B	�B	_B	B��B��B�<B��B�B�B�UB�B�fB�B�vB��B�SB�NB�lB�fB�?B�B��B��B��B��B��B��B��B�;B��B��B��B�B�CB��B��B�&B�-B�jB��B��B�hB��B��B�1B�B�B}qB}�B|jB{�Bz�By	By>Bu�Bs3BqvBqBo�Bl"Bj�Bi�Bi�Bi*Bh�Bh�Be�Bc�B_�B^�B]�B[�BZ�BYBXEBW$BT�BUBR�BR�BP�BQBO�BMjBL�BLJBK�BIRBH�BFtBE�BD�BC�BDBA�B>]B<�B<PB:^B7�B6�B6+B5�B4�B4B4B2�B2-B3�B1�B/�B.}B.�B/ B-�B-�B.B./B-B,�B,�B,WB-]B,�B+�B+�B,"B,�B,�B-CB-CB-�B-]B1AB1�B1�B2GB3�B3MB4�B4�B4�B4�B5B5%B6`B8lB8RB:xB;JB;�B="B>BB?�B?�B?�B>�B?HBAUBA;BA�BFtBEBF?BF�BHKBL~BJ�BL�BN�BQ�BQ�BR�BT�BVSBVmBWYBWYBWYBW�BX�BZ�BZ�B[	B^5B`�Ba�Bb�Bb�Bb�BcBd&BeFBgBi_Bi�Bl�BpoBsBu%BvFBw�ByrByrBzxBz�Bz�B|BcB�oB��B��B��B��B��B��B��B�B��B��B��B�
B��B��B�=B�B�B�;B�NB�ZB�2B�8B�>B�XB��B�=B�B��B��B�B��B��B��B��B��B�YB��B�NB�FB�mB��B޸B�B��B��B��B��B�B��B��B��B�}B�B�%B�B�2B�8B�*B�^B�cB	MB	mB	mB	YB	zB	zB	zB	�B	�B	�B	�B	�B	�B	�B	�B	�B	%�B	(>B	*eB	+kB	*�B	-]B	1vB	3hB	5ZB	6zB	;�B	A�B	LdB	OB	Q B	UB	XEB	YKB	Y�B	ZkB	YB	[qB	]dB	_�B	b�B	gB	e�B	ezB	f�B	jB	h�B	gmB	i�B	l=B	k�B	l�B	n�B	o�B	o�B	o�B	q�B	r�B	vB	y$B	{0B	|B	}<B	}"B	~BB	�iB	�[B	�gB	�9B	�?B	�YB	�_B	�RB	�^B	��B	��B	��B	�4B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�2B	�B	�>B	�6B	�WB	�]B	�UB	�aB	�MB	�hB	�hB	�hB	�nB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�oB	ĜB	��B	��B	�B	�=B	�B	�	B	��B	��B	�B	��B	��B	�gB	�aB	�gB	�2B	�mB	�eB	�WB	�IB	ބB	�vB	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�-B	��B	��B	��B	�B	�?B	�B	�B	�	B	�B	�*B	�B	�dB	�"B	�"B	�B	�B	�B	�"B	�"B	�(B	�.B	�B	�B	�.B
 4B
 4B
 4B
;B
 B
 B
UB
UB
[B
aB
MB
MB
mB
mB
YB
fB
fB
fB
	lB
	lB
	�B
	�B
�B
xB
~B
dB
jB
pB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
	B
�B
�B
�B
�B
�B
�B
�B
�B
B
 B
 'B
!-B
"B
"B
!�B
"B
#:B
$&B
%B
%B
%B
%B
%,B
&2B
'B
'B
'B
'B
'B
'8B
'B
'8B
'mB
)DB
*0B
*0B
*KB
*0B
*0B
*KB
*KB
+6B
+QB
,=B
,=B
,=B
,WB
-CB
-CB
-CB
-CB
.IB
.IB
.IB
/iB
/OB
/5B
0;B
0UB
0UB
0oB
0oB
1�B
2aB
2aB
3hB
3hB
3hB
3hB
4nB
4nB
4�B
5tB
5ZB
5tB
5ZB
5tB
5tB
6zB
6zB
6zB
6zB
6zB
7�B
7�B
7�B
7�B
8�B
9rB
9�B
9�B
9�B
:�B
;�B
:�B
;B
;�B
;�B
;�B
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
?�B
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
A�B
A�B
A�B
A�B
B�B
DB
C�B
C�B
D�B
D�B
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
G�B
G�B
G�B
H�B
H�B
H�B
IB
IB
I�B
J#B
K)B
K�B
K�B
K�B
L�B
MB
MB
NB
NB
N"B
NB
NB
OB
O(B
O(B
PB
P.B
PB
PB
PB
PB
QB
Q B
Q4B
QB
Q4B
Q4B
R B
R:B
R:B
RB
RB
RB
S&B
S@B
S@B
S@B
SB
S&B
S@B
SB
TB
T,B
TB
TB
T,B
T,B
UB
UB
UB
U2B
U2B
UMB
U2B
U2B
U2B
V9B
W?B
W?B
W?B
XEB
XEB
X_B
XEB
XEB
XEB
YKB
Y1B
YKB
YKB
YKB
ZQB
ZQB
ZQB
[WB
[WB
[=B
\]B
\xB
\]B
\]B
\]B
]dB
]IB
]dB
]dB
]dB
^jB
^jB
^�B
^�B
_pB
_pB
_pB
_pB
_pB
_pB
_pB
`\B
`\B
`\B
`vB
`vB
`vB
`vB
a|B
a|B
a|B
b�B
b�B
b�B
b�B
c�B
c�B
d�B
ezB
f�B
f�B
f�B
f�B
g�B
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
j�B
j�B
j�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
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
r�B
r�B
r�B
s�B
s�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<K+<ea�<4;@<AV�<2��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.23(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201610180051262016101800512620161018005126201806221215142018062212151420180622121514201804050408022018040504080220180405040802  JA  ARFMdecpA19c                                                                20161019123515  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20161019033618  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20161019033619  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20161019033619  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20161019033620  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20161019033620  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20161019033620  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20161019033620  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20161019033620  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20161019033621                      G�O�G�O�G�O�                JA  ARUP                                                                        20161019043717                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20161017155006  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20161017155126  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20161017155126  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20161018000000  CF  PSAL_ADJUSTED_QCD�&fD�&fG�O�                JM  ARSQOW  1.1 2017V1                                                          20180404190802  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031514  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115111518                      G�O�G�O�G�O�                