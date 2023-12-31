CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-12-29T00:35:23Z creation;2016-12-29T00:35:25Z conversion to V3.1;2019-12-19T08:18:37Z update;     
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
_FillValue                 �  I(   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �x   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �\   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �<   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �T   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �p   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �p   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �p   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �p   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �    HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �@   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �P   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �T   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �d   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �h   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �l   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �pArgo profile    3.1 1.2 19500101000000  20161229003523  20200116211516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               IA   JA  I2_0577_073                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @��6 �Q 1   @��6�s��@2�*0U2a�d�Fs���1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C�C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D y�D ��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� DfD�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D���D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D�|�D��D�  D�@ D� D�� D�3D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @~{@���@���AQ�A<Q�A\Q�A|Q�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B{B{B{B{B'{B/{B7{B?{BG{BO{BW{B_{Bg{Bo{Bw{B{B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=BÊ=BǊ=Bˊ=Bϊ=Bӊ=B׊=Bۊ=Bߊ=B�=B�=B�=B�=B�=B��=B��=B��=C�C�C�C�C	�C�C�C�C�C޸C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��D j�D ��DqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HD	qHD	�HD
qHD
�HDqHD�HDqHD�HDqHD��Dw�D�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HD qHD �HD!qHD!�HD"qHD"�HD#qHD#�HD$qHD$�HD%qHD%�HD&qHD&�HD'qHD'�HD(qHD(�HD)qHD)�HD*qHD*�HD+qHD+�HD,qHD,�HD-qHD-�HD.qHD.�HD/qHD/�HD0qHD0�HD1qHD1�HD2qHD2�HD3qHD3�HD4qHD4�HD5qHD5�HD6qHD6�HD7qHD7�HD8qHD8�HD9qHD9�HD:qHD:�HD;qHD;�HD<qHD<�HD=qHD=�HD>qHD>�HD?qHD?�HD@qHD@�HDAqHDA�HDBqHDB�HDCqHDC�HDDqHDD�HDEqHDE�HDFqHDF�HDGqHDG�HDHqHDH�HDIqHDI�HDJqHDJ�HDKqHDK�HDLqHDL�HDMqHDM�HDNqHDN�HDOqHDO�HDPqHDP�HDQqHDQ�HDRqHDR�HDSqHDS�HDTqHDT�HDUqHDU�HDVqHDV�HDWqHDW�HDXqHDX�HDYqHDY�HDZqHDZ�HD[qHD[�HD\qHD\�HD]qHD]�HD^qHD^�HD_qHD_�HD`qHD`�HDaqHDa�HDbqHDb�HDcqHDc�HDdqHDd�HDeqHDe�HDfqHDf�HDgqHDg�HDhqHDh�HDiqHDi�HDjqHDj�HDkqHDk�HDlqHDl�HDmqHDm�HDnqHDn�HDoqHDo�HDpqHDp�HDqqHDq�HDrqHDr�HDsqHDs�HDtqHDt�HDuqHDu�HDvqHDv�HDwqHDw�HDxqHDx�HDyqHDy�HDzqHDz�HD{qHD{�HD|qHD|�HD}qHD}�HD~qHD~�HDqHD�HD�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�{�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�5qD�uqD���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D¸�D���D�8�D�x�Dø�D���D�8�D�x�Dĸ�D���D�8�D�x�DŸ�D���D�8�D�x�DƸ�D���D�8�D�x�DǸ�D���D�8�D�x�Dȸ�D��qD�8�D�x�Dɸ�D���D�8�D�x�Dʸ�D���D�8�D�x�D˸�D���D�8�D�x�D̸�D���D�8�D�x�D͸�D���D�8�D�x�Dθ�D���D�8�D�x�Dϸ�D���D�8�D�x�Dи�D���D�8�D�x�DѸ�D���D�8�D�x�DҸ�D���D�8�D�x�DӸ�D���D�8�D�x�DԸ�D���D�8�D�x�Dո�D���D�8�D�x�Dָ�D���D�8�D�x�D׸�D���D�8�D�x�Dظ�D���D�8�D�x�Dٸ�D���D�8�D�x�Dڸ�D���D�8�D�x�D۸�D���D�8�D�x�Dܸ�D���D�8�D�x�Dݸ�D���D�8�D�x�D޸�D���D�8�D�x�D߸�D���D�8�D�x�DฤD���D�8�D�x�DḤD���D�8�D�uqD�qD���D�8�D�x�D㸤D���D�8�D�x�D两D���D�;�D�x�D帤D���D�8�D�x�D渤D���D�8�D�x�D縤D���D�8�D�x�D踤D���D�8�D�x�D鸤D���D�8�D�x�D�qD���D�8�D�x�D븤D���D�8�D�x�D츤D���D�8�D�x�D���D���D�8�D�x�DD���D�8�D�x�D︤D���D�8�D�x�D�D���D�8�D�x�D�D���D�8�D�x�D�D���D�8�D�x�D�D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�dZA�dZA�jA�x�A�t�A�l�A�l�A�z�A�|�A�~�A�v�AуAѺ^A��A���A��A�^5A�p�A�p�A�p�A�p�A�p�A�p�A�n�A�r�A�t�A�v�A�x�A�z�A�|�AҁA҃A҅AҍPAҍPAҋDA҉7A҉7A҉7AҍPAҏ\Aҏ\AґhAґhAґhAҕ�Aҡ�AҰ!A�x�A�I�A��HA�v�A�ƨA�O�A��A�ƨA��Aϛ�A��`A�E�A��Aͧ�A��HAʼjA��mA���A��Aô9A�ZA�|�A��A��mA��A��TA���A���A��uA���A���A�;dA�oA���A��A���A��#A�\)A�A�E�A�ȴA�ĜA�=qA�?}A���A��FA�jA��A���A���A�^5A���A�;dA��TA�dZA� �A��jA���A���A���A�A�A�1'A�A�|�A��A��A�\)A���A�+A�hsA���A��wA��AhsA~ �A}�PA|=qAz��AyO�Aw/Av$�At��AqAo�7Am?}AlbAk�-Ai�Ag&�Ae
=Aa�PA`�uA_ƨA_%A^1A\�AY��AW�;AVVAUO�AS�hAR��AQ�
AO;dAN�AMG�AL(�AIhsAH�AF��AE��AE"�ACƨAB �A@�`A?��A>=qA=VA<�DA:�HA:bA9+A8(�A6�A4�A4~�A4Q�A2�A1oA0VA/l�A.��A-��A,��A,(�A*��A*A�A)A'�A&jA%�hA%+A$r�A#��A!?}A 5?A�-A\)A�/A9XA�PA�A�#A�`AZA�wA+A�#AS�A�jAI�A�A�!A�A��A�A��A	�AM�A�RA�A�9A�PA��A�PA ��@��;@�
=@�^5@���@�33@�b@�5?@��H@��@���@��
@�P@�o@�$�@���@�j@�t�@��@�p�@�D@�
=@�M�@�`B@��D@�9X@� �@��@�7L@�dZ@١�@ى7@�X@�  @�ff@Չ7@Ԭ@�1@�\)@���@ҟ�@��T@υ@�@�v�@͡�@�Q�@�t�@��H@��@�V@�j@�j@�(�@�  @ǶF@��@���@ļj@��;@�S�@�;d@�+@�ff@�hs@��`@�A�@�+@�5?@�hs@��D@��w@�;d@���@�n�@��h@��`@��@�b@��@�E�@���@���@�J@�M�@��H@�C�@�K�@�ff@�Z@���@�hs@�?}@���@�9X@�z�@�Q�@�V@�O�@�n�@�{@��-@��-@��@���@���@�~�@��`@��@��`@�j@�v�@��@��#@���@��@���@�r�@�r�@�A�@�1@���@���@�~�@�=q@�@���@��@�X@�7L@�V@���@�(�@�|�@�+@��H@�v�@�M�@�{@�`B@���@���@�j@�A�@�b@�ƨ@���@��@��m@�r�@���@���@�1'@�dZ@���@��+@�$�@��T@���@�@��7@��/@�z�@�Z@��j@��@�C�@�+@�C�@��!@�E�@�5?@�$�@�V@���@���@���@�v�@�O�@��@��9@���@��D@���@��9@�Q�@�bN@�I�@��
@�K�@�
=@���@��!@�ȴ@��@��+@���@�=q@��#@�ff@�M�@�J@�V@��@���@�j@��@���@���@�K�@��@���@���@���@��+@���@��@��T@��#@���@�@�$�@�^5@��#@�p�@��`@�Z@��m@�  @���@�A�@�1@�ƨ@��
@�\)@���@��w@�+@�n�@�=q@�=q@���@��@�%@���@�j@�Q�@�1'@��@���@���@�|�@�\)@�dZ@�;d@�33@���@���@���@�~�@�^5@�5?@��@��@��T@���@��-@��7@�?}@�V@��/@���@��j@��9@���@��D@�r�@� �@���@��@�"�@��@��+@�^5@�E�@�$�@���@��-@���@���@�x�@��@���@��@�j@�(�@� �@�  @���@�\)@�"�@�"�@�
=@���@���@�E�@�@��@��#@�p�@��`@���@��j@��u@�r�@�(�@�  @�@\)@K�@+@~�y@~�R@~��@~ff@~5?@~$�@~{@}�h@|9X@|1@{dZ@z��@z-@y�^@yX@y%@x�9@xbN@x1'@xb@x  @w�;@w��@w\)@v��@u@t��@t��@tI�@sS�@s"�@r�H@r��@r��@r�@rJ@q��@qX@p�`@pQ�@pb@o\)@nȴ@n5?@m�@m�T@m��@m`B@l��@lz�@lZ@l9X@l�@k�
@kdZ@k"�@j�H@j�!@jM�@i�^@ihs@i%@h�u@g�@g�P@gK�@f�@fȴ@fȴ@fȴ@f�R@fE�@f{@f@ep�@eV@d��@d�@dz�@d9X@c�
@c��@cC�@b�H@b=q@a�#@a��@ahs@a&�@a%@`��@`Ĝ@`bN@_�@_|�@_|�@_K�@]��@]�@]�@\�/@\Z@[ƨ@["�@Z�!@Z~�@Z^5@Z-@Z�@Z�@Z�@Z�@Y�#@Y�7@Yx�@Y&�@X1'@X  @W��@V5?@U��@U@Up�@T�@T1@S��@St�@SC�@R�@RM�@Q�@Q�^@QX@P�9@P  @O�w@O�@Nȴ@N��@N$�@M�-@MO�@L�@LI�@L9X@K��@K�@KC�@J�!@J�\@J~�@J-@I�#@I��@IX@I&�@H��@H��@HbN@H �@G�@G��@Gl�@G+@F�+@F$�@E@E�-@E��@E�h@E`B@E?}@E�@EV@D�@Dz�@D1@CdZ@B�H@B��@B�!@BJ@A��@A%@@��@@r�@@  @?;d@>��@>E�@>5?@>$�@>@=��@=�h@=`B@=�@<�j@<j@;�m@;�F@;��@;�@;t�@;o@:��@:��@:-@9��@9%@8�@8r�@8Q�@7�@7��@7l�@6�y@6�+@6@5@5�@5`B@5?}@5V@4�@4��@4�D@4z�@4(�@3�F@3dZ@3"�@2�@2�H@2��@2��@2�\@2=q@1�#@1G�@0��@0�9@0r�@01'@/��@/l�@/K�@/
=@.v�@.$�@-@,��@,�D@,1@+�
@+�
@+ƨ@+��@+�@+S�@+33@+@*�H@*��@*=q@)��@)X@(�`@(Ĝ@(�9@(A�@(  @'�;@'�w@'�@'��@'�@'��@'|�@&�@&E�@&$�@%�@%@%��@%�@%�@$�/@$Z@#��@#��@#��@#��@#�@#t�@#"�@"�@"��@"M�@"�@!��@!��@!��@!hs@!G�@ ��@ Ĝ@ ��@ r�@ 1'@�;@�@|�@\)@;d@+@�@�y@�+@{@��@@�-@��@��@`B@�@��@��@j@9X@(�@�m@��@��@t�@��@��@t�@dZ@t�@�@dZ@C�@��@�\@n�@^5@M�@�@��@��@hs@��@�9@�@A�@1'@ �@b@  @  @�@�w@l�@\)@;d@��@v�@v�@v�@v�@ff@V@5?@@@�T@��@`B@/@��@j@I�@I�@1@�@dZ@S�@C�@33@"�@o@o@�@��@^5@��@��@�7@hs@hs@G�@7L@%@Ĝ@�u@�@Q�@ �@b@�@�@�;@�;@�;@�@�;@�@\)@�y@��@v�@E�@�T@@�-@��@`B@V@�/@��@j@Z@9X@(�@(�@�@�m@ƨ@��@dZ@C�@C�@C�@C�@C�@C�@C�@o@11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�dZA�dZA�jA�x�A�t�A�l�A�l�A�z�A�|�A�~�A�v�AуAѺ^A��A���A��A�^5A�p�A�p�A�p�A�p�A�p�A�p�A�n�A�r�A�t�A�v�A�x�A�z�A�|�AҁA҃A҅AҍPAҍPAҋDA҉7A҉7A҉7AҍPAҏ\Aҏ\AґhAґhAґhAҕ�Aҡ�AҰ!A�x�A�I�A��HA�v�A�ƨA�O�A��A�ƨA��Aϛ�A��`A�E�A��Aͧ�A��HAʼjA��mA���A��Aô9A�ZA�|�A��A��mA��A��TA���A���A��uA���A���A�;dA�oA���A��A���A��#A�\)A�A�E�A�ȴA�ĜA�=qA�?}A���A��FA�jA��A���A���A�^5A���A�;dA��TA�dZA� �A��jA���A���A���A�A�A�1'A�A�|�A��A��A�\)A���A�+A�hsA���A��wA��AhsA~ �A}�PA|=qAz��AyO�Aw/Av$�At��AqAo�7Am?}AlbAk�-Ai�Ag&�Ae
=Aa�PA`�uA_ƨA_%A^1A\�AY��AW�;AVVAUO�AS�hAR��AQ�
AO;dAN�AMG�AL(�AIhsAH�AF��AE��AE"�ACƨAB �A@�`A?��A>=qA=VA<�DA:�HA:bA9+A8(�A6�A4�A4~�A4Q�A2�A1oA0VA/l�A.��A-��A,��A,(�A*��A*A�A)A'�A&jA%�hA%+A$r�A#��A!?}A 5?A�-A\)A�/A9XA�PA�A�#A�`AZA�wA+A�#AS�A�jAI�A�A�!A�A��A�A��A	�AM�A�RA�A�9A�PA��A�PA ��@��;@�
=@�^5@���@�33@�b@�5?@��H@��@���@��
@�P@�o@�$�@���@�j@�t�@��@�p�@�D@�
=@�M�@�`B@��D@�9X@� �@��@�7L@�dZ@١�@ى7@�X@�  @�ff@Չ7@Ԭ@�1@�\)@���@ҟ�@��T@υ@�@�v�@͡�@�Q�@�t�@��H@��@�V@�j@�j@�(�@�  @ǶF@��@���@ļj@��;@�S�@�;d@�+@�ff@�hs@��`@�A�@�+@�5?@�hs@��D@��w@�;d@���@�n�@��h@��`@��@�b@��@�E�@���@���@�J@�M�@��H@�C�@�K�@�ff@�Z@���@�hs@�?}@���@�9X@�z�@�Q�@�V@�O�@�n�@�{@��-@��-@��@���@���@�~�@��`@��@��`@�j@�v�@��@��#@���@��@���@�r�@�r�@�A�@�1@���@���@�~�@�=q@�@���@��@�X@�7L@�V@���@�(�@�|�@�+@��H@�v�@�M�@�{@�`B@���@���@�j@�A�@�b@�ƨ@���@��@��m@�r�@���@���@�1'@�dZ@���@��+@�$�@��T@���@�@��7@��/@�z�@�Z@��j@��@�C�@�+@�C�@��!@�E�@�5?@�$�@�V@���@���@���@�v�@�O�@��@��9@���@��D@���@��9@�Q�@�bN@�I�@��
@�K�@�
=@���@��!@�ȴ@��@��+@���@�=q@��#@�ff@�M�@�J@�V@��@���@�j@��@���@���@�K�@��@���@���@���@��+@���@��@��T@��#@���@�@�$�@�^5@��#@�p�@��`@�Z@��m@�  @���@�A�@�1@�ƨ@��
@�\)@���@��w@�+@�n�@�=q@�=q@���@��@�%@���@�j@�Q�@�1'@��@���@���@�|�@�\)@�dZ@�;d@�33@���@���@���@�~�@�^5@�5?@��@��@��T@���@��-@��7@�?}@�V@��/@���@��j@��9@���@��D@�r�@� �@���@��@�"�@��@��+@�^5@�E�@�$�@���@��-@���@���@�x�@��@���@��@�j@�(�@� �@�  @���@�\)@�"�@�"�@�
=@���@���@�E�@�@��@��#@�p�@��`@���@��j@��u@�r�@�(�@�  @�@\)@K�@+@~�y@~�R@~��@~ff@~5?@~$�@~{@}�h@|9X@|1@{dZ@z��@z-@y�^@yX@y%@x�9@xbN@x1'@xb@x  @w�;@w��@w\)@v��@u@t��@t��@tI�@sS�@s"�@r�H@r��@r��@r�@rJ@q��@qX@p�`@pQ�@pb@o\)@nȴ@n5?@m�@m�T@m��@m`B@l��@lz�@lZ@l9X@l�@k�
@kdZ@k"�@j�H@j�!@jM�@i�^@ihs@i%@h�u@g�@g�P@gK�@f�@fȴ@fȴ@fȴ@f�R@fE�@f{@f@ep�@eV@d��@d�@dz�@d9X@c�
@c��@cC�@b�H@b=q@a�#@a��@ahs@a&�@a%@`��@`Ĝ@`bN@_�@_|�@_|�@_K�@]��@]�@]�@\�/@\Z@[ƨ@["�@Z�!@Z~�@Z^5@Z-@Z�@Z�@Z�@Z�@Y�#@Y�7@Yx�@Y&�@X1'@X  @W��@V5?@U��@U@Up�@T�@T1@S��@St�@SC�@R�@RM�@Q�@Q�^@QX@P�9@P  @O�w@O�@Nȴ@N��@N$�@M�-@MO�@L�@LI�@L9X@K��@K�@KC�@J�!@J�\@J~�@J-@I�#@I��@IX@I&�@H��@H��@HbN@H �@G�@G��@Gl�@G+@F�+@F$�@E@E�-@E��@E�h@E`B@E?}@E�@EV@D�@Dz�@D1@CdZ@B�H@B��@B�!@BJ@A��@A%@@��@@r�@@  @?;d@>��@>E�@>5?@>$�@>@=��@=�h@=`B@=�@<�j@<j@;�m@;�F@;��@;�@;t�@;o@:��@:��@:-@9��@9%@8�@8r�@8Q�@7�@7��@7l�@6�y@6�+@6@5@5�@5`B@5?}@5V@4�@4��@4�D@4z�@4(�@3�F@3dZ@3"�@2�@2�H@2��@2��@2�\@2=q@1�#@1G�@0��@0�9@0r�@01'@/��@/l�@/K�@/
=@.v�@.$�@-@,��@,�D@,1@+�
@+�
@+ƨ@+��@+�@+S�@+33@+@*�H@*��@*=q@)��@)X@(�`@(Ĝ@(�9@(A�@(  @'�;@'�w@'�@'��@'�@'��@'|�@&�@&E�@&$�@%�@%@%��@%�@%�@$�/@$Z@#��@#��@#��@#��@#�@#t�@#"�@"�@"��@"M�@"�@!��@!��@!��@!hs@!G�@ ��@ Ĝ@ ��@ r�@ 1'@�;@�@|�@\)@;d@+@�@�y@�+@{@��@@�-@��@��@`B@�@��@��@j@9X@(�@�m@��@��@t�@��@��@t�@dZ@t�@�@dZ@C�@��@�\@n�@^5@M�@�@��@��@hs@��@�9@�@A�@1'@ �@b@  @  @�@�w@l�@\)@;d@��@v�@v�@v�@v�@ff@V@5?@@@�T@��@`B@/@��@j@I�@I�@1@�@dZ@S�@C�@33@"�@o@o@�@��@^5@��@��@�7@hs@hs@G�@7L@%@Ĝ@�u@�@Q�@ �@b@�@�@�;@�;@�;@�@�;@�@\)@�y@��@v�@E�@�T@@�-@��@`B@V@�/@��@j@Z@9X@(�@(�@�@�m@ƨ@��@dZ@C�@C�@C�@C�@C�@C�@C�@o@11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
<jB
<jB
=qB
>wB
>wB
=qB
=qB
?}B
@�B
@�B
>wB
@�B
P�B
[#B
e`B
q�B
�1B
�\B
�\B
�\B
�VB
�VB
�VB
�VB
�\B
�bB
�bB
�oB
�oB
�oB
�uB
�uB
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�!B
��B
��B
�TB
��B$�B<jBF�BXBffB� B��B��B�BPB	7B�B+B2-B2-B!�B�B��BÖB%BL�B]/BjB{�B�B�7B� Bp�BdZB_;BZBVBQ�BO�BI�B=qB?}B=qB=qB<jB<jB8RB49B!�B�BhB+B%B
=B
=BB�B�;B��B�
B�NB�
B�FB��B�1B~�Bo�BVB>wB"�BPB
�B
�;B
��B
ɺB
�jB
��B
��B
��B
�1B
|�B
w�B
o�B
e`B
ZB
J�B
@�B
7LB
�B
�B
%B	��B	��B	�B	�)B	��B	�LB	�B	��B	��B	��B	��B	�%B	z�B	r�B	k�B	cTB	]/B	ZB	K�B	E�B	@�B	<jB	0!B	(�B	 �B	�B	�B	oB	DB	%B	B��B��B�B�B�yB�mB�TB�BB�#B�B�B�B��B��B��B��B��B��BɺBǮBŢBǮBŢBĜB��B��B��B�wBB�}B��BĜBǮBȴBȴB��BƨBŢBB�}B�XB�3B�!B�B�-B�!B�B��B��B��B��B�bB�7B�%B�7B�=B�+B�B�B�B�B�B�%B�B�Bv�Bo�BhsBdZBcTBaHBaHBcTBcTBdZBffBffBhsBhsBjBl�Bm�Bp�Br�Br�Br�Bu�B|�B� B�B�+B�JB�VB�\B�\B�hB�uB�{B�{B�{B��B��B��B��B��B��B��B��B��B�B�B�'B�9B�FB�FB�LB�^B�qB��BÖBȴBɺB��B��B��B��B�B�B�B�#B�/B�5B�;B�;B�NB�TB�TB�`B�fB�B�B�B�B��B��B	B	
=B	
=B		7B	VB	\B	oB	�B	�B	!�B	#�B	+B	33B	=qB	=qB	<jB	=qB	A�B	@�B	?}B	>wB	=qB	A�B	D�B	I�B	G�B	F�B	F�B	H�B	P�B	VB	XB	ZB	[#B	\)B	bNB	e`B	ffB	k�B	k�B	m�B	p�B	p�B	q�B	q�B	r�B	s�B	t�B	u�B	w�B	x�B	y�B	{�B	|�B	�B	�%B	�1B	�7B	�JB	�VB	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�3B	�?B	�FB	�dB	�FB	�FB	�FB	�LB	�RB	�RB	�dB	�dB	�}B	��B	��B	��B	B	B	B	ĜB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�/B	�)B	�)B	�)B	�/B	�;B	�HB	�TB	�mB	�mB	�fB	�ZB	�`B	�`B	�fB	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
+B
1B
1B
	7B
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
\B
\B
bB
hB
hB
hB
oB
oB
oB
uB
uB
{B
{B
�B
�B
�B
�B
�B
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
"�B
#�B
#�B
#�B
$�B
$�B
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
%�B
%�B
%�B
%�B
%�B
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
)�B
)�B
)�B
+B
+B
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
.B
/B
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
0!B
0!B
0!B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
33B
33B
33B
49B
49B
49B
49B
49B
5?B
5?B
5?B
5?B
6FB
7LB
7LB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
<jB
<jB
=qB
=qB
=qB
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
B�B
B�B
B�B
B�B
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
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
H�B
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
N�B
O�B
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
T�B
T�B
T�B
VB
VB
VB
W
B
VB
W
B
W
B
W
B
XB
YB
YB
ZB
ZB
ZB
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
cTB
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
dZB
e`B
e`B
e`B
e`B
e`B
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
m�B
m�B
m�B
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
r�B
r�B
r�B
r�B
r�B
r�B
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
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
<jB
<�B
=qB
>�B
>�B
=�B
=�B
?}B
@�B
@�B
>wB
@iB
P�B
Z�B
e,B
qvB
��B
�vB
�vB
�vB
�pB
�pB
�VB
�pB
�vB
�bB
�}B
��B
��B
�oB
��B
�uB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�UB
��B
��B
�TB
�.B%FB<�BG�BY1Bg�B� B��B�"B�B�B
�B�B+�B3�B4�B&2B��B�{B�fB	7BO�Ba-Bm�B~]B��B��B��Bs�Bf�Ba�B\�BYeBU�BVBL�B>�BAoB>�B>�B>BB>BB<B:�B$�B!B�B	�B_B0B�B�B�B��B�\B�B��B�]B��B�B�=B��Bt9BYKBB'B&�BNB
��B
��B
��B
�JB
�4B
�KB
��B
�xB
��B
~(B
y�B
q�B
gRB
\�B
L�B
CB
;B
!�B
B
�B	�6B	�^B	�B	�;B	�VB	��B	�OB	�KB	�zB	�B	��B	�fB	|�B	tTB	m�B	d�B	_;B	\�B	MjB	G+B	B�B	?}B	2B	*�B	!�B	!B	�B	�B	B	�B	�B�jB��B��B��B�B�*B��B�B��B�	B�B�7B�9B�FB�B�TB�(B�0B�xB��B�zB�RBǔBżB�uB��B�AB�;B��B�OB�ABňB��B�=B�^B��B�B��B�3BB��B�B�'B� B�hB�vB�cB��B�QB�sB�B��B�^B��B��B��B�B�B�tB�B��B��B�zB��B�-Bx�Bq�BjKBfLBd�Ba�Ba�BdBdBe`Bg�Bg�BiBi_Bk�BmCBncBqABsBs3Bs�BwLB~BB� B�mB��B�jB�vB�.B�B�B�B�B��B��B�B�7B�=B��B��B��B��B��B��B��B�CB�vB��B��B��B�RB�JB�(B�B��B�B�rB͟B�}BѝB��B��B��B��B��BݲB޸BߤB��B��B�B�B��B�RB��B�B�B�B��B��B	�B	DB	�B	
�B	(B	�B	�B	#B	�B	!�B	#�B	*�B	2�B	=�B	=�B	<�B	=�B	B�B	A�B	@�B	?�B	=�B	A�B	ESB	J�B	H1B	F�B	GB	IRB	Q4B	V9B	XEB	ZkB	[�B	\�B	b�B	e�B	f�B	k�B	k�B	m�B	p�B	p�B	rB	rB	sMB	t9B	u%B	v+B	x8B	y$B	zDB	|�B	}qB	�uB	�tB	��B	��B	��B	��B	�vB	�NB	�{B	��B	��B	�/B	�CB	�#B	�B	�	B	�B	��B	�B	�'B	�\B	�-B	�B	�B	��B	��B	�$B	�$B	�B	�_B	�0B	�B	�B	� B	�hB	��B	��B	�6B	��B	��B	�zB	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	ªB	��B	�B	��B	�6B	�B	��B	� B	�uB	өB	�@B	�@B	�FB	ևB	�?B	�YB	׍B	�_B	�QB	�QB	�eB	�kB	ݲB	�]B	�CB	�CB	�IB	�VB	�bB	�nB	��B	��B	��B	��B	�B	�`B	�LB	��B	��B	��B	��B	��B	�B	��B	�LB	�?B	��B	��B	�9B	�3B	��B	��B	�B	��B	��B	��B	��B	�B	�B	�B	��B	�B	�B	�<B	�(B	�BB	�.B	�.B
 4B
 4B
UB
;B
UB
AB
{B
gB
�B
YB
_B
_B
_B
zB
_B
fB
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
/B
�B
B
!B
!B
 B
 B
�B
!B
 �B
 �B
 �B
 �B
!B
 �B
"B
"4B
"4B
#:B
$&B
$@B
$@B
%B
%,B
%B
%B
%FB
%B
%B
%,B
%,B
&2B
&2B
&LB
&LB
&2B
&B
&2B
&B
&2B
'8B
'B
(>B
($B
($B
($B
(>B
)*B
)*B
)*B
)_B
)DB
*0B
*KB
*KB
+QB
+kB
,=B
,WB
,"B
,"B
,"B
,=B
,WB
-CB
-CB
-]B
-CB
.IB
.cB
.IB
.IB
.cB
/OB
/OB
/iB
/iB
/OB
/OB
/iB
0UB
0UB
0UB
0UB
0oB
0oB
0oB
0oB
1�B
1�B
2|B
2aB
2|B
2�B
2�B
2�B
2|B
3hB
3hB
3hB
4TB
4TB
4TB
4nB
4nB
5�B
5tB
5�B
5�B
6�B
7�B
7�B
9�B
9�B
9�B
9�B
:�B
:�B
:�B
:�B
:�B
;�B
;�B
;�B
;�B
<�B
<�B
=�B
=�B
=�B
>�B
>�B
>�B
>�B
>�B
?�B
?�B
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
B�B
B�B
B�B
B�B
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
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
HB
H�B
H�B
IB
IB
J	B
J�B
J�B
J�B
KB
J�B
J�B
J�B
K�B
LB
LB
LB
MB
MB
MB
MB
MB
MB
N"B
N"B
N"B
N<B
OBB
O�B
P.B
P.B
PB
PB
P.B
Q4B
QNB
R B
R B
R:B
R B
R B
R B
R B
R:B
R B
R:B
S@B
S&B
S&B
S@B
TB
T,B
TB
T,B
T,B
TFB
TaB
UMB
U2B
U2B
VSB
VSB
V9B
WYB
VSB
WsB
WYB
WYB
XyB
YeB
YeB
ZQB
Z7B
ZQB
ZQB
ZQB
ZQB
ZkB
ZQB
[qB
[WB
[�B
[qB
\xB
\xB
]dB
]~B
]~B
]dB
]dB
^OB
^OB
^jB
^OB
^jB
^jB
^�B
_�B
_�B
_pB
_pB
`�B
`vB
`vB
`vB
`�B
a�B
a|B
abB
abB
a|B
a|B
a|B
b�B
b�B
b�B
c�B
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
d�B
e�B
e�B
e�B
e�B
e�B
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
j�B
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
m�B
m�B
m�B
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
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
tB
s�B
tB
s�B
s�B
t�B
uB
t�B
t�B
t�B
t�B
t�B
vB
vB
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
wB
wB
wB
xB
xB
xB
xB
y	B
x�B
y	B
y$B
y	B
y	B
y	B
y	B
x�B
y	B
x�B
y�B
zB
zB
zB
zB
zB
z�B
z�B
z�B
z�B
z�B
{B
{B
{0B
z�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.23(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201701020039202017010200392020170102003920201806221307032018062213070320180622130703201804050707282018040507072820180405070728  JA  ARFMdecpA19c                                                                20161229093505  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20161229003523  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20161229003524  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20161229003524  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20161229003525  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20161229003525  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20161229003525  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20161229003525  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20161229003525  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20161229003525                      G�O�G�O�G�O�                JA  ARUP                                                                        20161229010206                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20161229153221  CV  JULD            G�O�G�O�F�)�                JM  ARCAJMQC2.0                                                                 20170101153920  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170101153920  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404220728  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622040703  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116211516                      G�O�G�O�G�O�                