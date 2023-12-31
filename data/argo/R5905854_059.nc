CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:55:08Z creation;2022-06-04T17:55:09Z conversion to V3.1      
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
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
_FillValue                 �  �P   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
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
resolution        :�o     �  �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604175508  20220610141506  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               ;A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @�8����1   @�8���P�@.Rn��O��cfffff1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B>��BHffBO33BW��B`  Bh��BnffBx  B�  B�  B�  B�33B���B�33B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�  B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�C33C  C	�fC  C�fC  C�fC  C  C  C  C  C  C   C"  C$  C&  C(�C*�C,L�C-��C0  C2  C4  C6  C7�fC:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ�C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DEfDE� DF  DF� DG  DGy�DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DSfDS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@z�@qG�@���@���AQ�A<Q�A\Q�A|Q�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B{B{B{B{B'{B/{B7{B=�HBGz�BNG�BV�B_{Bg�HBmz�Bw{B{B��=B��=B��pB�#�B��pB��=B��=B��=B��=B��=B��=B��=B��=B��B��=B��=BÊ=BǊ=Bˊ=Bϊ=BӽpB׊=B�W
Bߊ=B�=B�=B�=B�=B�=B��=B��=B��=C�C޹C�RC�C	��C�C��C�C��C�C�C�C�C�C�C�C!�C#�C%�C'޹C)޹C,�C-��C/�C1�C3�C5�C7��C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY޹C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��\C��C��C��C��C��C��C��C��C��D qHD �HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HD	qHD	�HD
qHD
�HDqHD�HDqHD�HDw�D�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HD qHD �HD!qHD!�HD"qHD"�HD#qHD#�HD$qHD$�HD%qHD%�HD&qHD&�HD'qHD'�HD(qHD(�HD)qHD)�HD*qHD*�HD+qHD+�HD,qHD,�HD-qHD-�HD.qHD.�HD/qHD/�HD0qHD0�HD1qHD1�HD2qHD2�HD3qHD3�HD4qHD4�HD5qHD5�HD6qHD6�HD7qHD7�HD8qHD8�HD9qHD9�HD:qHD:�HD;qHD;�HD<qHD<�HD=qHD=�HD>qHD>�HD?qHD?�HD@qHD@�HDAqHDA�HDBqHDB�HDCqHDC�HDDqHDD��DEqHDE�HDFqHDF�HDGj�DG�HDHqHDH�HDIqHDI�HDJqHDJ�HDKqHDK�HDLqHDL�HDMqHDM�HDNqHDN�HDOqHDO�HDPqHDP�HDQqHDQ�HDRqHDR��DSqHDS�HDTqHDT�HDUqHDU�HDVqHDV�HDWqHDW�HDXqHDX�HDYqHDY�HDZqHDZ�HD[qHD[�HD\qHD\�HD]qHD]�HD^qHD^�HD_qHD_�HD`qHD`�HDaqHDa�HDbqHDb�HDcqHDc�HDdqHDd�HDeqHDe�HDfqHDf�HDgqHDg�HDhqHDh�HDiqHDi�HDjqHDj�HDkqHDk�HDlqHDl�HDmqHDm�HDnqHDn�HDoqHDo�HDpqHDp�HDqqHDq�HDrqHDr�HDsqHDs�HDtqHDt�HDuqHDu�HDvqHDv�HDwqHDw�HDxqHDx�HDyqHDy�HDzqHDz�HD{qHD{�HD|qHD|�HD}qHD}�HD~qHD~�HDqHD�HD�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D��qD�8�D�x�D���D���D�8�D�x�D���D��qD�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D¸�D���D�8�D�x�Dø�D���D�8�D�x�Dĸ�D���D�8�D�x�DŸ�D���D�8�D�x�DƸ�D���D�8�D�x�DǸ�D���D�8�D�x�Dȸ�D���D�8�D�x�Dɸ�D���D�8�D�x�Dʸ�D���D�8�D�x�D˸�D���D�8�D�x�D̸�D���D�8�D�x�D͸�D���D�8�D�x�Dθ�D���D�8�D�x�Dϸ�D���D�8�D�x�Dи�D���D�8�D�x�DѸ�D���D�8�D�x�DҸ�D���D�8�D�x�DӸ�D���D�8�D�x�DԸ�D���D�8�D�x�Dո�D���D�8�D�x�Dָ�D���D�8�D�x�D׸�D���D�8�D�x�Dظ�D���D�8�D�x�Dٸ�D���D�8�D�x�Dڸ�D���D�8�D�x�D۸�D���D�8�D�x�Dܸ�D���D�8�D�x�Dݸ�D���D�8�D�x�D޸�D���D�8�D�x�D߸�D���D�8�D�x�DฤD���D�8�D�x�DḤD���D�8�D�x�D⸤D���D�8�D�x�D㸤D���D�8�D�x�D两D���D�8�D�x�D帤D���D�8�D�x�D渤D���D�8�D�x�D縤D���D�8�D�x�D踤D���D�8�D�x�D鸤D���D�8�D�x�D긤D���D�8�D�x�D븤D���D�8�D�x�D츤D���D�8�D�x�D���D���D�8�D�x�DD���D�8�D�x�D︤D���D�8�D�x�D�D���D�8�D�x�D�D���D�8�D�x�D�D���D�8�D�x�D�D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D��>111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�רA���A��A���Aۼ�AۼjA۾Aۼ�AۼjAۼA۲aA۰!A۱'A۱�A۰�Aۙ�Aۑ Aۉ7Aۃ�A�sMA�Q�A� �Aڲ�A�$�Aؐ�A�Q�A�'A���A��A��	A�1�A�%FA�MjA�A�A�K�A�^�A�K�A�AUA�/A�I�A���A�ZQA�t�A���A��A�v�A�MA��NA��eA�A�A�T�A���A��KA�C�A���A��cA�y�A���A�՛A��YA���A��KA�J�A�hA���A��A�)*A�K�A�OA���A�)_A���A�U2A�A���A�[�A��A���A���A��?A��iA��UA�F�A���A�o5A{�Av��Ao�?Al�9Ahl"Ad�Aa<�AZ�"AV�
AU��AR.IAL�\AG��AE��AB~�A@9XA=�A<bNA9��A8|�A6�A66A6_A5}�A3�AA1֡A0�KA/4A-�~A-%FA.��A/ iA-��A*��A(�A'4A&�LA%+A$��A$ �A"bNA!�fA!�zA �AeA%FAy�A�zA$Aj�A��A��A�A�KA�FAkQA��A�HAA�pA��A�uA7A
�A	�6A	��A
p;Ap;A�A
n/A
OA

=A	�BA	S&A
}�A
��A
QA�xA	<6A	��A	S&A��A|�A	�A
_A
O�A	��A	o�A	�AMA�dAa�A+kA�A	A��A��AYA�A��A�1A?�A��A��AIRA��A�A�7A*0A �A �LA t�@���@��`@��O@��@��@��@�*0@���@�Mj@��v@�ѷ@���@�'R@��@�K^@���@��@�xl@�q@��@�&�@�7�@�@��@�@�W�@��@���@�8�@��@�&�@�@��@���@��@��g@��@�c�@��@�@�#:@�U�@��@�I@�g8@�@��@�"�@�rG@�\�@���@�k�@�5�@޻�@�B�@��@��@�?@��@�ݘ@�X@֬@Ցh@�+@Ԉ�@�-@ӣn@�(�@�;�@�@ђ:@�C@���@�,=@ρ�@�W?@�Y@�+�@�Z�@͹�@�[W@��@˯�@���@�|�@�J@��.@Ɍ~@�Mj@Ɋ�@�1�@�Ɇ@��@��Q@��@�Ft@�!�@���@şV@�Y�@��@Į}@�Ta@�@é�@�L�@��@½<@��&@�T�@�֡@�j@���@�#�@��z@�j@�1'@���@���@�}�@�J�@���@��@���@��@�Mj@�*0@���@�C-@���@���@�x�@�+@���@�c @���@��f@�[W@�*0@��@��@���@���@�)�@��K@�F�@���@��|@��B@�+k@�  @�G@��@���@��@�7�@��@��>@�{J@��P@�xl@�+k@��@�ԕ@�X@��c@�~(@�@�@��j@�4�@��'@�b@��K@��:@�m]@�+@���@�3�@�ԕ@�V@���@���@�v�@�4n@�O@��>@���@�C@��.@�Ft@��@���@�[W@��@���@���@�R�@�,=@��6@�Q�@�@�ߤ@��9@��o@�_�@�'R@���@�t�@�.I@���@��F@�_�@�M@�$�@��q@�c@�s�@��@��!@��e@��@��o@�kQ@�L0@�~@��3@���@�G�@��@���@�Z�@�:�@�7�@�)�@��9@��'@�o�@���@���@�<�@�1@��@��~@�;d@���@�y>@��@���@��@���@�~@��@��[@�A�@� i@��]@��@�bN@��@���@���@�A�@��@�Ĝ@�D�@��@��k@�2a@�҉@���@�(�@�|@�:�@�;@���@���@�u%@�V�@�2�@�@���@�@�]�@��@��E@��.@�ff@�-@��W@��q@���@�|@�L�@��@���@�7�@�_@��j@��@���@���@�a@���@��z@�a|@�u@�|@�,�@��@���@���@��R@���@�6�@��@��@���@��t@���@���@���@�hs@�33@��8@���@�~�@�+k@��H@���@�@O@��@��|@�Ɇ@�j@�$�@��r@��K@�P�@��	@��X@��1@�M�@�V�@��@~V@}^�@}�@|�`@|��@{��@{]�@{�@z��@zTa@y�@y��@y��@x��@xV�@w��@w�@w]�@v�1@u�)@uw2@tm�@t�@s��@r��@rYK@r
�@q�Z@q��@qA @pl"@pG@o��@n�H@n��@n�@m��@mf�@lی@lz�@lK^@k'�@j�@j{@i�@i��@i7L@i�@i�@h�z@h<�@g�@gE9@g@f�}@f$�@ew2@d�@d��@d`�@d*�@cj�@b��@b�+@b$�@a��@a��@aT�@`�4@`H@_�+@_�K@_��@_y�@_�@^n�@]�N@]A @\�@\r�@\�@\�@\c�@[��@[��@[��@[C�@Z�y@Zff@Y�@Yx�@Ya�@YV@X��@X��@XPH@W��@WU�@V�y@V��@V^5@V@�@U�j@U�M@UVm@T�@T �@S��@S=@So@R�"@R�@R��@R��@Rq�@Q��@Q��@Qw2@P�f@P�@Pm�@O��@N��@N��@Nq�@N�@M��@Mp�@M*0@L��@Lj@L'R@K��@Kn/@K i@J�x@JkQ@I��@Im]@IG�@H��@Hh�@HA�@G�}@G.I@G
=@Fߤ@F��@F=q@E�D@E�j@E�@Eu�@D��@Dr�@DV�@D�@CP�@B�X@B��@B5?@A��@Aa�@@�P@@��@@H@?�K@?o�@?!-@>�y@>��@>a|@>B[@>&�@>	@=��@=zx@= \@<��@<r�@<PH@<?�@<"h@;��@;RT@;@:�6@:u@9��@9e,@9 \@8�	@8Ɇ@8�@8S�@8>B@8-�@7��@7'�@6��@6Ov@6�@5��@5rG@5B�@5@4�|@4�?@4�I@49X@3خ@3H�@2�<@2?@2�@1�o@1�@1��@1�7@1c�@12a@0��@0�/@0�@0�D@0e�@/�Q@/�@@/dZ@/(@.�@.��@.@�@-�@-�X@-<6@,��@,c�@,	�@+�}@+�{@+S�@*��@*$�@)��@)�^@)x�@)2a@)%@(ی@(�D@(9X@'�;@'�f@'s@'A�@&��@&�b@&��@&J�@& �@%�3@%��@%hs@%S&@$�	@$I�@#�@#��@#_p@#o@"ߤ@"��@"h
@"8�@"4@!�>@!�@!hs@!V@ c�@ 2�@ @�@��@�k@>�@�@�@�@�6@W�@8�@�@�@��@�p@�9@��@oi@4n@�r@خ@�@RT@�@��@�A@W�@0U@��@��@}�@^�@IR@*0@�@Ɇ@�@tT@�@Mj@@�2@�@��@��@H�@��@ϫ@�N@��@O�@(�@�@+@�	@�@U2@G@�&@�@@|�@b�@'�@�@�y@��@ff@C�@)�@�@�M@hs@F@8�@�@��@��@l"@?�@1@�K@�@iD@P�@'�@�@�X@�F@M�@@��@ԕ@�H@��@�M@?}@%F@V@�@�)@��@`�@A�@"h@�@�@��@]�@)_@�@
�@
��@
�@
Q@
=q@
e@	��@	��@	�N@	�n@	e,@	G�@	=�@	=�@	2a@	+@�f@�@�O@��@�z@��@�@I�@�@�w@��@��@��@iD@=@$t@��@�@ں@�X@��@��@Z�@+k@_@��@��@��@x�@m]@\�@L�@:�@�@��@ѷ@Ĝ@��@�D@j@Xy@1'@��@�@�$@\)@�@@��@�m@�1@s�@Z�@8�@@@��@�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�רA���A��A���Aۼ�AۼjA۾Aۼ�AۼjAۼA۲aA۰!A۱'A۱�A۰�Aۙ�Aۑ Aۉ7Aۃ�A�sMA�Q�A� �Aڲ�A�$�Aؐ�A�Q�A�'A���A��A��	A�1�A�%FA�MjA�A�A�K�A�^�A�K�A�AUA�/A�I�A���A�ZQA�t�A���A��A�v�A�MA��NA��eA�A�A�T�A���A��KA�C�A���A��cA�y�A���A�՛A��YA���A��KA�J�A�hA���A��A�)*A�K�A�OA���A�)_A���A�U2A�A���A�[�A��A���A���A��?A��iA��UA�F�A���A�o5A{�Av��Ao�?Al�9Ahl"Ad�Aa<�AZ�"AV�
AU��AR.IAL�\AG��AE��AB~�A@9XA=�A<bNA9��A8|�A6�A66A6_A5}�A3�AA1֡A0�KA/4A-�~A-%FA.��A/ iA-��A*��A(�A'4A&�LA%+A$��A$ �A"bNA!�fA!�zA �AeA%FAy�A�zA$Aj�A��A��A�A�KA�FAkQA��A�HAA�pA��A�uA7A
�A	�6A	��A
p;Ap;A�A
n/A
OA

=A	�BA	S&A
}�A
��A
QA�xA	<6A	��A	S&A��A|�A	�A
_A
O�A	��A	o�A	�AMA�dAa�A+kA�A	A��A��AYA�A��A�1A?�A��A��AIRA��A�A�7A*0A �A �LA t�@���@��`@��O@��@��@��@�*0@���@�Mj@��v@�ѷ@���@�'R@��@�K^@���@��@�xl@�q@��@�&�@�7�@�@��@�@�W�@��@���@�8�@��@�&�@�@��@���@��@��g@��@�c�@��@�@�#:@�U�@��@�I@�g8@�@��@�"�@�rG@�\�@���@�k�@�5�@޻�@�B�@��@��@�?@��@�ݘ@�X@֬@Ցh@�+@Ԉ�@�-@ӣn@�(�@�;�@�@ђ:@�C@���@�,=@ρ�@�W?@�Y@�+�@�Z�@͹�@�[W@��@˯�@���@�|�@�J@��.@Ɍ~@�Mj@Ɋ�@�1�@�Ɇ@��@��Q@��@�Ft@�!�@���@şV@�Y�@��@Į}@�Ta@�@é�@�L�@��@½<@��&@�T�@�֡@�j@���@�#�@��z@�j@�1'@���@���@�}�@�J�@���@��@���@��@�Mj@�*0@���@�C-@���@���@�x�@�+@���@�c @���@��f@�[W@�*0@��@��@���@���@�)�@��K@�F�@���@��|@��B@�+k@�  @�G@��@���@��@�7�@��@��>@�{J@��P@�xl@�+k@��@�ԕ@�X@��c@�~(@�@�@��j@�4�@��'@�b@��K@��:@�m]@�+@���@�3�@�ԕ@�V@���@���@�v�@�4n@�O@��>@���@�C@��.@�Ft@��@���@�[W@��@���@���@�R�@�,=@��6@�Q�@�@�ߤ@��9@��o@�_�@�'R@���@�t�@�.I@���@��F@�_�@�M@�$�@��q@�c@�s�@��@��!@��e@��@��o@�kQ@�L0@�~@��3@���@�G�@��@���@�Z�@�:�@�7�@�)�@��9@��'@�o�@���@���@�<�@�1@��@��~@�;d@���@�y>@��@���@��@���@�~@��@��[@�A�@� i@��]@��@�bN@��@���@���@�A�@��@�Ĝ@�D�@��@��k@�2a@�҉@���@�(�@�|@�:�@�;@���@���@�u%@�V�@�2�@�@���@�@�]�@��@��E@��.@�ff@�-@��W@��q@���@�|@�L�@��@���@�7�@�_@��j@��@���@���@�a@���@��z@�a|@�u@�|@�,�@��@���@���@��R@���@�6�@��@��@���@��t@���@���@���@�hs@�33@��8@���@�~�@�+k@��H@���@�@O@��@��|@�Ɇ@�j@�$�@��r@��K@�P�@��	@��X@��1@�M�@�V�@��@~V@}^�@}�@|�`@|��@{��@{]�@{�@z��@zTa@y�@y��@y��@x��@xV�@w��@w�@w]�@v�1@u�)@uw2@tm�@t�@s��@r��@rYK@r
�@q�Z@q��@qA @pl"@pG@o��@n�H@n��@n�@m��@mf�@lی@lz�@lK^@k'�@j�@j{@i�@i��@i7L@i�@i�@h�z@h<�@g�@gE9@g@f�}@f$�@ew2@d�@d��@d`�@d*�@cj�@b��@b�+@b$�@a��@a��@aT�@`�4@`H@_�+@_�K@_��@_y�@_�@^n�@]�N@]A @\�@\r�@\�@\�@\c�@[��@[��@[��@[C�@Z�y@Zff@Y�@Yx�@Ya�@YV@X��@X��@XPH@W��@WU�@V�y@V��@V^5@V@�@U�j@U�M@UVm@T�@T �@S��@S=@So@R�"@R�@R��@R��@Rq�@Q��@Q��@Qw2@P�f@P�@Pm�@O��@N��@N��@Nq�@N�@M��@Mp�@M*0@L��@Lj@L'R@K��@Kn/@K i@J�x@JkQ@I��@Im]@IG�@H��@Hh�@HA�@G�}@G.I@G
=@Fߤ@F��@F=q@E�D@E�j@E�@Eu�@D��@Dr�@DV�@D�@CP�@B�X@B��@B5?@A��@Aa�@@�P@@��@@H@?�K@?o�@?!-@>�y@>��@>a|@>B[@>&�@>	@=��@=zx@= \@<��@<r�@<PH@<?�@<"h@;��@;RT@;@:�6@:u@9��@9e,@9 \@8�	@8Ɇ@8�@8S�@8>B@8-�@7��@7'�@6��@6Ov@6�@5��@5rG@5B�@5@4�|@4�?@4�I@49X@3خ@3H�@2�<@2?@2�@1�o@1�@1��@1�7@1c�@12a@0��@0�/@0�@0�D@0e�@/�Q@/�@@/dZ@/(@.�@.��@.@�@-�@-�X@-<6@,��@,c�@,	�@+�}@+�{@+S�@*��@*$�@)��@)�^@)x�@)2a@)%@(ی@(�D@(9X@'�;@'�f@'s@'A�@&��@&�b@&��@&J�@& �@%�3@%��@%hs@%S&@$�	@$I�@#�@#��@#_p@#o@"ߤ@"��@"h
@"8�@"4@!�>@!�@!hs@!V@ c�@ 2�@ @�@��@�k@>�@�@�@�@�6@W�@8�@�@�@��@�p@�9@��@oi@4n@�r@خ@�@RT@�@��@�A@W�@0U@��@��@}�@^�@IR@*0@�@Ɇ@�@tT@�@Mj@@�2@�@��@��@H�@��@ϫ@�N@��@O�@(�@�@+@�	@�@U2@G@�&@�@@|�@b�@'�@�@�y@��@ff@C�@)�@�@�M@hs@F@8�@�@��@��@l"@?�@1@�K@�@iD@P�@'�@�@�X@�F@M�@@��@ԕ@�H@��@�M@?}@%F@V@�@�)@��@`�@A�@"h@�@�@��@]�@)_@�@
�@
��@
�@
Q@
=q@
e@	��@	��@	�N@	�n@	e,@	G�@	=�@	=�@	2a@	+@�f@�@�O@��@�z@��@�@I�@�@�w@��@��@��@iD@=@$t@��@�@ں@�X@��@��@Z�@+k@_@��@��@��@x�@m]@\�@L�@:�@�@��@ѷ@Ĝ@��@�D@j@Xy@1'@��@�@�$@\)@�@@��@�m@�1@s�@Z�@8�@@@��@�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B5?B4�B5%B4�B4�B4�B4�B4�B4�B4�B4�B4�B4�B4�B4�B5%B5tB6+B6�B:�BD�BdtB��B	L�B	�"B	�AB	��B	��B	��B	�BB	��B	�0B	�kB	��B	خB	ݘB	�WB
j�B
�+B
�B
�4B
��B
��B
�B.}BNBm]BrBs�Bt�Bo�BoiBB��B��B��B�kB��B��B�B��B�sB��B�dB~�Bp�B~]By�B]~BE9B4nB 'ByB
��B
�jB
��B
��B
��B
��B
}"B
j�B
P�B
$�B	��B	�0B	��B	��B	`�B	J�B	3�B	 BB	�B	�B�RB�MB�yB��B�B��B��B�xB��B��B��B�'B�B�hB�vB�B��B	�B	�B�*B�B�5B	�B	<6B	=�B	.}B	=B	�B	B	�B	[B	�B	$B	-B	/�B	/�B	*KB	4�B	HfB	2|B	 B	�B��B�HB�3B�eB�B��B�/B�jB�:B�&B��B�fB�B�hB��B		�B	�B	4nB	BuB	:B	:�B	D�B	S@B	SuB	j�B	s�B	p;B	e�B	tB	HB	��B	��B	�oB	��B	�,B	��B	��B	��B	�CB	��B	��B	�eB	��B	�9B	��B	�DB	��B	�aB	��B	��B	�B	�RB	��B	��B	��B	��B	�2B	�+B	��B	��B	��B	��B	�xB	��B	�<B	�qB	��B	�xB	�jB	��B	��B	�OB	��B	��B	�oB	�iB	��B	�BB	��B	��B	��B	�B	�%B	�_B	�zB	�%B	ÖB	��B	��B	āB	��B	�?B	�B	��B	˒B	��B	�fB	�B	�aB	�{B	�uB	ňB	�B	�\B	��B	ЗB	��B	�MB	�{B	�bB	��B	�=B	�6B	�6B	�6B	бB	�B	ĜB	��B	�iB	��B	�=B	˒B	�JB	�0B	�^B	�^B	�)B	�B	�B	��B	ѝB	ѷB	��B	�@B	�{B	ӏB	�,B	յB	ٚB	�B	��B	�	B	ڠB	�sB	�{B	��B	�2B	�YB	��B	��B	�B	��B	��B	ߊB	�B	޸B	�HB	�B	�ZB	�B	�B	��B	�B	�B	�&B	�&B	�B	�B	�B	�B	��B	��B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�"B	�qB	�B	�)B	��B	��B	�B	�B	�B	�B	�B	�B	��B	�[B	�B	�'B	�'B	�GB	�B	�nB	�ZB	��B	�B	��B	�hB	�B	�B	��B	��B	��B	��B	�+B	��B	��B	��B	��B	�B	��B	��B	�zB	�fB	�B	��B	��B	�lB	��B	�>B	�XB	�rB	�xB	��B	�B	��B	��B	��B	��B	�6B	��B	��B	�B	�B	�B	�B	�(B	��B	�]B	��B	��B
 �B
 �B
 B
B
 B
�B
�B
�B
B
B
[B
�B
�B
uB
uB
�B
B
{B
�B
�B
�B
�B
�B
gB
gB
�B
�B
mB
SB
tB
?B
YB
tB
�B
YB
tB
?B
�B
�B
+B
EB
�B
B
B
B
�B
�B
�B
fB
	B

XB
xB
�B
�B
~B
B
�B
jB
�B
pB
�B
B
}B
�B
�B
B
NB
hB
�B
�B
:B
:B
�B
�B
B
B
�B
B
�B
aB
�B
�B
B
gB
�B
yB
�B
�B
�B
B
WB
�B
�B
B
�B
�B
/B
�B
�B
B
�B
!B
;B
;B
�B
 'B
 BB
�B
 B
 'B
 'B
 B
�B
�B
�B
 B
 \B
 �B
!�B
!�B
!�B
#:B
#�B
#�B
$B
$tB
%B
%�B
%�B
%�B
%�B
%�B
&B
&LB
&�B
'B
'B
'8B
'�B
(>B
'�B
($B
(XB
(�B
(�B
)DB
)_B
)DB
(�B
(XB
'�B
'B
'mB
(XB
)�B
)�B
(�B
)B
)_B
*�B
+6B
+kB
+�B
+�B
+�B
+�B
+�B
,B
,B
,=B
,qB
,qB
,qB
,qB
-)B
-�B
./B
/B
/5B
/�B
0;B
0UB
0�B
0�B
0�B
1AB
1�B
1�B
2-B
2�B
33B
3�B
3�B
3�B
3�B
3�B
4TB
6B
6zB
72B
7LB
7�B
7�B
7�B
8B
88B
8�B
8�B
9>B
9>B
9	B
9	B
9XB
:*B
:B
:*B
:^B
:DB
:^B
:�B
;dB
;�B
<�B
=VB
=<B
=VB
=�B
=�B
=�B
=�B
=�B
=�B
=�B
>BB
>BB
>�B
@4B
@�B
@�B
A B
BB
CB
CB
C�B
CGB
B�B
B[B
C-B
C�B
C�B
C�B
C�B
DgB
ESB
E�B
E�B
E�B
E�B
F%B
FYB
F%B
FtB
F�B
F�B
GB
G+B
G�B
HKB
HKB
HKB
HKB
H�B
H�B
IB
IB
IB
IlB
I�B
JXB
K)B
KxB
K�B
LdB
L�B
L�B
MB
MB
MPB
M�B
M�B
NB
NB
NB
N�B
N�B
OB
O�B
O�B
O�B
O�B
PB
P.B
PHB
PbB
P�B
P�B
P�B
P�B
Q B
Q�B
Q�B
Q�B
Q�B
R B
R�B
R�B
R�B
R�B
R�B
S@B
S�B
S�B
S�B
TB
T,B
TFB
TaB
T�B
T�B
T�B
T�B
U2B
UMB
U�B
U�B
U�B
U�B
U�B
U�B
VB
V�B
V�B
V�B
WsB
W�B
XyB
X�B
Y�B
ZQB
Z�B
Z�B
Z�B
Z�B
[WB
[qB
[�B
\B
\)B
\�B
\�B
\�B
\�B
\�B
]/B
]B
]~B
]�B
^5B
^�B
^�B
_B
^�B
^�B
_!B
_VB
_pB
_�B
_�B
_�B
_�B
_�B
_�B
`vB
`vB
`�B
`�B
`�B
a-B
a|B
a�B
a�B
bNB
b�B
cB
cTB
cTB
c�B
c�B
c�B
d@B
dtB
d�B
d�B
e,B
e,B
e,B
e�B
e�B
f2B
f�B
f�B
f�B
f�B
gB
gB
gRB
g�B
g�B
g�B
h
B
g�B
h$B
hsB
iB
i_B
i�B
i�B
i�B
jB
jKB
jB
j�B
j�B
j�B
kB
kQB
lB
l"B
lWB
lqB
lqB
l�B
mB
mB
m)B
m)B
m]B
m�B
m�B
m�B
nIB
n�B
n�B
o B
oB
o5B
oiB
o�B
o�B
o�B
pB
p;B
poB
p�B
p�B
p�B
q'B
q[B
qvB
q�B
q�B
q�B
q�B
q�B
rB
q�B
raB
r�B
sMB
sMB
s�B
shB
shB
s�B
tB
tB
s�B
tTB
t�B
t�B
t�B
t�B
t�B
u?B
u�B
u�B
vB
v`B
vFB
vzB
v�B
v�B
v�B
w2B
wfB
w�B
w�B
w�B
x8B
x8B
xlB
xlB
x�B
x�B
x�B
y	B
y$B
y$B
yXB
y�B
y�B
y�B
zB
zDB
zxB
z�B
z�B
{JB
{JB
{B
{B
{B
{�B
|B
|B
|B
|PB
|jB
|�B
|�B
|�B
}B
}"B
}B
}VB
}�B
}�B
~(B
~]B
~]B
~wB
~�B
~�B
~�B
~�B
B
B
.B
HB
�B
�B
�B
�B
�B
�B
�iB
��B
��B
��B
��B
��B
�B
�;B
�oB
��B
��B
��B
��B
�'B
�AB
�uB
��B
��B
��B
��B
��B
�B
�aB
�aB
��B
��B
��B
�B
�B
�3B
�MB
�MB
��B
��B
��B
��B
��B
��B
�B
�B
�9B
�mB
��B
��B
�?B
�tB
�tB
��B
��B
�B
��B
�B
�_B
��B
��B
��B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B5?B4�B5%B4�B4�B4�B4�B4�B4�B4�B4�B4�B4�B4�B4�B5%B5tB6+B6�B:�BD�BdtB��B	L�B	�"B	�AB	��B	��B	��B	�BB	��B	�0B	�kB	��B	خB	ݘB	�WB
j�B
�+B
�B
�4B
��B
��B
�B.}BNBm]BrBs�Bt�Bo�BoiBB��B��B��B�kB��B��B�B��B�sB��B�dB~�Bp�B~]By�B]~BE9B4nB 'ByB
��B
�jB
��B
��B
��B
��B
}"B
j�B
P�B
$�B	��B	�0B	��B	��B	`�B	J�B	3�B	 BB	�B	�B�RB�MB�yB��B�B��B��B�xB��B��B��B�'B�B�hB�vB�B��B	�B	�B�*B�B�5B	�B	<6B	=�B	.}B	=B	�B	B	�B	[B	�B	$B	-B	/�B	/�B	*KB	4�B	HfB	2|B	 B	�B��B�HB�3B�eB�B��B�/B�jB�:B�&B��B�fB�B�hB��B		�B	�B	4nB	BuB	:B	:�B	D�B	S@B	SuB	j�B	s�B	p;B	e�B	tB	HB	��B	��B	�oB	��B	�,B	��B	��B	��B	�CB	��B	��B	�eB	��B	�9B	��B	�DB	��B	�aB	��B	��B	�B	�RB	��B	��B	��B	��B	�2B	�+B	��B	��B	��B	��B	�xB	��B	�<B	�qB	��B	�xB	�jB	��B	��B	�OB	��B	��B	�oB	�iB	��B	�BB	��B	��B	��B	�B	�%B	�_B	�zB	�%B	ÖB	��B	��B	āB	��B	�?B	�B	��B	˒B	��B	�fB	�B	�aB	�{B	�uB	ňB	�B	�\B	��B	ЗB	��B	�MB	�{B	�bB	��B	�=B	�6B	�6B	�6B	бB	�B	ĜB	��B	�iB	��B	�=B	˒B	�JB	�0B	�^B	�^B	�)B	�B	�B	��B	ѝB	ѷB	��B	�@B	�{B	ӏB	�,B	յB	ٚB	�B	��B	�	B	ڠB	�sB	�{B	��B	�2B	�YB	��B	��B	�B	��B	��B	ߊB	�B	޸B	�HB	�B	�ZB	�B	�B	��B	�B	�B	�&B	�&B	�B	�B	�B	�B	��B	��B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�"B	�qB	�B	�)B	��B	��B	�B	�B	�B	�B	�B	�B	��B	�[B	�B	�'B	�'B	�GB	�B	�nB	�ZB	��B	�B	��B	�hB	�B	�B	��B	��B	��B	��B	�+B	��B	��B	��B	��B	�B	��B	��B	�zB	�fB	�B	��B	��B	�lB	��B	�>B	�XB	�rB	�xB	��B	�B	��B	��B	��B	��B	�6B	��B	��B	�B	�B	�B	�B	�(B	��B	�]B	��B	��B
 �B
 �B
 B
B
 B
�B
�B
�B
B
B
[B
�B
�B
uB
uB
�B
B
{B
�B
�B
�B
�B
�B
gB
gB
�B
�B
mB
SB
tB
?B
YB
tB
�B
YB
tB
?B
�B
�B
+B
EB
�B
B
B
B
�B
�B
�B
fB
	B

XB
xB
�B
�B
~B
B
�B
jB
�B
pB
�B
B
}B
�B
�B
B
NB
hB
�B
�B
:B
:B
�B
�B
B
B
�B
B
�B
aB
�B
�B
B
gB
�B
yB
�B
�B
�B
B
WB
�B
�B
B
�B
�B
/B
�B
�B
B
�B
!B
;B
;B
�B
 'B
 BB
�B
 B
 'B
 'B
 B
�B
�B
�B
 B
 \B
 �B
!�B
!�B
!�B
#:B
#�B
#�B
$B
$tB
%B
%�B
%�B
%�B
%�B
%�B
&B
&LB
&�B
'B
'B
'8B
'�B
(>B
'�B
($B
(XB
(�B
(�B
)DB
)_B
)DB
(�B
(XB
'�B
'B
'mB
(XB
)�B
)�B
(�B
)B
)_B
*�B
+6B
+kB
+�B
+�B
+�B
+�B
+�B
,B
,B
,=B
,qB
,qB
,qB
,qB
-)B
-�B
./B
/B
/5B
/�B
0;B
0UB
0�B
0�B
0�B
1AB
1�B
1�B
2-B
2�B
33B
3�B
3�B
3�B
3�B
3�B
4TB
6B
6zB
72B
7LB
7�B
7�B
7�B
8B
88B
8�B
8�B
9>B
9>B
9	B
9	B
9XB
:*B
:B
:*B
:^B
:DB
:^B
:�B
;dB
;�B
<�B
=VB
=<B
=VB
=�B
=�B
=�B
=�B
=�B
=�B
=�B
>BB
>BB
>�B
@4B
@�B
@�B
A B
BB
CB
CB
C�B
CGB
B�B
B[B
C-B
C�B
C�B
C�B
C�B
DgB
ESB
E�B
E�B
E�B
E�B
F%B
FYB
F%B
FtB
F�B
F�B
GB
G+B
G�B
HKB
HKB
HKB
HKB
H�B
H�B
IB
IB
IB
IlB
I�B
JXB
K)B
KxB
K�B
LdB
L�B
L�B
MB
MB
MPB
M�B
M�B
NB
NB
NB
N�B
N�B
OB
O�B
O�B
O�B
O�B
PB
P.B
PHB
PbB
P�B
P�B
P�B
P�B
Q B
Q�B
Q�B
Q�B
Q�B
R B
R�B
R�B
R�B
R�B
R�B
S@B
S�B
S�B
S�B
TB
T,B
TFB
TaB
T�B
T�B
T�B
T�B
U2B
UMB
U�B
U�B
U�B
U�B
U�B
U�B
VB
V�B
V�B
V�B
WsB
W�B
XyB
X�B
Y�B
ZQB
Z�B
Z�B
Z�B
Z�B
[WB
[qB
[�B
\B
\)B
\�B
\�B
\�B
\�B
\�B
]/B
]B
]~B
]�B
^5B
^�B
^�B
_B
^�B
^�B
_!B
_VB
_pB
_�B
_�B
_�B
_�B
_�B
_�B
`vB
`vB
`�B
`�B
`�B
a-B
a|B
a�B
a�B
bNB
b�B
cB
cTB
cTB
c�B
c�B
c�B
d@B
dtB
d�B
d�B
e,B
e,B
e,B
e�B
e�B
f2B
f�B
f�B
f�B
f�B
gB
gB
gRB
g�B
g�B
g�B
h
B
g�B
h$B
hsB
iB
i_B
i�B
i�B
i�B
jB
jKB
jB
j�B
j�B
j�B
kB
kQB
lB
l"B
lWB
lqB
lqB
l�B
mB
mB
m)B
m)B
m]B
m�B
m�B
m�B
nIB
n�B
n�B
o B
oB
o5B
oiB
o�B
o�B
o�B
pB
p;B
poB
p�B
p�B
p�B
q'B
q[B
qvB
q�B
q�B
q�B
q�B
q�B
rB
q�B
raB
r�B
sMB
sMB
s�B
shB
shB
s�B
tB
tB
s�B
tTB
t�B
t�B
t�B
t�B
t�B
u?B
u�B
u�B
vB
v`B
vFB
vzB
v�B
v�B
v�B
w2B
wfB
w�B
w�B
w�B
x8B
x8B
xlB
xlB
x�B
x�B
x�B
y	B
y$B
y$B
yXB
y�B
y�B
y�B
zB
zDB
zxB
z�B
z�B
{JB
{JB
{B
{B
{B
{�B
|B
|B
|B
|PB
|jB
|�B
|�B
|�B
}B
}"B
}B
}VB
}�B
}�B
~(B
~]B
~]B
~wB
~�B
~�B
~�B
~�B
B
B
.B
HB
�B
�B
�B
�B
�B
�B
�iB
��B
��B
��B
��B
��B
�B
�;B
�oB
��B
��B
��B
��B
�'B
�AB
�uB
��B
��B
��B
��B
��B
�B
�aB
�aB
��B
��B
��B
�B
�B
�3B
�MB
�MB
��B
��B
��B
��B
��B
��B
�B
�B
�9B
�mB
��B
��B
�?B
�tB
�tB
��B
��B
�B
��B
�B
�_B
��B
��B
��B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104959  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604175508  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604175509  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604175509                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605025516  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605025516  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141506                      G�O�G�O�G�O�                