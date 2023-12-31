CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-02-07T18:03:29Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \l   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `P   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  h   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �L   TEMP_ADJUSTED_QC         
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
_FillValue                 �  �D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �(   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ܴ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �    HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �`   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �p   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �t   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20230207180329  20230207180329  5906096 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7902                            2B  A   NAVIS_A                         1010                            170425                          863 @�	�m	�\1   @�	���@*a���o�cw|�hs1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   F   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBhffBo��Bx  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��fD��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @~z@���@���AQ�A<Q�A\Q�A|Q�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B{B{B{B{B'{B/{B7{B?{BG{BO{BW{B_z�Bgz�Bn�Bw{B{B�W
B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��pBÊ=BǊ=Bˊ=Bϊ=Bӊ=B׊=Bۊ=Bߊ=B�=B�=B�=B�=B�=B��=B��=B��=C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��D qHD �HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HD	qHD	�HD
qHD
�HDqHD��DqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HD qHD �HD!qHD!�HD"qHD"�HD#qHD#�HD$qHD$�HD%qHD%�HD&qHD&�HD'qHD'�HD(qHD(�HD)qHD)�HD*qHD*�HD+qHD+�HD,qHD,�HD-qHD-�HD.qHD.�HD/qHD/�HD0qHD0�HD1qHD1�HD2qHD2�HD3qHD3�HD4qHD4�HD5qHD5�HD6qHD6�HD7qHD7�HD8qHD8�HD9qHD9�HD:qHD:�HD;qHD;�HD<qHD<�HD=qHD=�HD>qHD>�HD?qHD?�HD@qHD@�HDAqHDA�HDBqHDB�HDCqHDC�HDDqHDD�HDEqHDE�HDFqHDF�HDGqHDG�HDHqHDH�HDIqHDI�HDJqHDJ�HDKqHDK�HDLqHDL�HDMqHDM�HDNqHDN�HDOqHDO�HDPqHDP�HDQqHDQ�HDRqHDR�HDSqHDS�HDTqHDT�HDUqHDU�HDVqHDV�HDWqHDW�HDXqHDX�HDYqHDY�HDZqHDZ�HD[qHD[�HD\qHD\�HD]qHD]�HD^qHD^�HD_qHD_�HD`qHD`�HDaqHDa�HDbqHDb�HDcqHDc�HDdqHDd�HDeqHDe�HDfqHDf�HDgqHDg�HDhqHDh�HDiqHDi�HDjqHDj�HDkqHDk�HDlqHDl�HDmqHDm�HDnqHDn�HDoqHDo�HDpqHDp�HDqqHDq�HDrqHDr�HDsqHDs�HDtqHDt�HDuqHDu�HDvqHDv�HDwqHDw�HDxqHDx�HDyqHDy�HDzqHDz�HD{qHD{�HD|qHD|�HD}qHD}�HD~qHD~�HDqHD�HD�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D��qD�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D¸�D���D�8�D�x�Dø�D���D�8�D�x�Dĸ�D���D�8�D�x�DŸ�D���D�8�D�x�DƸ�D���D�8�D�x�DǸ�D���D�8�D�x�Dȸ�D���D�8�D�x�Dɸ�D���D�8�D�x�Dʸ�D���D�8�D�x�D˸�D���D�8�D�x�D̸�D���D�8�D�x�D͸�D���D�8�D�x�Dθ�D���D�8�D�x�Dϸ�D���D�8�D�x�Dи�D���D�8�D�x�DѸ�D���D�8�D�x�DҸ�D���D�8�D�x�DӸ�D���D�8�D�x�DԸ�D���D�8�D�x�Dո�D���D�8�D�x�Dָ�D���D�8�D�x�D׸�D���D�8�D�x�Dظ�D���D�8�D�x�Dٸ�D���D�8�D�x�Dڸ�D���D�8�D�x�D۸�D���D�8�D�x�Dܸ�D���D�8�D�x�Dݸ�D���D�8�D�x�D޸�D���D�8�D�x�D߸�D���D�8�D�x�DฤD���D�8�D�x�DḤD���D�8�D�x�D⸤D���D�8�D�x�D㸤D���D�8�D�x�D两D���D�8�D�x�D帤D���D�8�D�x�D渤D���D�8�D�x�D縤D���D�8�D�x�D踤D���D�8�D�x�D鸤D���D�8�D�x�D긤D���D�8�D�x�D븤D���D�8�D�x�D츤D���D�8�D�x�D���D���D�8�D�x�DD���D�8�D�x�D︤D���D�8�D�x�D�D���D�8�D�x�D�D���D�8�D�x�D�D���D�8�D�x�D�D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�{�D��
D��>11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A���A���A���A���A���A���A���A�  A�A���A�  A�  A�A�VA�JA�bA��A��A� �A� �A��A�bAӺ^A�p�A��A��
A�~�Aϴ9A��TAȧ�A���A�A�"�A���A��/A��DA�ƨA��wA���A��#A��hA�XA�dZA��A��wA�$�A��A�I�A�Q�A�ffA��A���A��A�E�A�;dA��9A��A��wA�ZA~ffAu%Ap�Am��Ah�RAd��A_�AY�AUx�AT��AS�PAR�jAS�ATbAR�AM�TAH �AFĜAF1AE;dADbAC��ACAAO�A>ZA<��A<�/A;��A;�
A;|�A;S�A<JA<  A:��A:��A:�9A:�\A9S�A8�A7�wA7+A6��A6A�A5��A5%A4�RA333A1�A1p�A0A/�A-�A,��A+��A+7LA*ffA)ƨA)��A)��A(�RA(�DA(�+A(ffA(ZA'�7A&�DA%t�A$I�A$ �A#��A#33A"��A"1'A"ZA"5?A"�uA"�jA"�A"�DA"VA"1A!��A!�A!;dA!%A �jA r�A 1'A�
A�AC�AbNA`BA�A��A��A�AAt�AC�A�A�/A~�A�
At�A��AA�A��A��A��A~�A�;AVA5?A|�A��A�^A��A�RA��AVA��AO�A
=A�`A�9A�Az�A9XA �A�TAXA33A��A�DAbNA �A�wA�AS�AA
�A
9XA
JA	�#A	;dA�A�RA�+AQ�A��AK�A�`A��Az�A-A�#A�A�A��A�;A��AJAK�A
=A �A �!A �A ��A �A ��A j@��@�ȴ@�=q@��^@�/@�1'@�l�@�@�~�@�=q@��#@�G�@�7L@�I�@�K�@�ȴ@�J@���@��7@�p�@�X@��@���@�(�@�
=@��@���@�(�@�w@�
=@���@�R@@�/@�9X@�w@�@�;d@�@�!@�{@�^@陚@���@�t�@�V@��@�x�@��@�  @�o@�^5@�@�X@�7L@��@�A�@���@ߕ�@�ȴ@�n�@�ff@�E�@�&�@�1'@�1'@�S�@�ff@��@��#@�@�X@���@ج@ج@�b@��m@��
@�ƨ@׮@ו�@��y@֧�@�ff@�=q@ղ-@�?}@ԓu@�t�@�33@ҸR@��@�O�@���@У�@�9X@��@ϝ�@�"�@Χ�@�v�@�^5@��#@�O�@�I�@�  @ˮ@�\)@��@ʏ\@�5?@�@ə�@�&�@�%@ȣ�@�bN@�9X@��;@�;d@�@Ɨ�@�{@š�@�V@�Z@�@���@�&�@��@���@��D@�Q�@�9X@�1@��@�ff@�hs@��`@�Z@�  @��w@�|�@��y@�E�@��-@�?}@��@���@�z�@�1@��w@�o@�ff@���@�7L@��D@�  @�l�@���@���@�`B@�&�@���@��/@��u@�(�@���@�t�@�C�@���@�V@��^@�G�@�V@��@���@�(�@�33@�^5@��#@�?}@��@�r�@���@�t�@�+@�@���@�~�@��#@�&�@��j@��@� �@��@�@��+@�ff@��@�@��@�hs@�V@��D@�A�@��@��m@��@�C�@�;d@�;d@�33@��@��y@���@�-@�p�@��@���@��9@��@�I�@� �@�1@��
@��@�|�@�K�@��@���@�^5@�-@��T@��-@��7@�?}@��9@��;@���@��P@�\)@�"�@��y@���@��+@�ff@���@���@���@�hs@��@��@�1'@�  @���@���@�t�@��H@�^5@��@��7@��/@��@��
@��@�S�@�C�@��@��H@��+@�v�@��@���@�p�@�?}@���@�Ĝ@��D@�1'@�1@�ƨ@�|�@�K�@��@��R@��\@�ff@�{@���@�X@�?}@��@���@��@�r�@�A�@�1@��;@�ƨ@��@���@�+@���@�~�@�ff@�E�@��@�@���@��h@�x�@�x�@�x�@��@�x�@�G�@��@�Z@�A�@�A�@�A�@�1'@� �@��
@��F@�l�@�33@�+@�o@��!@�M�@��@�J@��@��#@��^@�G�@�/@�V@�%@��@���@�z�@�Z@|�@~�@~v�@~E�@~@}?}@|��@|��@|j@|j@|Z@|(�@{�@{@z��@z�\@zn�@z-@y��@y7L@x��@x�@xQ�@x  @w|�@w�@v�+@v{@u@u�h@u?}@t��@tj@tj@tI�@s��@r��@rM�@q&�@p��@p��@pr�@p1'@ol�@n�y@n�R@n$�@mp�@l�/@k�m@k��@kdZ@ko@j�!@j~�@i�@ix�@h��@h�9@hr�@h1'@g�;@g�P@g�@fff@e�T@e�@d9X@c�F@c��@co@b�!@bJ@ahs@a&�@`��@`Ĝ@`��@`��@`Q�@`A�@_�;@_\)@^��@^@]��@]�@]`B@\�D@[t�@[o@Z�@Z�H@Z��@Z��@Z�@YG�@X�9@Xr�@X  @W��@W�w@W��@WK�@Vȴ@V�+@V{@U�@U��@U`B@T�@T��@T�@TZ@S�m@S��@SdZ@S33@R�@R��@R~�@R=q@Q�7@Q%@P�`@PĜ@Pb@O��@O�@Nȴ@NV@M��@Mp�@MV@L�@Lz�@LI�@L�@K��@Kƨ@KdZ@J�@Jn�@I�@I��@I7L@H��@Hb@G�w@GK�@G+@G�@G�@F�y@F�@F��@F�+@F{@E��@E��@E@E��@EO�@E?}@E�@D�/@Dz�@D�@Cƨ@C��@CdZ@CC�@C@B��@B��@B=q@B-@A�#@A��@@�9@?�@?��@?�w@?|�@?K�@?�@>�@>�+@=�@=/@<�@<�@<j@<(�@<1@;��@;33@;o@:�H@:��@:=q@9�@9��@9�^@9��@9x�@9G�@9%@8��@8��@8�`@81'@7�@7l�@6��@6v�@5��@5��@5O�@5�@4��@4��@4j@4(�@41@3ƨ@3��@3t�@3dZ@3o@2��@2��@2M�@2J@2J@1��@1��@1�@1�#@1�7@17L@1�@0��@0��@0�9@0�u@0r�@0 �@/�;@/�@/��@/|�@.�y@.5?@-@-O�@,��@,I�@+�m@+dZ@*^5@*J@)�^@)%@(�@(Q�@( �@'��@'K�@'�@&�y@&�+@%�@%@%�-@%�h@%?}@%/@$��@$��@$9X@#ƨ@#��@#�@#t�@#dZ@#S�@#S�@"�H@"��@"n�@"M�@!��@!�@!�#@!��@!��@!��@!��@!&�@ �`@ Q�@   @��@\)@�R@�+@E�@��@p�@O�@?}@�@�j@I�@1@�F@��@dZ@C�@�@��@��@�\@~�@=q@�@J@�#@��@�7@G�@�@%@��@Ĝ@�u@r�@1'@�@�P@|�@l�@�@��@�@ff@@@�h@p�@?}@�@V@V@��@�j@Z@9X@(�@�@1@1@��@�m@��@t�@�H@�\@~�@n�@n�@^5@�@��@�#@��@��@�^@��@hs@�@�@%@��@�`@Ĝ@�9@��@�@Q�@1'@b@  @�w@\)@+@
=@�y@�y@�y@�@�@ȴ@��@��@ff@5?@@@�@p�@/@��@�/@�/@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A���A���A���A���A���A���A���A�  A�A���A�  A�  A�A�VA�JA�bA��A��A� �A� �A��A�bAӺ^A�p�A��A��
A�~�Aϴ9A��TAȧ�A���A�A�"�A���A��/A��DA�ƨA��wA���A��#A��hA�XA�dZA��A��wA�$�A��A�I�A�Q�A�ffA��A���A��A�E�A�;dA��9A��A��wA�ZA~ffAu%Ap�Am��Ah�RAd��A_�AY�AUx�AT��AS�PAR�jAS�ATbAR�AM�TAH �AFĜAF1AE;dADbAC��ACAAO�A>ZA<��A<�/A;��A;�
A;|�A;S�A<JA<  A:��A:��A:�9A:�\A9S�A8�A7�wA7+A6��A6A�A5��A5%A4�RA333A1�A1p�A0A/�A-�A,��A+��A+7LA*ffA)ƨA)��A)��A(�RA(�DA(�+A(ffA(ZA'�7A&�DA%t�A$I�A$ �A#��A#33A"��A"1'A"ZA"5?A"�uA"�jA"�A"�DA"VA"1A!��A!�A!;dA!%A �jA r�A 1'A�
A�AC�AbNA`BA�A��A��A�AAt�AC�A�A�/A~�A�
At�A��AA�A��A��A��A~�A�;AVA5?A|�A��A�^A��A�RA��AVA��AO�A
=A�`A�9A�Az�A9XA �A�TAXA33A��A�DAbNA �A�wA�AS�AA
�A
9XA
JA	�#A	;dA�A�RA�+AQ�A��AK�A�`A��Az�A-A�#A�A�A��A�;A��AJAK�A
=A �A �!A �A ��A �A ��A j@��@�ȴ@�=q@��^@�/@�1'@�l�@�@�~�@�=q@��#@�G�@�7L@�I�@�K�@�ȴ@�J@���@��7@�p�@�X@��@���@�(�@�
=@��@���@�(�@�w@�
=@���@�R@@�/@�9X@�w@�@�;d@�@�!@�{@�^@陚@���@�t�@�V@��@�x�@��@�  @�o@�^5@�@�X@�7L@��@�A�@���@ߕ�@�ȴ@�n�@�ff@�E�@�&�@�1'@�1'@�S�@�ff@��@��#@�@�X@���@ج@ج@�b@��m@��
@�ƨ@׮@ו�@��y@֧�@�ff@�=q@ղ-@�?}@ԓu@�t�@�33@ҸR@��@�O�@���@У�@�9X@��@ϝ�@�"�@Χ�@�v�@�^5@��#@�O�@�I�@�  @ˮ@�\)@��@ʏ\@�5?@�@ə�@�&�@�%@ȣ�@�bN@�9X@��;@�;d@�@Ɨ�@�{@š�@�V@�Z@�@���@�&�@��@���@��D@�Q�@�9X@�1@��@�ff@�hs@��`@�Z@�  @��w@�|�@��y@�E�@��-@�?}@��@���@�z�@�1@��w@�o@�ff@���@�7L@��D@�  @�l�@���@���@�`B@�&�@���@��/@��u@�(�@���@�t�@�C�@���@�V@��^@�G�@�V@��@���@�(�@�33@�^5@��#@�?}@��@�r�@���@�t�@�+@�@���@�~�@��#@�&�@��j@��@� �@��@�@��+@�ff@��@�@��@�hs@�V@��D@�A�@��@��m@��@�C�@�;d@�;d@�33@��@��y@���@�-@�p�@��@���@��9@��@�I�@� �@�1@��
@��@�|�@�K�@��@���@�^5@�-@��T@��-@��7@�?}@��9@��;@���@��P@�\)@�"�@��y@���@��+@�ff@���@���@���@�hs@��@��@�1'@�  @���@���@�t�@��H@�^5@��@��7@��/@��@��
@��@�S�@�C�@��@��H@��+@�v�@��@���@�p�@�?}@���@�Ĝ@��D@�1'@�1@�ƨ@�|�@�K�@��@��R@��\@�ff@�{@���@�X@�?}@��@���@��@�r�@�A�@�1@��;@�ƨ@��@���@�+@���@�~�@�ff@�E�@��@�@���@��h@�x�@�x�@�x�@��@�x�@�G�@��@�Z@�A�@�A�@�A�@�1'@� �@��
@��F@�l�@�33@�+@�o@��!@�M�@��@�J@��@��#@��^@�G�@�/@�V@�%@��@���@�z�@�Z@|�@~�@~v�@~E�@~@}?}@|��@|��@|j@|j@|Z@|(�@{�@{@z��@z�\@zn�@z-@y��@y7L@x��@x�@xQ�@x  @w|�@w�@v�+@v{@u@u�h@u?}@t��@tj@tj@tI�@s��@r��@rM�@q&�@p��@p��@pr�@p1'@ol�@n�y@n�R@n$�@mp�@l�/@k�m@k��@kdZ@ko@j�!@j~�@i�@ix�@h��@h�9@hr�@h1'@g�;@g�P@g�@fff@e�T@e�@d9X@c�F@c��@co@b�!@bJ@ahs@a&�@`��@`Ĝ@`��@`��@`Q�@`A�@_�;@_\)@^��@^@]��@]�@]`B@\�D@[t�@[o@Z�@Z�H@Z��@Z��@Z�@YG�@X�9@Xr�@X  @W��@W�w@W��@WK�@Vȴ@V�+@V{@U�@U��@U`B@T�@T��@T�@TZ@S�m@S��@SdZ@S33@R�@R��@R~�@R=q@Q�7@Q%@P�`@PĜ@Pb@O��@O�@Nȴ@NV@M��@Mp�@MV@L�@Lz�@LI�@L�@K��@Kƨ@KdZ@J�@Jn�@I�@I��@I7L@H��@Hb@G�w@GK�@G+@G�@G�@F�y@F�@F��@F�+@F{@E��@E��@E@E��@EO�@E?}@E�@D�/@Dz�@D�@Cƨ@C��@CdZ@CC�@C@B��@B��@B=q@B-@A�#@A��@@�9@?�@?��@?�w@?|�@?K�@?�@>�@>�+@=�@=/@<�@<�@<j@<(�@<1@;��@;33@;o@:�H@:��@:=q@9�@9��@9�^@9��@9x�@9G�@9%@8��@8��@8�`@81'@7�@7l�@6��@6v�@5��@5��@5O�@5�@4��@4��@4j@4(�@41@3ƨ@3��@3t�@3dZ@3o@2��@2��@2M�@2J@2J@1��@1��@1�@1�#@1�7@17L@1�@0��@0��@0�9@0�u@0r�@0 �@/�;@/�@/��@/|�@.�y@.5?@-@-O�@,��@,I�@+�m@+dZ@*^5@*J@)�^@)%@(�@(Q�@( �@'��@'K�@'�@&�y@&�+@%�@%@%�-@%�h@%?}@%/@$��@$��@$9X@#ƨ@#��@#�@#t�@#dZ@#S�@#S�@"�H@"��@"n�@"M�@!��@!�@!�#@!��@!��@!��@!��@!&�@ �`@ Q�@   @��@\)@�R@�+@E�@��@p�@O�@?}@�@�j@I�@1@�F@��@dZ@C�@�@��@��@�\@~�@=q@�@J@�#@��@�7@G�@�@%@��@Ĝ@�u@r�@1'@�@�P@|�@l�@�@��@�@ff@@@�h@p�@?}@�@V@V@��@�j@Z@9X@(�@�@1@1@��@�m@��@t�@�H@�\@~�@n�@n�@^5@�@��@�#@��@��@�^@��@hs@�@�@%@��@�`@Ĝ@�9@��@�@Q�@1'@b@  @�w@\)@+@
=@�y@�y@�y@�@�@ȴ@��@��@ff@5?@@@�@p�@/@��@�/@�/@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	B	B	B	B	B	B	B	B	B	B	B	B	B	B	%B		7B	1B	1B	JB	DB	PB	\B	�B	49B
#�BPB%�B(�B'�B)�B'�B/BG�B%�B �B7LBbNBq�B��Bt�BbNBbNBp�Bp�Bu�Bx�By�Bn�Bm�BjBbNBXBC�B�B
��B
�/B
��B
��B
�qB
�\B
iyB
uB	�B	�sB	�;B	��B	ÖB	ǮB	�FB	�!B	�9B	�qB	�B
1'B
`BB
gmB
p�B
l�B
~�B
��B
�jB
ÖB
��B
�B
�#B
��B
��B
�HB
�fB
�B
��B
��B\B�BoBhB�B�B"�B�B�B#�B#�B"�B �B�B �B�BJB�BbBhBVB	7BBBBBB+B1B+B
=BVBoBVBB
��B
��B
��B
�B
�B
�B
��B
��B
��B%BVBPBbB\BVBPBPBPBJBJB
=B
=B%BBB
��B
��B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�`B
�TB
�;B
�BB
�;B
�#B
��B
��B
ÖB
�dB
�FB
�B
�!B
�RB
�jB
�wB
�jB
�jB
�jB
�jB
�qB
B
ÖB
ÖB
B
��B
��B
�}B
�wB
�wB
�qB
�qB
�jB
�dB
�^B
�XB
�RB
�LB
�FB
�?B
�3B
�3B
�-B
�'B
�!B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B
�uB
�\B
�PB
�DB
�DB
�DB
�VB
�bB
�hB
�uB
�uB
�oB
�bB
�\B
�\B
�PB
�JB
�DB
�=B
�=B
�7B
�7B
�1B
�+B
�+B
�+B
�B
�B
�B
�B
�B
�B
�B
�B
� B
}�B
z�B
w�B
u�B
u�B
u�B
u�B
u�B
u�B
w�B
y�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
z�B
z�B
z�B
x�B
y�B
x�B
u�B
r�B
q�B
o�B
o�B
u�B
v�B
w�B
w�B
v�B
v�B
v�B
u�B
u�B
s�B
s�B
x�B
w�B
u�B
u�B
w�B
v�B
w�B
v�B
v�B
x�B
w�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
x�B
x�B
v�B
w�B
w�B
w�B
v�B
v�B
u�B
v�B
u�B
u�B
u�B
u�B
t�B
u�B
w�B
v�B
v�B
v�B
u�B
v�B
v�B
v�B
w�B
w�B
w�B
v�B
v�B
u�B
t�B
t�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
u�B
s�B
p�B
p�B
q�B
r�B
r�B
r�B
s�B
s�B
t�B
r�B
r�B
r�B
r�B
r�B
t�B
u�B
t�B
s�B
s�B
s�B
r�B
r�B
r�B
r�B
r�B
q�B
q�B
q�B
o�B
p�B
o�B
o�B
p�B
m�B
p�B
r�B
r�B
s�B
s�B
t�B
t�B
u�B
v�B
v�B
u�B
x�B
y�B
y�B
y�B
z�B
z�B
{�B
|�B
|�B
|�B
}�B
|�B
|�B
|�B
}�B
~�B
� B
� B
~�B
~�B
�B
�B
�B
~�B
}�B
}�B
~�B
~�B
� B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�B
�B
�%B
�%B
�B
�%B
�+B
�1B
�7B
�=B
�=B
�=B
�=B
�DB
�JB
�PB
�PB
�PB
�PB
�VB
�VB
�VB
�bB
�\B
�bB
�bB
�hB
�oB
�oB
�oB
�uB
�oB
�oB
�uB
�oB
�oB
�{B
�{B
�uB
�{B
�{B
��B
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
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�'B
�!B
�'B
�!B
�'B
�'B
�-B
�-B
�-B
�-B
�-B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�9B
�3B
�9B
�9B
�9B
�?B
�?B
�?B
�?B
�?B
�?B
�FB
�?B
�FB
�FB
�FB
�FB
�FB
�LB
�LB
�LB
�RB
�LB
�RB
�LB
�RB
�RB
�RB
�RB
�RB
�XB
�XB
�XB
�XB
�XB
�^B
�^B
�^B
�dB
�dB
�^B
�dB
�dB
�^B
�dB
�dB
�dB
�jB
�jB
�dB
�jB
�jB
�qB
�wB
�wB
�wB
�wB
�wB
�}B
�wB
�}B
�}B
��B
��B
��B
��B
��B
B
B
B
B
B
B
ÖB
B
ÖB
ÖB
ÖB
ÖB
ÖB
ĜB
ÖB
ĜB
ĜB
ŢB
ŢB
ĜB
ŢB
ŢB
ŢB
ŢB
ŢB
ŢB
ƨB
ƨB
ǮB
ǮB
ǮB
ǮB
ǮB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ɺB
ɺB
ɺB
ɺB
ɺB
ɺB
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
��B
��B
��B
�B
�B
�B
�B
�
B
�
B
�
B
�
B
�
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�#B
�B
�#B
�B
�B
�#B
�#B
�#B
�#B
�#B
�#B
�#B
�#B
�#B
�#B
�)B
�)B
�)B
�/B
�/B
�/B
�/B
�/B
�/B
�/B
�/B
�5B
�5B
�;B
�;B
�;B
�;B
�;B
�;B
�;B
�;B
�;B
�BB
�BB
�BB
�BB
�BB
�BB
�BB
�BB
�HB
�HB
�HB
�NB
�NB
�NB
�NB
�NB
�NB
�NB
�NB
�TB
�NB
�NB
�NB
�TB
�TB
�TB
�TB
�TB
�TB
�TB
�ZB
�ZB
�ZB
�ZB
�ZB
�`B
�ZB
�`B
�`B
�`B
�fB
�fB
�fB
�fB
�fB
�fB
�fB
�fB
�fB
�fB
�fB
�fB
�mB
�mB
�mB
�mB
�mB
�mB
�mB
�s33333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333 B	B	B	B	B	B	B	B	B	B	B	B	B	B	B	%B		7B	1B	1B	JB	DB	PB	\B	�B	49B
#�BPB%�B(�B'�B)�B'�B/BG�B%�B �B7LBbNBq�B��Bt�BbNBbNBp�Bp�Bu�Bx�By�Bn�Bm�BjBbNBXBC�B�B
��B
�/B
��B
��B
�qB
�\B
iyB
uB	�B	�sB	�;B	��B	ÖB	ǮB	�FB	�!B	�9B	�qB	�B
1'B
`BB
gmB
p�B
l�B
~�B
��B
�jB
ÖB
��B
�B
�#B
��B
��B
�HB
�fB
�B
��B
��B\B�BoBhB�B�B"�B�B�B#�B#�B"�B �B�B �B�BJB�BbBhBVB	7BBBBBB+B1B+B
=BVBoBVBB
��B
��B
��B
�B
�B
�B
��B
��B
��B%BVBPBbB\BVBPBPBPBJBJB
=B
=B%BBB
��B
��B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�`B
�TB
�;B
�BB
�;B
�#B
��B
��B
ÖB
�dB
�FB
�B
�!B
�RB
�jB
�wB
�jB
�jB
�jB
�jB
�qB
B
ÖB
ÖB
B
��B
��B
�}B
�wB
�wB
�qB
�qB
�jB
�dB
�^B
�XB
�RB
�LB
�FB
�?B
�3B
�3B
�-B
�'B
�!B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B
�uB
�\B
�PB
�DB
�DB
�DB
�VB
�bB
�hB
�uB
�uB
�oB
�bB
�\B
�\B
�PB
�JB
�DB
�=B
�=B
�7B
�7B
�1B
�+B
�+B
�+B
�B
�B
�B
�B
�B
�B
�B
�B
� B
}�B
z�B
w�B
u�B
u�B
u�B
u�B
u�B
u�B
w�B
y�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
z�B
z�B
z�B
x�B
y�B
x�B
u�B
r�B
q�B
o�B
o�B
u�B
v�B
w�B
w�B
v�B
v�B
v�B
u�B
u�B
s�B
s�B
x�B
w�B
u�B
u�B
w�B
v�B
w�B
v�B
v�B
x�B
w�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
x�B
x�B
v�B
w�B
w�B
w�B
v�B
v�B
u�B
v�B
u�B
u�B
u�B
u�B
t�B
u�B
w�B
v�B
v�B
v�B
u�B
v�B
v�B
v�B
w�B
w�B
w�B
v�B
v�B
u�B
t�B
t�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
u�B
s�B
p�B
p�B
q�B
r�B
r�B
r�B
s�B
s�B
t�B
r�B
r�B
r�B
r�B
r�B
t�B
u�B
t�B
s�B
s�B
s�B
r�B
r�B
r�B
r�B
r�B
q�B
q�B
q�B
o�B
p�B
o�B
o�B
p�B
m�B
p�B
r�B
r�B
s�B
s�B
t�B
t�B
u�B
v�B
v�B
u�B
x�B
y�B
y�B
y�B
z�B
z�B
{�B
|�B
|�B
|�B
}�B
|�B
|�B
|�B
}�B
~�B
� B
� B
~�B
~�B
�B
�B
�B
~�B
}�B
}�B
~�B
~�B
� B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�B
�B
�%B
�%B
�B
�%B
�+B
�1B
�7B
�=B
�=B
�=B
�=B
�DB
�JB
�PB
�PB
�PB
�PB
�VB
�VB
�VB
�bB
�\B
�bB
�bB
�hB
�oB
�oB
�oB
�uB
�oB
�oB
�uB
�oB
�oB
�{B
�{B
�uB
�{B
�{B
��B
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
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�'B
�!B
�'B
�!B
�'B
�'B
�-B
�-B
�-B
�-B
�-B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�9B
�3B
�9B
�9B
�9B
�?B
�?B
�?B
�?B
�?B
�?B
�FB
�?B
�FB
�FB
�FB
�FB
�FB
�LB
�LB
�LB
�RB
�LB
�RB
�LB
�RB
�RB
�RB
�RB
�RB
�XB
�XB
�XB
�XB
�XB
�^B
�^B
�^B
�dB
�dB
�^B
�dB
�dB
�^B
�dB
�dB
�dB
�jB
�jB
�dB
�jB
�jB
�qB
�wB
�wB
�wB
�wB
�wB
�}B
�wB
�}B
�}B
��B
��B
��B
��B
��B
B
B
B
B
B
B
ÖB
B
ÖB
ÖB
ÖB
ÖB
ÖB
ĜB
ÖB
ĜB
ĜB
ŢB
ŢB
ĜB
ŢB
ŢB
ŢB
ŢB
ŢB
ŢB
ƨB
ƨB
ǮB
ǮB
ǮB
ǮB
ǮB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ɺB
ɺB
ɺB
ɺB
ɺB
ɺB
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
��B
��B
��B
�B
�B
�B
�B
�
B
�
B
�
B
�
B
�
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�#B
�B
�#B
�B
�B
�#B
�#B
�#B
�#B
�#B
�#B
�#B
�#B
�#B
�#B
�)B
�)B
�)B
�/B
�/B
�/B
�/B
�/B
�/B
�/B
�/B
�5B
�5B
�;B
�;B
�;B
�;B
�;B
�;B
�;B
�;B
�;B
�BB
�BB
�BB
�BB
�BB
�BB
�BB
�BB
�HB
�HB
�HB
�NB
�NB
�NB
�NB
�NB
�NB
�NB
�NB
�TB
�NB
�NB
�NB
�TB
�TB
�TB
�TB
�TB
�TB
�TB
�ZB
�ZB
�ZB
�ZB
�ZB
�`B
�ZB
�`B
�`B
�`B
�fB
�fB
�fB
�fB
�fB
�fB
�fB
�fB
�fB
�fB
�fB
�fB
�mB
�mB
�mB
�mB
�mB
�mB
�mB
�s33333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.23 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted during real time processing based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       20230207180329                              AO  ARCAADJP                                                                    20230207180329    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20230207180329  QCP$                G�O�G�O�G�O�1B83E           AO  ARGQQCPL                                                                    20230207180329  QCF$                G�O�G�O�G�O�8000            