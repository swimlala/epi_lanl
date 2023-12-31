CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-06-24T09:48:33Z creation;2016-06-24T09:48:35Z conversion to V3.1;2019-12-19T08:37:34Z update;     
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
_FillValue                 �  I$   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `t   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �L   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �$   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �4   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ܼ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �L   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �L   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �L   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �L   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �,   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �0   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �@   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �D   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �H   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �LArgo profile    3.1 1.2 19500101000000  20160624094833  20200115101518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               	A   JA  I2_0576_009                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @׵�%[g 1   @׵��}( @;�iDg8�dr�t�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7�fD8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@y�DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�3D�C3Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��fD��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @~{@���@���AQ�A<Q�A\Q�A|Q�A�(�A�(�A�(�A�(�A�(�A�(�A���A�(�B{B{B{B{B'{B/{B7{B?{BG{BO{BW{B_{Bg{Bo{Bw{B{B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=BÊ=BǊ=Bˊ=Bϊ=Bӊ=B׊=Bۊ=Bߊ=B�=B�=B�=B�=B�=B��=B��=B��=C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D qHD �HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HD	qHD	�HD
qHD
�HDqHD�HDqHD�HDqHD�HDqHD��DqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HD qHD �HD!qHD!�HD"qHD"�HD#qHD#�HD$qHD$�HD%qHD%�HD&qHD&�HD'qHD'�HD(qHD(�HD)qHD)�HD*qHD*�HD+qHD+�HD,qHD,�HD-qHD-�HD.qHD.�HD/qHD/�HD0qHD0�HD1qHD1�HD2qHD2�HD3qHD3�HD4qHD4�HD5qHD5�HD6qHD6�HD7w�D7�HD8qHD8�HD9qHD9�HD:qHD:�HD;qHD;�HD<qHD<�HD=qHD=�HD>qHD>�HD?qHD?�HD@j�D@�HDAqHDA�HDBqHDB�HDCqHDC�HDDqHDD�HDEqHDE�HDFqHDF�HDGqHDG�HDHqHDH�HDIqHDI�HDJqHDJ�HDKqHDK�HDLqHDL�HDMqHDM�HDNqHDN�HDOqHDO�HDPqHDP�HDQqHDQ�HDRqHDR�HDSqHDS�HDTqHDT�HDUqHDU�HDVqHDV�HDWqHDW�HDXqHDX�HDYqHDY�HDZqHDZ�HD[qHD[�HD\qHD\�HD]qHD]�HD^qHD^�HD_qHD_�HD`qHD`�HDaqHDa�HDbqHDb�HDcqHDc�HDdqHDd�HDeqHDe�HDfqHDf�HDgqHDg�HDhqHDh�HDiqHDi�HDjqHDj�HDkqHDk�HDlqHDl�HDmqHDm�HDnqHDn�HDoqHDo�HDpqHDp�HDqqHDq�HDrqHDr�HDsqHDs�HDtqHDt�HDuqHDu�HDvqHDv�HDwqHDw�HDxqHDx�HDyqHDy�HDzqHDz�HD{qHD{�HD|qHD|�HD}qHD}�HD~qHD~�HDqHD�HD�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�{�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D��qD���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�5qD�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D¸�D���D�8�D�x�Dø�D���D�8�D�x�Dĸ�D���D�8�D�x�DŸ�D���D�8�D�x�DƸ�D���D�8�D�x�DǸ�D���D�8�D�x�Dȸ�D���D�8�D�x�Dɸ�D���D�8�D�x�Dʸ�D���D�8�D�x�D˸�D���D�8�D�x�D̸�D���D�8�D�x�D͸�D���D�8�D�x�Dθ�D���D�8�D�x�Dϸ�D���D�8�D�x�Dи�D���D�8�D�x�DѸ�D���D�8�D�x�DҸ�D���D�8�D�x�DӸ�D���D�8�D�x�DԸ�D���D�8�D�x�Dո�D���D�8�D�x�Dָ�D���D�8�D�x�D׸�D���D�8�D�x�Dظ�D���D�8�D�x�Dٸ�D���D�;�D�x�Dڸ�D���D�8�D�x�D۸�D���D�8�D�x�Dܸ�D���D�8�D�x�Dݸ�D���D�8�D�x�D޸�D���D�8�D�x�D߸�D���D�8�D�x�DฤD���D�8�D�uqDḤD���D�8�D�x�D⸤D���D�8�D�x�D㸤D���D�8�D�x�D两D���D�8�D�x�D帤D���D�8�D�x�D渤D���D�8�D�x�D縤D���D�8�D�x�D踤D���D�8�D�x�D鸤D���D�8�D�x�D긤D���D�8�D�x�D븤D���D�8�D�x�D츤D���D�8�D�x�D���D���D�8�D�x�DD���D�8�D�x�D︤D���D�8�D�x�D�D���D�8�D�x�D�D���D�8�D�x�D�D���D�8�D�x�D�D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�;�D�
D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A���A���A���A���A���A���A���A��A��A��A��A��A���AōPAÛ�A�^5A�ZA�"�A�ZA���A���A��\A���A�C�A�C�A�oA��/A�7LA��wA�hsA�5?A��A��A��hA�I�A��#A�t�A�oA��A�&�A�C�A�A���A�O�A�;dA�x�A��jA�%A���A�"�A�l�A�G�A���A�%A�$�A��A�-A���A��A��mA���A�S�A��jA��A�x�A��jA�bNA�  A�n�A���A���A�;dA�O�A�C�A��A�JA�"�A���A�7LA��/A�~�A�ȴA�M�A��jA��^A���A��jA��DA�dZA�+A�XA��A��-A�Q�A���A�C�A��;A��-A�S�A��A~�!A}�hA}K�A};dA}�A}�A}VA|�jA{�Az-Axr�Aw�^As��Ap�Ao�Ao��Ao�An�Am��AlĜAkXAi��Ah�jAhZAgx�AeXAc�FAb�Aa�A`z�A_ƨA^�`A^VA\�HAZ�AX��AX(�AW�AVĜAV�uAUt�AS�mASO�AR��AP��AOVAM��AL��AL�AL�RAK�AJVAH5?AF��ADr�ACACdZAB��AAG�A@bNA?�;A>�uA=7LA<�9A;��A:n�A9�hA8�!A7/A61'A4�+A4VA3��A2��A1�^A1hsA1?}A1
=A0ȴA0JA.��A-��A-oA,ZA+��A+��A+;dA*��A)�PA(^5A'��A'+A&ȴA&$�A%ƨA%
=A#��A#oA"��A!�A!&�A z�A A��Az�A\)AVA�mA�yA�A��A|�A�A�\A��Az�A�A�+A��A
=A�
Ar�A�PA��A�A�`AbA|�AG�A
ĜA
�A��A�yA��A�A&�A��A^5A�wA��AI�A {@��m@���@��@�ff@�J@��@�ƨ@��@�z�@��P@���@�z�@�@�\)@��@�$�@�Q�@�@�K�@�"�@��@��@�@�r�@�w@柾@���@�x�@�/@�9@��@��@���@�ff@��
@�v�@٩�@��/@�9X@��m@���@�E�@�hs@�A�@ҟ�@с@Ь@ϥ�@Η�@���@��@���@�(�@��@�x�@�bN@�l�@��@�X@�Z@�
=@��^@�%@���@��^@���@��@���@��D@��m@��@�p�@���@� �@���@�33@���@���@�@�G�@���@�o@�n�@�x�@���@��@���@�=q@��#@�I�@�;d@��@��h@���@� �@�|�@�33@���@�@�hs@��@��D@�Q�@��@�b@��w@�dZ@�33@��@���@���@�n�@��@���@�X@��@��@�r�@�j@�Z@�Q�@�Z@�I�@���@��
@��F@��@���@��+@�v�@�^5@�M�@�E�@�@��#@���@���@��@���@��@��w@�t�@�K�@���@�v�@�5?@�J@�%@�Z@�A�@�(�@��@���@�\)@���@��@��F@��@�t�@�"�@�@��@���@�n�@��T@�?}@�%@���@��
@���@�5?@�$�@��@��@��^@�X@���@��j@�Q�@��w@�"�@�n�@�J@���@���@��-@�hs@��@���@��9@�9X@�  @�ƨ@�|�@�\)@�S�@�K�@�K�@�C�@�33@��y@��R@�~�@�$�@��T@���@��^@���@�p�@�`B@�/@��9@���@�r�@�bN@�I�@�1'@�1@~�@~E�@}��@|�@|�@|Z@{�@{33@z�@z��@z�\@zn�@y�@x  @w�@w�;@w�w@w�@w�@v��@v5?@u��@t��@t�@t(�@sƨ@s�@s33@s@r�!@q�#@qhs@qG�@q7L@q�@pr�@p  @o�w@oK�@n�R@n@m�@l�j@l(�@kƨ@kƨ@k�@ko@j=q@i�^@ix�@iX@i�@hQ�@h1'@h �@h  @g|�@fȴ@f5?@e��@e`B@d�@dI�@d9X@d9X@d9X@d9X@d(�@d�@d1@c��@c�F@cdZ@c33@co@c@b�H@b~�@b^5@bJ@a�7@a%@`r�@` �@_;d@_�@_�@^�R@^{@^@^{@]�T@]�h@]O�@]�@\��@\�/@\��@\�/@\z�@\I�@[��@[�
@[�F@[dZ@[o@Z��@Z��@ZM�@Y�^@YX@Y�@X��@XĜ@X�9@X�9@X�u@X�@X �@X1'@X1'@W�;@W�@V�@Vȴ@V�R@Vff@V5?@V$�@V$�@V5?@U��@Tz�@S��@St�@SdZ@SdZ@SdZ@SdZ@SS�@SS�@S33@S"�@SC�@St�@SC�@SC�@R�@R�H@R��@R=q@Q��@Q�@Q�^@Q��@Qx�@Q&�@P�`@PbN@O��@O��@O;d@Nȴ@N��@Nv�@N$�@M��@Mp�@L�j@L�j@L�@Lz�@L1@KdZ@J�@J��@J�!@J��@J~�@J�@I�#@I�^@I��@Ix�@IG�@I&�@I%@H��@H��@H�9@H��@H�@HA�@Hb@Hb@G�;@G�w@G�@G��@G��@G��@Gl�@G+@F��@F�@Fv�@E��@E��@Ep�@D��@DZ@D9X@D�@C��@C�m@C�
@C�
@C��@C�@CS�@C"�@B��@B�\@B^5@BJ@A��@AG�@@Ĝ@@Q�@?�;@?;d@?
=@>�y@>�R@>�+@>{@=��@=�-@=?}@<�/@<�@;��@;�
@;ƨ@;��@;��@;33@:~�@:�@:J@9�@9�^@9x�@9�@8Ĝ@8�@8Q�@8A�@8 �@7�@7�@7K�@6�R@6v�@6{@5�T@5�-@5�-@5�@5?}@4�/@4�@4j@49X@3�m@3�@3S�@3o@2�@2�H@2��@2��@2~�@2M�@1��@1�^@1��@1��@1��@1�7@1�7@1x�@1&�@1%@0��@0�u@0A�@0b@/�@/�@.��@.ff@.E�@.{@-�@-�T@-�T@-��@-��@-��@-��@-��@-�h@-�@-?}@,��@,I�@+�
@+�@+o@*�@*�H@*�H@*�H@*�!@*-@)�7@)G�@(��@(Ĝ@(r�@(A�@(  @'�@'�@'l�@'+@&��@&ȴ@&��@&�+@&E�@&@%@%��@%�h@%`B@%`B@%?}@$�/@$�@$�D@$Z@$9X@$�@$1@#�m@#dZ@"�\@"n�@"M�@"�@!��@!��@!x�@!hs@!X@!G�@!7L@!&�@!�@!�@!%@ �9@ ��@ �@ bN@ 1'@   @�P@|�@l�@l�@\)@l�@\)@\)@��@ff@{@@`B@V@�j@��@z�@�@��@�m@�m@�
@��@��@dZ@C�@o@�!@n�@M�@=q@=q@-@��@��@�@��@��@�7@�7@X@%@��@�9@�u@�@r�@r�@r�@A�@1'@ �@�P@+@�@�y@ȴ@�+@E�@{@�-@O�@�@��@j@�@ƨ@��@C�@�H@�\@=q@�#@��@hs@&�@%@��@�`@��@Ĝ@Ĝ@�9@�u@�u@r�@A�@ �@�;@��@\)@;d@�@��@�y@��@E�@$�@�@�@��@��@��@��@@��@�h@p�@`B@`B@O�@V@�/@�@�D@Z@(�@1@��@dZ@o@
�H@
�H@
�H@
��@
��@
��@
~�@
^5@
M�@
=q@
-@	�@	�^@	��@	hs@	7L@	%@Ĝ@Ĝ@Ĝ@Ĝ@�9@�9@��@�u@�@r�@bN@A�@1'@  @�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A���A���A���A���A���A���A���A��A��A��A��A��A���AōPAÛ�A�^5A�ZA�"�A�ZA���A���A��\A���A�C�A�C�A�oA��/A�7LA��wA�hsA�5?A��A��A��hA�I�A��#A�t�A�oA��A�&�A�C�A�A���A�O�A�;dA�x�A��jA�%A���A�"�A�l�A�G�A���A�%A�$�A��A�-A���A��A��mA���A�S�A��jA��A�x�A��jA�bNA�  A�n�A���A���A�;dA�O�A�C�A��A�JA�"�A���A�7LA��/A�~�A�ȴA�M�A��jA��^A���A��jA��DA�dZA�+A�XA��A��-A�Q�A���A�C�A��;A��-A�S�A��A~�!A}�hA}K�A};dA}�A}�A}VA|�jA{�Az-Axr�Aw�^As��Ap�Ao�Ao��Ao�An�Am��AlĜAkXAi��Ah�jAhZAgx�AeXAc�FAb�Aa�A`z�A_ƨA^�`A^VA\�HAZ�AX��AX(�AW�AVĜAV�uAUt�AS�mASO�AR��AP��AOVAM��AL��AL�AL�RAK�AJVAH5?AF��ADr�ACACdZAB��AAG�A@bNA?�;A>�uA=7LA<�9A;��A:n�A9�hA8�!A7/A61'A4�+A4VA3��A2��A1�^A1hsA1?}A1
=A0ȴA0JA.��A-��A-oA,ZA+��A+��A+;dA*��A)�PA(^5A'��A'+A&ȴA&$�A%ƨA%
=A#��A#oA"��A!�A!&�A z�A A��Az�A\)AVA�mA�yA�A��A|�A�A�\A��Az�A�A�+A��A
=A�
Ar�A�PA��A�A�`AbA|�AG�A
ĜA
�A��A�yA��A�A&�A��A^5A�wA��AI�A {@��m@���@��@�ff@�J@��@�ƨ@��@�z�@��P@���@�z�@�@�\)@��@�$�@�Q�@�@�K�@�"�@��@��@�@�r�@�w@柾@���@�x�@�/@�9@��@��@���@�ff@��
@�v�@٩�@��/@�9X@��m@���@�E�@�hs@�A�@ҟ�@с@Ь@ϥ�@Η�@���@��@���@�(�@��@�x�@�bN@�l�@��@�X@�Z@�
=@��^@�%@���@��^@���@��@���@��D@��m@��@�p�@���@� �@���@�33@���@���@�@�G�@���@�o@�n�@�x�@���@��@���@�=q@��#@�I�@�;d@��@��h@���@� �@�|�@�33@���@�@�hs@��@��D@�Q�@��@�b@��w@�dZ@�33@��@���@���@�n�@��@���@�X@��@��@�r�@�j@�Z@�Q�@�Z@�I�@���@��
@��F@��@���@��+@�v�@�^5@�M�@�E�@�@��#@���@���@��@���@��@��w@�t�@�K�@���@�v�@�5?@�J@�%@�Z@�A�@�(�@��@���@�\)@���@��@��F@��@�t�@�"�@�@��@���@�n�@��T@�?}@�%@���@��
@���@�5?@�$�@��@��@��^@�X@���@��j@�Q�@��w@�"�@�n�@�J@���@���@��-@�hs@��@���@��9@�9X@�  @�ƨ@�|�@�\)@�S�@�K�@�K�@�C�@�33@��y@��R@�~�@�$�@��T@���@��^@���@�p�@�`B@�/@��9@���@�r�@�bN@�I�@�1'@�1@~�@~E�@}��@|�@|�@|Z@{�@{33@z�@z��@z�\@zn�@y�@x  @w�@w�;@w�w@w�@w�@v��@v5?@u��@t��@t�@t(�@sƨ@s�@s33@s@r�!@q�#@qhs@qG�@q7L@q�@pr�@p  @o�w@oK�@n�R@n@m�@l�j@l(�@kƨ@kƨ@k�@ko@j=q@i�^@ix�@iX@i�@hQ�@h1'@h �@h  @g|�@fȴ@f5?@e��@e`B@d�@dI�@d9X@d9X@d9X@d9X@d(�@d�@d1@c��@c�F@cdZ@c33@co@c@b�H@b~�@b^5@bJ@a�7@a%@`r�@` �@_;d@_�@_�@^�R@^{@^@^{@]�T@]�h@]O�@]�@\��@\�/@\��@\�/@\z�@\I�@[��@[�
@[�F@[dZ@[o@Z��@Z��@ZM�@Y�^@YX@Y�@X��@XĜ@X�9@X�9@X�u@X�@X �@X1'@X1'@W�;@W�@V�@Vȴ@V�R@Vff@V5?@V$�@V$�@V5?@U��@Tz�@S��@St�@SdZ@SdZ@SdZ@SdZ@SS�@SS�@S33@S"�@SC�@St�@SC�@SC�@R�@R�H@R��@R=q@Q��@Q�@Q�^@Q��@Qx�@Q&�@P�`@PbN@O��@O��@O;d@Nȴ@N��@Nv�@N$�@M��@Mp�@L�j@L�j@L�@Lz�@L1@KdZ@J�@J��@J�!@J��@J~�@J�@I�#@I�^@I��@Ix�@IG�@I&�@I%@H��@H��@H�9@H��@H�@HA�@Hb@Hb@G�;@G�w@G�@G��@G��@G��@Gl�@G+@F��@F�@Fv�@E��@E��@Ep�@D��@DZ@D9X@D�@C��@C�m@C�
@C�
@C��@C�@CS�@C"�@B��@B�\@B^5@BJ@A��@AG�@@Ĝ@@Q�@?�;@?;d@?
=@>�y@>�R@>�+@>{@=��@=�-@=?}@<�/@<�@;��@;�
@;ƨ@;��@;��@;33@:~�@:�@:J@9�@9�^@9x�@9�@8Ĝ@8�@8Q�@8A�@8 �@7�@7�@7K�@6�R@6v�@6{@5�T@5�-@5�-@5�@5?}@4�/@4�@4j@49X@3�m@3�@3S�@3o@2�@2�H@2��@2��@2~�@2M�@1��@1�^@1��@1��@1��@1�7@1�7@1x�@1&�@1%@0��@0�u@0A�@0b@/�@/�@.��@.ff@.E�@.{@-�@-�T@-�T@-��@-��@-��@-��@-��@-�h@-�@-?}@,��@,I�@+�
@+�@+o@*�@*�H@*�H@*�H@*�!@*-@)�7@)G�@(��@(Ĝ@(r�@(A�@(  @'�@'�@'l�@'+@&��@&ȴ@&��@&�+@&E�@&@%@%��@%�h@%`B@%`B@%?}@$�/@$�@$�D@$Z@$9X@$�@$1@#�m@#dZ@"�\@"n�@"M�@"�@!��@!��@!x�@!hs@!X@!G�@!7L@!&�@!�@!�@!%@ �9@ ��@ �@ bN@ 1'@   @�P@|�@l�@l�@\)@l�@\)@\)@��@ff@{@@`B@V@�j@��@z�@�@��@�m@�m@�
@��@��@dZ@C�@o@�!@n�@M�@=q@=q@-@��@��@�@��@��@�7@�7@X@%@��@�9@�u@�@r�@r�@r�@A�@1'@ �@�P@+@�@�y@ȴ@�+@E�@{@�-@O�@�@��@j@�@ƨ@��@C�@�H@�\@=q@�#@��@hs@&�@%@��@�`@��@Ĝ@Ĝ@�9@�u@�u@r�@A�@ �@�;@��@\)@;d@�@��@�y@��@E�@$�@�@�@��@��@��@��@@��@�h@p�@`B@`B@O�@V@�/@�@�D@Z@(�@1@��@dZ@o@
�H@
�H@
�H@
��@
��@
��@
~�@
^5@
M�@
=q@
-@	�@	�^@	��@	hs@	7L@	%@Ĝ@Ĝ@Ĝ@Ĝ@�9@�9@��@�u@�@r�@bN@A�@1'@  @�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BM�B�%B�PB�{B��B��B��B�DBx�Bt�B`BBR�B6FB6FB1'B.B'�B�BJBB��B�B�B�ZB�;B�BŢB�RB�-B�'B��B��B��B��B��B�oB}�B�B}�Bs�BhsB^5BVBL�BB�B?}B;dB6FB0!B"�B�BVB1BB��B�B�ZB��BȴB�9B�B��B�\B�B}�Bt�Bm�B_;BVBN�B:^B(�B�BhBVB	7B
��B
�B
�;B
�
B
ɺB
ÖB
�wB
�dB
�?B
�B
��B
��B
��B
��B
�{B
�{B
�uB
�bB
�7B
{�B
m�B
dZB
S�B
8RB
0!B
/B
-B
&�B
�B
{B
DB	��B	��B	�B	�sB	�HB	�B	��B	ǮB	�}B	�dB	�?B	�B	��B	�uB	�=B	�B	{�B	x�B	x�B	u�B	jB	e`B	aHB	YB	M�B	I�B	F�B	E�B	D�B	A�B	9XB	.B	"�B	{B	\B	PB	
=B	%B	  B��B��B�B�B�B�TB�#B�
B��BǮB��BÖBÖB��B�RB�LB�RB�RB�RB�9B�B��B��B��B��B��B��B��B��B��B��B�{B�oB�bB�\B�VB�7B�+B�%B�B�B�B�B}�B{�Bu�Bu�Bu�Bs�Bp�Bo�Bm�Bk�BjBk�BhsBdZBaHB_;B]/BYBT�BP�BN�BK�BH�BG�BE�BD�BC�BA�B=qB;dB8RB7LB6FB5?B5?B33B2-B1'B/B.B.B-B,B,B+B)�B)�B&�B&�B'�B&�B%�B$�B$�B$�B$�B#�B#�B"�B"�B �B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B!�B%�B'�B(�B)�B+B-B/B1'B33B33B49B49B33B8RB9XB9XB:^B;dB<jB=qB>wB?}B@�BA�BA�BC�BD�BG�BL�BN�BO�BT�BYB^5B]/B]/B_;BaHBbNBdZBffBiyBk�Bl�Bm�Bn�Bn�Bo�Bp�Bq�Br�Br�Bs�Bs�Bu�Bv�Bw�Bx�B{�B{�B|�B}�B~�B� B�B�B�B�B�B�B�+B�+B�1B�1B�1B�1B�7B�7B�=B�PB�\B�oB�{B��B��B��B��B��B��B��B��B��B�B�B�B�B�FB�}BÖBÖBĜBǮBȴBɺB��B��B��B��B��B��B�B�TB�sB�sB�sB�B�B�B�B�B��B��B��B��B��B��B��B��B��B	B	B	B	+B	1B	
=B	PB	\B	hB	hB	oB	uB	{B	�B	�B	�B	�B	"�B	#�B	#�B	$�B	&�B	&�B	(�B	.B	/B	0!B	1'B	2-B	33B	49B	:^B	=qB	?}B	B�B	C�B	D�B	H�B	J�B	L�B	M�B	N�B	N�B	R�B	VB	W
B	W
B	XB	XB	ZB	[#B	]/B	`BB	dZB	e`B	gmB	hsB	iyB	k�B	k�B	m�B	q�B	t�B	v�B	v�B	w�B	{�B	}�B	}�B	� B	�B	�B	�%B	�1B	�7B	�DB	�JB	�JB	�VB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�!B	�'B	�'B	�'B	�'B	�-B	�-B	�3B	�?B	�LB	�RB	�RB	�XB	�^B	�dB	�jB	�wB	��B	ÖB	ÖB	ŢB	ƨB	ƨB	ǮB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�)B	�5B	�;B	�BB	�BB	�BB	�HB	�HB	�NB	�NB	�TB	�TB	�`B	�fB	�fB	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
%B
+B
1B
1B
1B

=B

=B
JB
JB
PB
PB
PB
VB
bB
bB
hB
hB
hB
hB
oB
oB
uB
uB
uB
{B
{B
{B
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
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
#�B
$�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
'�B
(�B
)�B
)�B
+B
+B
+B
+B
,B
-B
.B
.B
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
49B
49B
49B
5?B
6FB
6FB
6FB
6FB
7LB
8RB
8RB
9XB
9XB
9XB
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
:^B
;dB
;dB
;dB
;dB
<jB
<jB
<jB
>wB
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
?}B
?}B
?}B
?}B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
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
G�B
G�B
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
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
O�B
O�B
P�B
Q�B
Q�B
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
XB
XB
XB
XB
XB
XB
YB
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
ZB
[#B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
^5B
^5B
^5B
_;B
_;B
`BB
`BB
`BB
aHB
aHB
aHB
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
n�B
n�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B��B��B��B�B�B�lB��BBQ�B��B�VB�SB��B��B�xB�HB{�ByrBe�BXyB88B7�B2�B1[B,B�B<BB��B��B��B�LB�B��B�1B��B��B��B��B��B�ZB��B��B�gBB��B�Bu�Bi�B_�BWYBM�BCGB@OB<PB7�B1�B$�B)B\B	lB�B��B��B�fB�YB��B��B��B��B��B�?B~�Bu�Bo5B`�BW�BQhB<�B*�B?B BvBDBB
�vB
�vB
ؓB
��B
ĜB
�.B
��B
��B
�WB
�B
�B
��B
��B
��B
��B
�FB
��B
�xB
~(B
o�B
h�B
V�B
9�B
0�B
/�B
.B
(�B
!-B
SB
B	�}B	��B	�GB	�B	�:B	�sB	�oB	�7B	��B	��B	��B	�vB	��B	�B	�xB	�YB	|�B	y�B	zxB	w�B	k�B	f�B	c�B	[WB	OvB	J�B	GB	FtB	F?B	C�B	<B	0UB	%FB	�B	.B	�B	�B	_B	 B��B��B��B�'B�"B�BܬB��B�jBɆB�ABĜB�B��B��B��B��B�$B��B�B�}B��B��B��B��B��B�
B�`B�BB��B�_B�MB�uB�NB��B�B�#B��B�_B�SB�B��B�B�B}qBw2Bv�BwBt�BqABp;BncBl�Bk�Bm�Bj0Be`BbhB`�B_BZ�BVSBR BP�BMBI�BH�BF?BE�BEBC�B?�B<�B9$B8B6�B6+B6FB4B4TB3�B/�B.�B.�B-�B,�B-B,=B+QB+B'�B(>B(�B'�B&LB%zB%�B&B%zB$ZB$tB$&B$�B!�B!�B vB �BVB;B;BVB�B�BjB�BdB�BWBWBWB=BkBkB�B�B�B�BqB�B�BkBWB)BxB�B�B�B�B!�B#B&�B)B)�B+B,WB.�B0�B2|B4B3�B4�B4�B4�B9	B9�B9�B:�B;�B<�B>(B?HB@�BA BBABB[BDMBEmBH�BMjBO�BQBU�BZB^�B]�B]�B_�Ba�Bb�BeBgBi�Bk�Bl�Bm�Bn�Bo BpBp�Bq�Br�Br�BtBtBvFBw2Bx8ByXB|B|B}"B~(B.B�iB�uB�aB�aB�{B��B�mB�zB�zB�fB�fB��B��B��B��B��B��B��B��B��B��B��B�B�B�5B��B�fB�0B�KB�kB��B��B�iB�LB�B��B��B�B��B��B�#B�JB�vB�oB�TB�oB��B��B�B�B�B��B��B��B�!B�B�3B�ZB�ZB�zB�2B�*B�B�6B�VB�HB	oB	�B	�B	zB	�B	
�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	 B	#B	$B	$&B	%,B	'B	'8B	)yB	.cB	/iB	0UB	1[B	2|B	3�B	4�B	:�B	=�B	@ B	B�B	C�B	EB	IB	J�B	MB	NB	OBB	O�B	S[B	V9B	WYB	W?B	XEB	X�B	ZkB	[qB	]~B	`�B	d�B	e�B	g�B	h�B	i�B	k�B	k�B	m�B	q�B	uB	v�B	v�B	xRB	|6B	~BB	~BB	�iB	�uB	��B	��B	��B	��B	�xB	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	��B	�-B	�,B	�RB	�DB	�KB	�WB	�IB	�;B	�UB	�;B	�AB	�vB	�[B	�[B	�aB	�aB	��B	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�.B	�B	� B	�&B	�2B	�MB	�YB	�eB	�]B	�jB	ߊB	�\B	�\B	�vB	�B	�|B	�hB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�!B	�B	��B	��B	�B	�B	��B	�B	�B	��B	��B	��B	��B	�	B	��B	�	B	�B	�B	�<B	�(B
 4B
[B
GB
GB
aB
gB
gB
mB
YB
tB
zB
�B
�B
�B

rB

�B
�B
~B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
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
 B
 B
!B
!B
"B
# B
$&B
%B
%B
%B
&B
&2B
'8B
'B
'RB
(>B
)DB
*0B
*0B
+B
+6B
+6B
+QB
,WB
-]B
.IB
.IB
.IB
.IB
.cB
/iB
/OB
/OB
0UB
0UB
0UB
0UB
0oB
1vB
2aB
2|B
3�B
3�B
4TB
4nB
4nB
5tB
6zB
6zB
6zB
6zB
7�B
8�B
8�B
9�B
9�B
9rB
9�B
9�B
9�B
9�B
:�B
:�B
:xB
:xB
:xB
:�B
:�B
:�B
;�B
;�B
;�B
;�B
<�B
<�B
<�B
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
?�B
?�B
?�B
?�B
?�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
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
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
IB
I�B
I�B
I�B
J	B
J	B
KB
KB
J�B
J�B
KB
KB
LB
MB
MB
MB
MB
NB
NB
M�B
M�B
M�B
M�B
M�B
M�B
M�B
OB
O(B
OB
O(B
OB
OB
OB
PB
O�B
O�B
QB
Q B
Q B
Q B
PB
PHB
QNB
R B
R:B
S&B
S@B
T,B
T,B
TFB
T,B
U2B
UB
U2B
U2B
V9B
V9B
V9B
V9B
VSB
VSB
WYB
W?B
W$B
XEB
X+B
XEB
X+B
X+B
XEB
XEB
XEB
X+B
X_B
YKB
YKB
YKB
Z7B
Z7B
Z7B
Z7B
Z7B
ZQB
ZQB
ZQB
ZkB
[WB
\]B
\]B
\]B
\]B
\]B
]~B
]dB
]dB
^jB
^jB
^�B
_pB
_pB
`vB
`vB
`�B
a�B
a|B
a|B
b�B
b�B
c�B
c�B
cnB
c�B
cnB
cnB
cnB
cnB
c�B
dtB
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
j�B
j�B
j�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
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
n�B
n�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.23(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201606270033422016062700334220160627003342201806221209582018062212095820180622120958201804050402132018040504021320180405040213  JA  ARFMdecpA19c                                                                20160624183526  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160624094833  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160624094833  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160624094834  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160624094834  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160624094834  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160624094834  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160624094834  QCP$                G�O�G�O�G�O�            FB40JA      jafc1.0                                                                 20160624094835                      G�O�G�O�G�O�                JA  ARUP                                                                        20160624102548                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160623153559  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20160626153342  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160626153342  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404190213  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622030958  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115101518                      G�O�G�O�G�O�                