CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-06-22T09:00:50Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         C   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    9   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    9    HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    9$   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    9(   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    98   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    9H   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    9X   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  9`   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  9�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  @  9�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        :    	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    :$   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    :(   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     :,   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    :L   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    :P   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     :T   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     :t   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     :�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    :�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           :�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    :�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            :�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           :�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           :�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    :�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    :�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    :�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        ;�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_NB_SAMPLE_CTD_QC               	long_name         ,Global quality flag of NB_SAMPLE_CTD profile   conventions       Argo reference table 2a    
_FillValue                    ;�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  O�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  _0   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  c   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  r�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �d   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �P   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ̸   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Ф   NB_SAMPLE_CTD            
         	long_name         2Number of samples in each pressure bin for the CTD     
_FillValue        �     units         count      C_format      %5d    FORTRAN_format        I5     
resolution                �  �H   NB_SAMPLE_CTD_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  @  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �H   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �H   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �H   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  8  �H   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  20230622090050  20230622090050  5906369 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            NB_SAMPLE_CTD      QA   AO  8344                            2B  A   NAVIS_A                         1262                            170425                          863 @�4Ѿcfb1   @�4�`�V@9������cj~��#1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         QA   A   A       @���@�  A��A!��A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:�C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D ��D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR�fDSfDS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}�fD~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�3D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�p�@���@��A�A<Q�A\Q�A|Q�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B{B{B{B{B'{B/{B7{B?{BG{BO{BW{B_{Bg{Bo{Bw{B{B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=BÊ=BǊ=Bˊ=Bϊ=Bӊ=B׊=Bۊ=Bߊ=B�=B�=B�=B�=B�=B��=B��=B��=C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9޹C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D qHD �HDqHD�HDqHD�HDqHD�HDj�D�HDqHD�HDqHD�HDqHD�HDqHD�HD	qHD	�HD
qHD
�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HD qHD ��D!qHD!�HD"qHD"�HD#qHD#�HD$qHD$�HD%qHD%�HD&qHD&�HD'qHD'�HD(qHD(�HD)qHD)�HD*qHD*�HD+qHD+�HD,qHD,�HD-qHD-�HD.qHD.�HD/qHD/�HD0qHD0�HD1qHD1�HD2qHD2�HD3qHD3�HD4qHD4�HD5qHD5�HD6qHD6�HD7qHD7�HD8qHD8�HD9qHD9�HD:qHD:�HD;qHD;�HD<qHD<�HD=qHD=�HD>qHD>�HD?qHD?�HD@qHD@�HDAqHDA�HDBqHDB�HDCqHDC�HDDqHDD�HDEqHDE�HDFqHDF�HDGqHDG�HDHqHDH�HDIqHDI�HDJqHDJ�HDKqHDK�HDLqHDL�HDMqHDM�HDNqHDN�HDOqHDO�HDPqHDP�HDQqHDQ�HDRw�DR��DSqHDS�HDTqHDT�HDUqHDU�HDVqHDV�HDWqHDW�HDXqHDX�HDYqHDY�HDZqHDZ�HD[qHD[�HD\qHD\�HD]qHD]�HD^qHD^�HD_qHD_�HD`qHD`�HDaqHDa�HDbqHDb�HDcqHDc�HDdqHDd�HDeqHDe�HDfqHDf�HDgqHDg�HDhqHDh�HDiqHDi�HDjqHDj�HDkqHDk�HDlqHDl�HDmqHDm�HDnqHDn�HDoqHDo�HDpqHDp�HDqqHDq�HDrqHDr�HDsqHDs�HDtqHDt�HDuqHDu�HDvqHDv�HDwqHDw�HDxqHDx�HDyqHDy�HDzqHDz�HD{qHD{�HD|qHD|�HD}w�D}�HD~qHD~�HDqHD�HD�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�;�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�{�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D¸�D���D�8�D�x�Dø�D���D�8�D�x�Dĸ�D���D�8�D�x�DŸ�D���D�8�D�x�DƸ�D���D�8�D�x�DǸ�D���D�8�D�x�Dȸ�D���D�8�D�x�Dɸ�D���D�8�D�x�Dʸ�D���D�8�D�x�D˸�D���D�8�D�x�D̸�D���D�8�D�x�D͸�D���D�8�D�x�Dθ�D���D�8�D�x�Dϸ�D���D�8�D�x�Dи�D���D�8�D�x�DѸ�D���D�8�D�x�DҸ�D���D�8�D�x�DӸ�D���D�8�D�x�DԸ�D���D�8�D�x�Dո�D���D�8�D�x�Dָ�D���D�8�D�x�D׸�D���D�8�D�x�Dظ�D���D�8�D�x�Dٸ�D���D�8�D�x�Dڸ�D���D�8�D�x�D۸�D���D�8�D�x�Dܸ�D���D�8�D�x�Dݸ�D���D�8�D�x�D޸�D���D�8�D�x�D߸�D���D�8�D�x�DฤD���D�8�D�x�DḤD���D�8�D�x�D⸤D���D�8�D�x�D㸤D���D�8�D�x�D两D���D�8�D�x�D帤D���D�8�D�x�D渤D���D�8�D�x�D縤D���D�8�D�x�D踤D���D�8�D�x�D鸤D���D�8�D�x�D긤D���D�8�D�x�D븤D���D�8�D�x�D츤D���D�8�D�x�D���D���D�8�D�x�DD���D�8�D�x�D︤D��qD�8�D�x�D�D���D�8�D�x�D�D���D�8�D�x�D�D���D�8�D�x�D�D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�oA�oA�A�A�A�JA��A��A��A��A��A��A���A�=qA�jA�ĜA� �A���A�bA���A���A��\A�M�A�A�p�A��RA�$�A���A���A��A���A��RA�/A��!A�7LA��uA���A��HA�VA���A�9XA��!A��`A��A�r�A��A�K�A�jA�$�A�ffA��A�A��jA���A�9XA���A�^5A�{A���A�ĜA��jA� �A�A���A�p�A��TA���A�A�;dA�t�A�+A��TA��PA���A�^5A���A�9XA��A��\A���A�/A��uA��A��#A�ZA���A�ĜA��;A}ƨAy�TAv��Av=qAu��As?}Ap5?Al�`Al �Aj�\Ah�jAg�Ag��AfM�Ad�/Ad1'AaA]��A\�!AY�AY��AW�;AV  AT  AShsAR�uAQVAO\)AM��AL��ALJAK;dAI�AH�yAG��AGC�AF��AE/AD�yAC��ABE�A?�wA=�FA<�yA<A;��A<1A<bA;+A8�/A81A7l�A7A5�;A3oA2-A1x�A0�+A/dZA.-A-��A-33A,��A,�\A,�A*~�A(��A'�A&��A&E�A$��A#G�A"M�A!�A ��A 1'AhsA��A�A�A-A��A�FA+Al�AffA�#A��A�\A��A/A�AĜAA&�A�`AA�AI�A�A�A^5A��A?}A
��A
$�A	ƨA	O�A	�A(�A�HA9XA��A��A��A~�A �A�^AhsA�\A?}A bN@��-@��@���@��9@���@�7L@�j@���@�@��@�7@�9@�(�@��@�|�@���@�Z@�C�@�\@�7@�@�\@�h@߶F@�
=@�=q@���@��H@���@ؼj@ָR@�7L@�Q�@�=q@�A�@ϥ�@�o@���@��/@̋D@�  @�K�@��@�E�@ȓu@�J@��#@��#@ř�@�j@�ƨ@�l�@��@�ff@��h@��@��
@�;d@��@�~�@�$�@��h@�G�@��@�bN@��@�^5@�J@���@�;d@�5?@�x�@�  @��@��^@�Ĝ@�Q�@��;@�\)@���@�V@��@��@�/@��@��@�33@���@��+@�=q@���@�j@��m@���@�"�@�{@��7@��@���@��;@�S�@���@�V@�{@���@�Z@��j@���@�j@��@�9X@��P@�ȴ@��\@�n�@�=q@��@��T@�@��-@��7@�&�@�Ĝ@�z�@�b@�ƨ@���@�
=@�ff@�@���@��-@���@�7L@��u@�(�@��
@�t�@�+@���@�^5@�$�@�@��T@�@��@���@�j@�1@���@�;d@�@�p�@�`B@��@��@�%@��@�1'@��m@�ƨ@��F@���@�C�@�+@�o@���@�ȴ@�ȴ@���@���@�M�@�J@��@���@�G�@��@��/@��D@�Q�@�(�@�b@�  @��;@�ƨ@�ƨ@��w@�ƨ@�ƨ@�ƨ@��F@���@���@�|�@�l�@�dZ@�^5@��@��T@��#@��^@�hs@���@��D@�b@��y@�ff@��\@���@���@�t�@�dZ@�\)@�33@��@��H@���@�v�@�J@��T@���@��T@��#@��#@���@�X@��/@��@�z�@�9X@��@��P@�S�@��@�ȴ@�5?@��@�E�@�$�@�5?@�ff@�V@�E�@�-@��@���@�?}@�%@��/@��/@��@���@���@�bN@�1'@�@~�@~{@~@}��@|�@|(�@{��@{dZ@{"�@zn�@y��@x  @w�@wl�@v��@v��@vff@vV@v5?@u��@uO�@t��@t1@s�
@sƨ@s�@sS�@so@r��@r�!@r^5@q��@q��@qx�@qX@q&�@q�@q%@q%@p��@p�`@p�9@p�u@pr�@p �@o�;@o�P@o;d@o�@o
=@n��@n�R@n@m�-@m�h@mp�@m?}@l�@lz�@lZ@k�
@k��@k��@k�@kt�@kC�@j��@j�\@j�@i��@h�9@h �@gl�@gl�@g+@f�R@f�+@fff@e�@e�@dj@c��@cC�@co@b��@bM�@a�#@a��@ahs@aG�@a��@a%@`r�@_|�@^��@^E�@]�@]�-@]?}@]�@]�@\�j@\�D@\�@[��@[t�@[t�@[C�@["�@Z�@Z��@Z^5@ZM�@Y��@Yhs@Y�@X�9@Xb@W�@W|�@WK�@W�@V��@V�R@V5?@U��@U��@U�h@UO�@T�@T�j@Tz�@TI�@T(�@T1@S�
@S��@SC�@S@R�!@R~�@Q��@Q�#@Q��@QX@Q7L@Q%@P��@P��@P�9@PA�@P  @O�@O��@O|�@O|�@Ol�@O;d@N��@Nv�@Nff@NV@N5?@N$�@N@M�@M�T@M�h@Mp�@M?}@M/@L��@Lj@K�m@K�@Ko@J��@J��@J�@I��@I��@I�#@I��@I��@I7L@I&�@I�@H��@H��@HA�@G�;@G�P@G\)@G�@F�R@F�+@Fff@F@Ep�@E?}@E�@EV@EV@EV@D��@D��@DI�@C��@C�m@C�
@C�@CS�@CC�@C33@B�H@B�!@BM�@BM�@A��@Ax�@A7L@A7L@@��@@Ĝ@@r�@@Q�@@  @?��@?��@?\)@?
=@>�@>�R@>v�@>V@>{@=�@=�-@=O�@<�@<Z@<9X@;��@;ƨ@;��@;dZ@;C�@;@:��@:^5@:J@9��@9�7@9�7@9x�@9X@9�@8�`@8Ĝ@8�9@8�u@8r�@8bN@81'@7��@7�P@7|�@7l�@7
=@6v�@65?@6@5�T@5�-@5p�@5O�@5?}@5V@4z�@49X@4�@3��@3ƨ@3��@3dZ@2�@2��@2-@1��@2J@1��@1��@1��@1%@0bN@0  @/��@/�P@/\)@/+@/
=@.�R@.��@.��@.v�@.{@-O�@-/@-V@,�/@,�@,z�@,I�@+�
@+��@+S�@+33@+@*��@*^5@*=q@*J@)�@)�#@)�^@)x�@)G�@(��@(��@(��@(A�@'�@'�w@'|�@'K�@'K�@'
=@&��@&V@&@%�h@%?}@%�@$��@$�/@$�/@$��@$��@$j@$Z@$9X@#�m@#��@#"�@"�@"��@"��@"n�@"=q@"-@!�#@!��@!X@!7L@ �`@ �9@ �@ 1'@ b@�;@��@�@|�@\)@
=@�@��@v�@5?@�@�-@O�@/@�@�@�/@�@z�@Z@(�@��@ƨ@��@��@��@t�@"�@��@�\@M�@M�@=q@�@��@��@X@&�@�@%@��@�u@�@Q�@�@|�@\)@\)@;d@�@�@�y@v�@E�@@��@V@�@z�@Z@9X@1@�
@ƨ@��@�@t�@S�@S�@"�@o@�@�!@�\@~�@~�@~�@M�@J@�@��@��@�7@hs@X@&�@&�@&�@&�@%@�`@Ĝ@��@�@r�@bN@ �@�@��@�w@|�@;d@�@
=@�@��@��@v�@V@E�@5?@@��@��@p�@O�@�@V@�@�j@�@��@�D@j@I�@9X@(�@�@�
@C�@@
�H@
��@
��@
^5@
M�@
M�@
M�@
M�@
=q@
=q@
=q@
�@	�@	��@	X@	&�@	%@��@�9@�@Q�@Q�@1'@b@  @�@�P@+@�y@�R@�+@E�@��@��@p�@?}11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�oA�oA�A�A�A�JA��A��A��A��A��A��A���A�=qA�jA�ĜA� �A���A�bA���A���A��\A�M�A�A�p�A��RA�$�A���A���A��A���A��RA�/A��!A�7LA��uA���A��HA�VA���A�9XA��!A��`A��A�r�A��A�K�A�jA�$�A�ffA��A�A��jA���A�9XA���A�^5A�{A���A�ĜA��jA� �A�A���A�p�A��TA���A�A�;dA�t�A�+A��TA��PA���A�^5A���A�9XA��A��\A���A�/A��uA��A��#A�ZA���A�ĜA��;A}ƨAy�TAv��Av=qAu��As?}Ap5?Al�`Al �Aj�\Ah�jAg�Ag��AfM�Ad�/Ad1'AaA]��A\�!AY�AY��AW�;AV  AT  AShsAR�uAQVAO\)AM��AL��ALJAK;dAI�AH�yAG��AGC�AF��AE/AD�yAC��ABE�A?�wA=�FA<�yA<A;��A<1A<bA;+A8�/A81A7l�A7A5�;A3oA2-A1x�A0�+A/dZA.-A-��A-33A,��A,�\A,�A*~�A(��A'�A&��A&E�A$��A#G�A"M�A!�A ��A 1'AhsA��A�A�A-A��A�FA+Al�AffA�#A��A�\A��A/A�AĜAA&�A�`AA�AI�A�A�A^5A��A?}A
��A
$�A	ƨA	O�A	�A(�A�HA9XA��A��A��A~�A �A�^AhsA�\A?}A bN@��-@��@���@��9@���@�7L@�j@���@�@��@�7@�9@�(�@��@�|�@���@�Z@�C�@�\@�7@�@�\@�h@߶F@�
=@�=q@���@��H@���@ؼj@ָR@�7L@�Q�@�=q@�A�@ϥ�@�o@���@��/@̋D@�  @�K�@��@�E�@ȓu@�J@��#@��#@ř�@�j@�ƨ@�l�@��@�ff@��h@��@��
@�;d@��@�~�@�$�@��h@�G�@��@�bN@��@�^5@�J@���@�;d@�5?@�x�@�  @��@��^@�Ĝ@�Q�@��;@�\)@���@�V@��@��@�/@��@��@�33@���@��+@�=q@���@�j@��m@���@�"�@�{@��7@��@���@��;@�S�@���@�V@�{@���@�Z@��j@���@�j@��@�9X@��P@�ȴ@��\@�n�@�=q@��@��T@�@��-@��7@�&�@�Ĝ@�z�@�b@�ƨ@���@�
=@�ff@�@���@��-@���@�7L@��u@�(�@��
@�t�@�+@���@�^5@�$�@�@��T@�@��@���@�j@�1@���@�;d@�@�p�@�`B@��@��@�%@��@�1'@��m@�ƨ@��F@���@�C�@�+@�o@���@�ȴ@�ȴ@���@���@�M�@�J@��@���@�G�@��@��/@��D@�Q�@�(�@�b@�  @��;@�ƨ@�ƨ@��w@�ƨ@�ƨ@�ƨ@��F@���@���@�|�@�l�@�dZ@�^5@��@��T@��#@��^@�hs@���@��D@�b@��y@�ff@��\@���@���@�t�@�dZ@�\)@�33@��@��H@���@�v�@�J@��T@���@��T@��#@��#@���@�X@��/@��@�z�@�9X@��@��P@�S�@��@�ȴ@�5?@��@�E�@�$�@�5?@�ff@�V@�E�@�-@��@���@�?}@�%@��/@��/@��@���@���@�bN@�1'@�@~�@~{@~@}��@|�@|(�@{��@{dZ@{"�@zn�@y��@x  @w�@wl�@v��@v��@vff@vV@v5?@u��@uO�@t��@t1@s�
@sƨ@s�@sS�@so@r��@r�!@r^5@q��@q��@qx�@qX@q&�@q�@q%@q%@p��@p�`@p�9@p�u@pr�@p �@o�;@o�P@o;d@o�@o
=@n��@n�R@n@m�-@m�h@mp�@m?}@l�@lz�@lZ@k�
@k��@k��@k�@kt�@kC�@j��@j�\@j�@i��@h�9@h �@gl�@gl�@g+@f�R@f�+@fff@e�@e�@dj@c��@cC�@co@b��@bM�@a�#@a��@ahs@aG�@a��@a%@`r�@_|�@^��@^E�@]�@]�-@]?}@]�@]�@\�j@\�D@\�@[��@[t�@[t�@[C�@["�@Z�@Z��@Z^5@ZM�@Y��@Yhs@Y�@X�9@Xb@W�@W|�@WK�@W�@V��@V�R@V5?@U��@U��@U�h@UO�@T�@T�j@Tz�@TI�@T(�@T1@S�
@S��@SC�@S@R�!@R~�@Q��@Q�#@Q��@QX@Q7L@Q%@P��@P��@P�9@PA�@P  @O�@O��@O|�@O|�@Ol�@O;d@N��@Nv�@Nff@NV@N5?@N$�@N@M�@M�T@M�h@Mp�@M?}@M/@L��@Lj@K�m@K�@Ko@J��@J��@J�@I��@I��@I�#@I��@I��@I7L@I&�@I�@H��@H��@HA�@G�;@G�P@G\)@G�@F�R@F�+@Fff@F@Ep�@E?}@E�@EV@EV@EV@D��@D��@DI�@C��@C�m@C�
@C�@CS�@CC�@C33@B�H@B�!@BM�@BM�@A��@Ax�@A7L@A7L@@��@@Ĝ@@r�@@Q�@@  @?��@?��@?\)@?
=@>�@>�R@>v�@>V@>{@=�@=�-@=O�@<�@<Z@<9X@;��@;ƨ@;��@;dZ@;C�@;@:��@:^5@:J@9��@9�7@9�7@9x�@9X@9�@8�`@8Ĝ@8�9@8�u@8r�@8bN@81'@7��@7�P@7|�@7l�@7
=@6v�@65?@6@5�T@5�-@5p�@5O�@5?}@5V@4z�@49X@4�@3��@3ƨ@3��@3dZ@2�@2��@2-@1��@2J@1��@1��@1��@1%@0bN@0  @/��@/�P@/\)@/+@/
=@.�R@.��@.��@.v�@.{@-O�@-/@-V@,�/@,�@,z�@,I�@+�
@+��@+S�@+33@+@*��@*^5@*=q@*J@)�@)�#@)�^@)x�@)G�@(��@(��@(��@(A�@'�@'�w@'|�@'K�@'K�@'
=@&��@&V@&@%�h@%?}@%�@$��@$�/@$�/@$��@$��@$j@$Z@$9X@#�m@#��@#"�@"�@"��@"��@"n�@"=q@"-@!�#@!��@!X@!7L@ �`@ �9@ �@ 1'@ b@�;@��@�@|�@\)@
=@�@��@v�@5?@�@�-@O�@/@�@�@�/@�@z�@Z@(�@��@ƨ@��@��@��@t�@"�@��@�\@M�@M�@=q@�@��@��@X@&�@�@%@��@�u@�@Q�@�@|�@\)@\)@;d@�@�@�y@v�@E�@@��@V@�@z�@Z@9X@1@�
@ƨ@��@�@t�@S�@S�@"�@o@�@�!@�\@~�@~�@~�@M�@J@�@��@��@�7@hs@X@&�@&�@&�@&�@%@�`@Ĝ@��@�@r�@bN@ �@�@��@�w@|�@;d@�@
=@�@��@��@v�@V@E�@5?@@��@��@p�@O�@�@V@�@�j@�@��@�D@j@I�@9X@(�@�@�
@C�@@
�H@
��@
��@
^5@
M�@
M�@
M�@
M�@
=q@
=q@
=q@
�@	�@	��@	X@	&�@	%@��@�9@�@Q�@Q�@1'@b@  @�@�P@+@�y@�R@�+@E�@��@��@p�@?}11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B��B��B��B��B��B��B��B�B��B�B1B%B	7B�B{BuB�B{BoBhB\BJB
=B	7B1BBB  B��B��B��B�B�B�B�NB�BB�B��BĜB�3B��B��B�PBt�Be`BS�BA�B2-B&�B�B\B	7BB��B�BBȴB��B�%Bz�B�B�%Bl�BG�B6FB0!B/B9XB2-B'�B�BB
�fB
�
B
��B
�FB
�B
��B
�B
^5B
F�B
7LB
)�B
�B
  B	�BB	ɺB	��B	�jB	�B	��B	~�B	x�B	p�B	e`B	`BB	]/B	W
B	M�B	G�B	9XB	%�B	�B	PB	+B	B��B�B�B�mB�;B�B��BƨB�}B�XB�-B�B��B��B��B��B�?B�-B��B�{B�DB�7B�DB�uB��B�3B�RB�B��B��B��B��B��B�uB�bB�\B�1B�+B�7B�=B�7B�1B�7B�B� Bw�Bv�Bs�Bn�BdZB_;B^5B[#BZBW
BVBQ�BO�BM�BJ�BH�BG�BF�BB�BA�B?}B=qB=qB;dB:^B9XB9XB7LB5?B49B2-B0!B/B/B.B-B,B+B+B)�B'�B'�B%�B$�B#�B#�B"�B"�B!�B �B�B�B�B�B�B�B�B{BoBoBbBbB\B\BVBPBJBDB
=BDB	7B	7B1B1B1B	7B+B%BBBBB%BBBBBBBBB%B+BPB\BbBbBhB�B�B�B �B!�B!�B"�B"�B#�B$�B#�B%�B(�B+B+B+B-B/B/B0!B1'B5?B6FB6FB9XB=qB>wB@�BD�BF�BI�BL�BN�BQ�BR�BT�BW
BYBZB[#B]/B_;BbNBcTBdZBe`Bk�Bl�Bn�Bo�Bp�Bs�Bu�Bv�By�B}�B�B�%B�=B�PB�JB�DB�uB��B��B��B��B��B��B��B��B�B�B�-B�3B�9B�?B�LB�^B�jB�wBBĜBȴB��B��B��B��B��B��B�B�/B�BB�ZB�fB�B�B�B�B�B��B��B��B	  B	B	+B	1B	JB	PB	\B	hB	hB	oB	�B	�B	�B	�B	�B	 �B	&�B	(�B	+B	,B	.B	.B	0!B	1'B	49B	8RB	9XB	<jB	?}B	@�B	A�B	C�B	E�B	G�B	H�B	I�B	J�B	L�B	L�B	M�B	P�B	R�B	VB	YB	ZB	ZB	ZB	[#B	ZB	`BB	dZB	e`B	ffB	gmB	iyB	jB	k�B	l�B	o�B	r�B	y�B	}�B	�B	�+B	�+B	�+B	�1B	�DB	�DB	�JB	�JB	�PB	�PB	�PB	�bB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�'B	�9B	�?B	�RB	�XB	�dB	�jB	�wB	�}B	�}B	�}B	�}B	��B	��B	��B	��B	��B	��B	B	ĜB	ŢB	ŢB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�B	�#B	�#B	�)B	�/B	�5B	�5B	�5B	�;B	�BB	�BB	�BB	�HB	�HB	�NB	�ZB	�`B	�`B	�fB	�fB	�fB	�fB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
+B
1B
1B
1B
	7B

=B

=B

=B

=B
DB
JB
PB
PB
VB
VB
\B
\B
\B
bB
\B
\B
hB
oB
oB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
+B
+B
+B
+B
+B
,B
,B
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
0!B
1'B
1'B
1'B
1'B
1'B
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
5?B
5?B
5?B
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
9XB
9XB
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
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
?}B
?}B
?}B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
F�B
F�B
F�B
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
G�B
G�B
G�B
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
K�B
L�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
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
P�B
P�B
P�B
Q�B
Q�B
Q�B
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
W
B
XB
XB
XB
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
]/B
^5B
]/B
]/B
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
aHB
aHB
aHB
aHB
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
hsB
gmB
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
q�B
q�B
q�B
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
v�B
v�B
v�B
w�B
w�B
w�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B��B��B��B��B��B��B��B�B��B�B1B%B	7B�B{BuB�B{BoBhB\BJB
=B	7B1BBB  B��B��B��B�B�B�B�NB�BB�B��BĜB�3B��B��B�PBt�Be`BS�BA�B2-B&�B�B\B	7BB��B�BBȴB��B�%Bz�B�B�%Bl�BG�B6FB0!B/B9XB2-B'�B�BB
�fB
�
B
��B
�FB
�B
��B
�B
^5B
F�B
7LB
)�B
�B
  B	�BB	ɺB	��B	�jB	�B	��B	~�B	x�B	p�B	e`B	`BB	]/B	W
B	M�B	G�B	9XB	%�B	�B	PB	+B	B��B�B�B�mB�;B�B��BƨB�}B�XB�-B�B��B��B��B��B�?B�-B��B�{B�DB�7B�DB�uB��B�3B�RB�B��B��B��B��B��B�uB�bB�\B�1B�+B�7B�=B�7B�1B�7B�B� Bw�Bv�Bs�Bn�BdZB_;B^5B[#BZBW
BVBQ�BO�BM�BJ�BH�BG�BF�BB�BA�B?}B=qB=qB;dB:^B9XB9XB7LB5?B49B2-B0!B/B/B.B-B,B+B+B)�B'�B'�B%�B$�B#�B#�B"�B"�B!�B �B�B�B�B�B�B�B�B{BoBoBbBbB\B\BVBPBJBDB
=BDB	7B	7B1B1B1B	7B+B%BBBBB%BBBBBBBBB%B+BPB\BbBbBhB�B�B�B �B!�B!�B"�B"�B#�B$�B#�B%�B(�B+B+B+B-B/B/B0!B1'B5?B6FB6FB9XB=qB>wB@�BD�BF�BI�BL�BN�BQ�BR�BT�BW
BYBZB[#B]/B_;BbNBcTBdZBe`Bk�Bl�Bn�Bo�Bp�Bs�Bu�Bv�By�B}�B�B�%B�=B�PB�JB�DB�uB��B��B��B��B��B��B��B��B�B�B�-B�3B�9B�?B�LB�^B�jB�wBBĜBȴB��B��B��B��B��B��B�B�/B�BB�ZB�fB�B�B�B�B�B��B��B��B	  B	B	+B	1B	JB	PB	\B	hB	hB	oB	�B	�B	�B	�B	�B	 �B	&�B	(�B	+B	,B	.B	.B	0!B	1'B	49B	8RB	9XB	<jB	?}B	@�B	A�B	C�B	E�B	G�B	H�B	I�B	J�B	L�B	L�B	M�B	P�B	R�B	VB	YB	ZB	ZB	ZB	[#B	ZB	`BB	dZB	e`B	ffB	gmB	iyB	jB	k�B	l�B	o�B	r�B	y�B	}�B	�B	�+B	�+B	�+B	�1B	�DB	�DB	�JB	�JB	�PB	�PB	�PB	�bB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�'B	�9B	�?B	�RB	�XB	�dB	�jB	�wB	�}B	�}B	�}B	�}B	��B	��B	��B	��B	��B	��B	B	ĜB	ŢB	ŢB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�B	�#B	�#B	�)B	�/B	�5B	�5B	�5B	�;B	�BB	�BB	�BB	�HB	�HB	�NB	�ZB	�`B	�`B	�fB	�fB	�fB	�fB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
+B
1B
1B
1B
	7B

=B

=B

=B

=B
DB
JB
PB
PB
VB
VB
\B
\B
\B
bB
\B
\B
hB
oB
oB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
+B
+B
+B
+B
+B
,B
,B
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
0!B
1'B
1'B
1'B
1'B
1'B
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
5?B
5?B
5?B
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
9XB
9XB
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
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
?}B
?}B
?}B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
F�B
F�B
F�B
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
G�B
G�B
G�B
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
K�B
L�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
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
P�B
P�B
P�B
Q�B
Q�B
Q�B
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
W
B
XB
XB
XB
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
]/B
^5B
]/B
]/B
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
aHB
aHB
aHB
aHB
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
hsB
gmB
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
q�B
q�B
q�B
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
v�B
v�B
v�B
w�B
w�B
w�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�  % ! 2 0      !  $ "                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           ! ��00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000   PRES            TEMP            PSAL            NB_SAMPLE_CTD   PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.23 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted during real time processing based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       20230622090050                                          AO  ARCAADJP                                                                    20230622090050    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20230622090050  QCP$                G�O�G�O�G�O�1F83E           AO  ARGQQCPL                                                                    20230622090050  QCF$                G�O�G�O�G�O�0               