CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:08:55Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         ZPRIMARY | https://orcid.org/0000-0001-7324-3159 | Matthew Alkire, University of Washington        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7(   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7,   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    70   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7@   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7P   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7`   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7h   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8    DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8$   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8D   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8H   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8L   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8l   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
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
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �0   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �@   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �D   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �T   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �X   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �\   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �`Argo profile    3.1 1.2 19500101000000  20200619170855  20220204114412  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @؇�33B61   @؇��l& @7���v��c�\(�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`ffBh  Bp  Bx  B�  B�  B�  B���B���B���B���B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ�CL  CN  CP  CR  CT  CV  CX  CZ  C\  C]�fC`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*y�D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4fD4� D5  D5� D6  D6� D7  D7� D7��D8� D9  D9� D:  D:� D;  D;�fD<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DNfDN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY�fDZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Dr��Ds� Dt  Dts3Dy��D� �D�[�D���D��D�(�D�c3D��D��\D��D�U�D���D��)D�${D�Q�DڐRD��D�=D�Q�D�{D��H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�p�@���@���AQ�A<Q�A\Q�A|Q�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B{B{B{B{B'{B/{B7{B?{BG{BO{BWz�B_z�Bg{Bo{Bw{B{B��=B��=B�#�B�W
B�#�B�W
B��=B��pB��=B��=B��=B��=B��=B��=B��=B��=BÊ=BǊ=Bˊ=Bϊ=Bӊ=B׊=Bۊ=Bߊ=B�=B�=B�=B�=B�=B��=B��=B��=C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI޹CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]��C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D qHD �HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HD	qHD	�HD
qHD
�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HD qHD �HD!qHD!�HD"qHD"�HD#qHD#�HD$qHD$�HD%qHD%�HD&qHD&�HD'qHD'�HD(qHD(�HD)qHD)�HD*j�D*�HD+qHD+�HD,qHD,�HD-qHD-�HD.qHD.�HD/qHD/�HD0qHD0�HD1qHD1�HD2qHD2�HD3qHD3��D4qHD4�HD5qHD5�HD6qHD6�HD7qHD7��D8qHD8�HD9qHD9�HD:qHD:�HD;w�D;�HD<qHD<�HD=qHD=�HD>qHD>�HD?qHD?�HD@qHD@�HDAqHDA�HDBqHDB�HDCqHDC�HDDqHDD�HDEqHDE�HDFqHDF�HDGqHDG�HDHqHDH�HDIqHDI�HDJqHDJ�HDKqHDK�HDLqHDL�HDMqHDM��DNqHDN�HDOqHDO�HDPqHDP�HDQqHDQ�HDRqHDR�HDSqHDS�HDTqHDT�HDUqHDU�HDVqHDV�HDWqHDW�HDXqHDX�HDYw�DY�HDZqHDZ�HD[qHD[�HD\qHD\�HD]qHD]�HD^qHD^�HD_qHD_�HD`qHD`�HDaqHDa�HDbqHDb�HDcqHDc�HDdqHDd�HDeqHDe�HDfqHDf�HDgqHDg�HDhqHDh�HDiqHDi�HDjqHDj�HDkqHDk�HDlqHDl�HDmqHDm�HDnqHDn�HDoqHDo�HDpqHDp�HDqqHDq�HDrqHDr��DsqHDs�HDtd{Dys4D��D�T{D��qD�θD�!�D�[�D���D�� D�3D�NgD��HD���D�D�J>Dڈ�D��RD��D�J�D�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�&�A�1'A�/A�+A�-A�1'A�33A�-A�?}A�=qA�;dA�;dA�1'A�/A�/A�A��A��mA���AԲ-A԰!Aԗ�A�`BA�M�A��`A�ƨA�ȴA��A�A�1A���A�VA���A�$�A� �A�"�A+A� �A���A��A�l�A�v�A���A��A�C�A���A��9A�/A���A��HA�r�A�+A�A��A���A��A�?}A�l�A�n�A�jA�v�A���A�  A�-A��RA�&�A�+A��#A�x�A�"�A��jA�5?A�z�A���A��!A��A���A�A�/A�(�A��PA�;dA��
A�t�A��HA��DA�-A���A�9XA�ZA���A��mA���A���A� �A���A�~�A���A�JA�n�A�1A�hsA�"�A\)A~�/A|E�Az�Ay�Aw%Au
=At  Ar�RAp�Anr�Am��Al�uAjVAgƨAf9XAdJAbJA^�A]dZA\^5A[�AY�AW�AV�uATZAR$�AP�AO�
AN�+AL5?AJ��AI�AGt�AF�DADQ�AB��AA�^A@A??}A>��A>9XA=dZA<�9A<ffA;��A:E�A9p�A7�#A7C�A6jA5�^A533A4A�A3�A1��A1�A0jA/�A/hsA/?}A/%A,�/A+33A)��A)G�A'�A&�jA$��A"��A"�\A"=qA"5?A!�^A �A   A��A�AE�AA�A�A\)A�\A��A�Av�A=qA$�A�-A33A��AS�A��AȴA$�Ax�AC�A�A��A&�AM�A�-A7LA
�jA
 �A	l�A�/A9XAS�A�/A~�AA�A��AVA�FA j@��@���@�Q�@��F@���@��#@�33@�&�@�9X@���@�z�@�
=@�@�bN@�l�@ꗍ@��@�33@�@�7L@��@�n�@�-@��/@�;d@�?}@�V@ܣ�@ٺ^@�@���@�9X@� �@�  @ӶF@��@ҟ�@�$�@���@ёh@�x�@�G�@��`@Ѓ@��
@Ο�@͙�@̋D@ʧ�@ɩ�@�/@���@��@�K�@�o@��@�x�@�7L@ļj@Å@���@�ȴ@�7L@���@��
@��@��@��`@�Ĝ@�j@���@��H@���@�j@���@��!@���@��u@�ƨ@�33@��H@���@�M�@��@���@�7L@��/@��9@��u@��@�z�@�  @��F@���@�l�@�+@��@���@�M�@��@��-@�V@���@��@��@���@�5?@��@��-@��`@�Z@��@���@�=q@���@�G�@��u@��
@���@�l�@�S�@�C�@�
=@�ȴ@���@���@��+@�ff@�-@�{@���@�O�@��/@���@��D@���@�9X@���@��w@�l�@��@�M�@��T@��-@���@��@�@��@�/@���@���@��@�I�@�  @��F@���@��@��P@�\)@�o@��y@��R@��R@��\@�n�@�n�@�ff@�-@�J@��T@��-@�`B@��@�Ĝ@�Z@���@�\)@�S�@�K�@�K�@�C�@�;d@�C�@�;d@�+@��@��@��@��@���@��R@���@���@���@�~�@��@��@�p�@��@��@��j@�bN@�I�@�9X@�(�@�l�@�C�@�\)@�|�@���@�@���@��\@�^5@�-@�J@���@�hs@��@��@���@��@���@�bN@�Q�@�A�@��@��m@��F@�|�@�l�@�"�@��y@���@��\@��+@�v�@�^5@�E�@��@��@�n�@��+@�~�@�n�@�ff@�M�@��@���@�`B@�G�@�&�@���@��j@��@���@��@�A�@�b@��@�K�@��@�o@��y@�ȴ@�n�@�M�@�=q@�{�@�Q�@vh
@m�7@c�@Yԕ@Sj�@L֡@G]�@A�@;J#@5zx@0��@*&�@&�@!+@�,@N<@J@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�&�A�1'A�/A�+A�-A�1'A�33A�-A�?}A�=qA�;dA�;dA�1'A�/A�/A�A��A��mA���AԲ-A԰!Aԗ�A�`BA�M�A��`A�ƨA�ȴA��A�A�1A���A�VA���A�$�A� �A�"�A+A� �A���A��A�l�A�v�A���A��A�C�A���A��9A�/A���A��HA�r�A�+A�A��A���A��A�?}A�l�A�n�A�jA�v�A���A�  A�-A��RA�&�A�+A��#A�x�A�"�A��jA�5?A�z�A���A��!A��A���A�A�/A�(�A��PA�;dA��
A�t�A��HA��DA�-A���A�9XA�ZA���A��mA���A���A� �A���A�~�A���A�JA�n�A�1A�hsA�"�A\)A~�/A|E�Az�Ay�Aw%Au
=At  Ar�RAp�Anr�Am��Al�uAjVAgƨAf9XAdJAbJA^�A]dZA\^5A[�AY�AW�AV�uATZAR$�AP�AO�
AN�+AL5?AJ��AI�AGt�AF�DADQ�AB��AA�^A@A??}A>��A>9XA=dZA<�9A<ffA;��A:E�A9p�A7�#A7C�A6jA5�^A533A4A�A3�A1��A1�A0jA/�A/hsA/?}A/%A,�/A+33A)��A)G�A'�A&�jA$��A"��A"�\A"=qA"5?A!�^A �A   A��A�AE�AA�A�A\)A�\A��A�Av�A=qA$�A�-A33A��AS�A��AȴA$�Ax�AC�A�A��A&�AM�A�-A7LA
�jA
 �A	l�A�/A9XAS�A�/A~�AA�A��AVA�FA j@��@���@�Q�@��F@���@��#@�33@�&�@�9X@���@�z�@�
=@�@�bN@�l�@ꗍ@��@�33@�@�7L@��@�n�@�-@��/@�;d@�?}@�V@ܣ�@ٺ^@�@���@�9X@� �@�  @ӶF@��@ҟ�@�$�@���@ёh@�x�@�G�@��`@Ѓ@��
@Ο�@͙�@̋D@ʧ�@ɩ�@�/@���@��@�K�@�o@��@�x�@�7L@ļj@Å@���@�ȴ@�7L@���@��
@��@��@��`@�Ĝ@�j@���@��H@���@�j@���@��!@���@��u@�ƨ@�33@��H@���@�M�@��@���@�7L@��/@��9@��u@��@�z�@�  @��F@���@�l�@�+@��@���@�M�@��@��-@�V@���@��@��@���@�5?@��@��-@��`@�Z@��@���@�=q@���@�G�@��u@��
@���@�l�@�S�@�C�@�
=@�ȴ@���@���@��+@�ff@�-@�{@���@�O�@��/@���@��D@���@�9X@���@��w@�l�@��@�M�@��T@��-@���@��@�@��@�/@���@���@��@�I�@�  @��F@���@��@��P@�\)@�o@��y@��R@��R@��\@�n�@�n�@�ff@�-@�J@��T@��-@�`B@��@�Ĝ@�Z@���@�\)@�S�@�K�@�K�@�C�@�;d@�C�@�;d@�+@��@��@��@��@���@��R@���@���@���@�~�@��@��@�p�@��@��@��j@�bN@�I�@�9X@�(�@�l�@�C�@�\)@�|�@���@�@���@��\@�^5@�-@�J@���@�hs@��@��@���@��@���@�bN@�Q�@�A�@��@��m@��F@�|�@�l�@�"�@��y@���@��\@��+@�v�@�^5@�E�@��@��@�n�@��+@�~�@�n�@�ff@�M�@��@���@�`B@�G�@�&�@���@��j@��@���@��@�A�@�b@��@�K�@��@�o@��y@�ȴ@�n�@�M�G�O�@�{�@�Q�@vh
@m�7@c�@Yԕ@Sj�@L֡@G]�@A�@;J#@5zx@0��@*&�@&�@!+@�,@N<@J@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�-B�-B�3B�-B�-B�-B�3B�3B�3B�-B�3B�9B�FB�LB�LB�qB�wB�}B��BŢBƨBɺB��B��B�B��B��B�B �B,B49B:^B)�B/B6FB=qB]/Bu�B�B�PB�{B�uB��B�{B�{B�VB�JB�=B�7B�B� B|�Bz�Bp�BjBffB`BBYBM�B=qB2-B"�B�BDBB��B�sB��BĜB�}B�XB�B��B��B��B�VB{�Bl�BXBE�B=qB7LB1'B)�B �B�BoBDBB
��B
�B
�/B
��B
ŢB
�jB
�B
�{B
}�B
n�B
_;B
YB
D�B
+B
"�B
�B
uB
B	��B	�yB	�B	��B	ĜB	�RB	��B	��B	�hB	{�B	iyB	\)B	M�B	A�B	#�B	�B	\B	+B��B�B�`B�BɺBB�}B�XB�9B�B�B��B��B��B��B��B��B��B��B��BȴBŢBĜBǮBȴBŢBB�}B�qB�RB�'B�B��B��B��B��B��B��B�{B�oB�=B}�Bx�Bu�Bq�Bl�BiyBbNBaHBbNBiyBgmBe`B_;BbNBk�Bm�Br�Bn�BbNBiyBk�Bl�Bk�Bk�BjBjBhsBffBe`B`BB\)B[#BZBZBYBW
BM�BH�BI�BH�BF�BE�BC�BC�BC�BE�BC�BB�BA�BA�B?}B>wB=qB<jB9XB9XB8RB8RB8RB7LB;dB9XB8RB:^B9XB7LB9XB8RB7LB6FB7LB6FB6FB6FB6FB6FB:^B?}B?}B@�BA�BA�BC�BB�BB�BA�BA�BA�BB�BD�BE�BG�BH�BI�BI�BJ�BK�BM�BO�BQ�BQ�BR�BR�BP�BP�BO�BP�BR�BT�BW
BYBXBYBZB\)B^5BaHB`BBffBhsBm�Bo�Bp�Bq�Bs�Bt�Bv�Bx�By�B{�B|�B�B�B�B�%B�%B�+B�1B�=B�DB�DB�JB�JB�JB�DB�PB�VB�VB�VB�\B�\B�\B�bB�hB�uB��B��B��B��B��B��B��B��B��B��B�!B�-B�9B�FB�^B�qB��B��BBÖBÖBƨBȴB��B��B��B��B�B�B�;B�NB�fB�mB�yB�B�B��B��B��B��B��B	B	B	PB	hB	oB	�B	�B	 �B	#�B	&�B	)�B	-B	0!B	33B	8RB	:^B	>wB	@�B	@�B	@�B	@�B	A�B	A�B	D�B	H�B	J�B	J�B	K�B	L�B	M�B	O�B	T�B	ZB	]/B	_;B	aHB	cTB	dZB	e`B	ffB	ffB	gmB	hsB	hsB	iyB	iyB	m�B	o�B	q�B	s�B	s�B	t�B	v�B	x�B	x�B	}�B	�B	�B	�B	�%B	�+B	�+B	�7B	�JB	�VB	�\B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�-B	�3B	�?B	�LB	�LB	�LB	�RB	�RB	�dB	�qB	��B	ÖB	ĜB	ŢB	ŢB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�B	�\B	�nB
�B
�B
�B
#�B
,WB
3B
9$B
?�B
E9B
J�B
O�B
TFB
XyB
_VB
e`B
jB
n/B
q�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B�CB�CB�IB�CB�CB�CB�IB�IB�IB�CB�IB�NB�[B�aB�aB��B��B��B��B��B��B��B� B�B�B��B�B�B�B#B+FB1kB!B&*B-UB4�BT=Bl�B|,B�]B��B��B��B��B��B�eB�YB�LB�GByBwBs�Bq�Bg�Ba�B]yBWVBP,BD�B4�B)EB�B�B_B�(B��BߑB��B��B��B�zB�6B��B��B��B�{BsBc�BO:B<�B4�B.xB(TB!)B�B�B	�BtB
�BB
��B
�B
�bB
�B
��B
��B
�EB
��B
u/B
e�B
VyB
PUB
;�B
"DB
B
�B

�B	�QB	�'B	��B	�fB	�.B	��B	��B	�B	��B	��B	s7B	`�B	S|B	E'B	8�B	/B	�B	�B��B�IB� BܽB�{B�B��B��B��B��B�}B�vB�LB�FB�AB�TB�(B�@B�5B�/B�#B�B�B��B�B�B�B��B��B��B��B��B��B�\B�$B�B�B��B��B��B��B��Bu^Bp?Bm-BiBc�B`�BY�BX�BY�B`�B^�B\�BV�BY�Bb�Bd�BjBfBY�B`�Bb�Bc�Bb�Bb�Ba�Ba�B_�B]�B\�BW�BS�BR�BQ�BQ�BP�BNzBEDB@&BA,B@&B>B=B;	B;	B;	B=B;	B:B8�B8�B6�B5�B4�B3�B0�B0�B/�B/�B/�B.�B2�B0�B/�B1�B0�B.�B0�B/�B.�B-�B.�B-�B-�B-�B-�B-�B1�B6�B6�B7�B9 B9 B;B:B:B9B9B9B:B<B=B?%B@+BA1BA1BB8BC>BEJBGVBIcBIcBJiBJiBH\BH]BGWBH]BJjBLuBN�BP�BO�BP�BQ�BS�BU�BX�BW�B]�B_�BeBgBhBi!Bk-Bl3Bn@BpLBqRBs^BteBx|B{�B|�B}�B}�B~�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�3B�:B�FB�LB�LB�^B�kB��B��B��B��B��B��B��B��B�B�	B�	B�B�'B�4B�:B�@B�XBЉBяB֭B��B��B��B��B�	B�'B�3B�9B�EB�WB�jB�|B��B	�B	�B		�B	�B	&B	3B	EB	VB	!iB	${B	'�B	*�B	/�B	1�B	5�B	7�B	7�B	7�B	7�B	8�B	8�B	<B	@B	B,B	B,B	C2B	D8B	E>B	GJB	LiB	Q�B	T�B	V�B	X�B	Z�B	[�B	\�B	]�B	]�B	^�B	_�B	_�B	`�B	`�B	d�B	gB	iB	kB	kB	l%B	n2B	p>B	p>B	u\B	{�B	|�B	|�B	}�B	~�B	~�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�+B	�7B	�=B	�IB	�OB	�VB	�bB	�nB	�nB	�tB	�yB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�%B	�+B	�1B	�7B	�<B	�<B	�<B	�<B	�BB	�OB	�UB	�[B	�gB	�mB	�mB	�sB	�sB	�zB	�zG�O�B	׿B	��B	�+B
B
�B
�B
#�B
*xB
0�B
7B
<�B
BSB
GWB
K�B
O�B
V�B
\�B
a�B
e�B
i;B
mT11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.23 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.009(+/-0.003) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144122022020411441220220204114412  AO  ARCAADJP                                                                    20200619170855    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170855  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170855  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114412  IP                  G�O�G�O�G�O�                