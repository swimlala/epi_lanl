CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-24T14:01:08Z creation      
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
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �T   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �d   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �h   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �x   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �|   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200624140108  20220204114427  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @�#����1   @�#��n\@6	��l�D�b�&�x��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B   B��B  B  B   B(  B0  B8  B@ffBH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&fD&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DGy�DH  DH�fDI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DN��DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� DtfDt� Dt� Dy�3D��D�d�D��\D�޸D��D�X D���D���D�%qD�]�D��HD��D�!�D�VDڏ\D��D�$)D�K�D�)D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�p�@���@���AQ�A<Q�A\Q�A|Q�A�(�A�(�A�(�A�(�A�(�A�(�A���A�(�B�B{B{B{B'{B/{B7{B?z�BG{BO{BW{B_{Bg{Bo{Bw{B{B��=B��pB��=B��=B��=B�W
B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=BÊ=BǊ=Bˊ=Bϊ=Bӊ=B׊=Bۊ=Bߊ=B�=B�=B�=B�=B�=B��=B��=B��=C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��C��C��C��C��C��C���C��C��\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D qHD �HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HD	qHD	�HD
qHD
�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD��DqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HD qHD �HD!qHD!�HD"qHD"�HD#qHD#�HD$qHD$�HD%qHD%��D&qHD&�HD'qHD'�HD(qHD(�HD)qHD)�HD*qHD*�HD+qHD+�HD,qHD,�HD-qHD-�HD.qHD.�HD/qHD/�HD0qHD0�HD1qHD1�HD2qHD2�HD3qHD3�HD4qHD4�HD5qHD5�HD6qHD6�HD7qHD7�HD8qHD8�HD9qHD9�HD:qHD:�HD;qHD;�HD<qHD<�HD=qHD=�HD>qHD>�HD?qHD?�HD@qHD@�HDAqHDA�HDBqHDB�HDCqHDC�HDDqHDD�HDEqHDE�HDFqHDF�HDGj�DG�HDHw�DH�HDIqHDI�HDJqHDJ�HDKqHDK�HDLqHDL�HDMqHDM�HDNqHDN��DOqHDO�HDPqHDP�HDQqHDQ�HDRqHDR�HDSqHDS�HDTqHDT�HDUqHDU�HDVqHDV�HDWqHDW�HDXqHDX�HDYqHDY�HDZqHDZ�HD[qHD[�HD\qHD\�HD]qHD]�HD^qHD^�HD_qHD_�HD`qHD`�HDaqHDa�HDbqHDb�HDcqHDc�HDdqHDd�HDeqHDe�HDfqHDf�HDgqHDg�HDhqHDh�HDiqHDi�HDjqHDj�HDkqHDk�HDlqHDl�HDmqHDm�HDnqHDn�HDoqHDo�HDpqHDp�HDqqHDq�HDrqHDr�HDsqHDs��DtqHDt�HDyt{D��)D�]qD�� D��\D�RD�P�D��{D��gD�D�VgD���D��>D�>D�N�Dڈ D��)D��D�D)D��D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A��A��A��
A��A��/A��#A��/A��;A��/A��mA��A��A��A���A�VAǋDA�K�Aȇ+Aȧ�AȮAȲ-Aȧ�A�`BA���A���AĲ-AA���A�5?A���A���A�;dA�l�A�VA�VA�|�A�=qA��PA�n�A�z�A�x�A�1'A�C�A��A�%A�z�A��FA�S�A�A��7A�"�A�%A���A���A�`BA��A��A�r�A��A�&�A�ZA�$�A���A�K�A��A�I�A�1'A��A�VA��A�C�A���A��A��uA�r�A�"�A���A��7A��7A��+A�?}A��-A�ffA�5?A���A��A�ZA���A���A�5?A�A��A�jA�(�A��A�A���A��A���A�5?A��^A��A��^A�1A��A�hsA���A��A���A�%A�M�A���A��A��-A�/A��A�A�(�A}��A{�FA{;dAw��At�!Ar�`Al�AidZAf��AdjAbbNA^�DAZ��AYoAW;dAT��AR�AP�AP1'AOS�AMl�AK`BAI��AG\)AC��AB��ABI�AAVA@-A>�`A>(�A=l�A=�A<�uA;�FA:��A9�A8jA7�wA7VA5A3��A2bA1/A0(�A/A-�A,ĜA,(�A++A*{A)K�A(-A'�A'��A'33A&�A%��A$��A#33A"9XA!p�A 1A��A�TA
=A��A�mA
=AhsA��A��Ap�Ar�AAȴAĜA5?A��A��A�At�A��A�+A�7AM�A�mA�hA
~�A��A`BA��A�DAt�A9XAG�A�`A��A&�A �A ^5A (�@��P@�V@�`B@��m@�M�@��@���@��@�x�@��m@�A�@��@���@�h@�z�@�+@��-@�7L@�9X@�{@�w@��@�n�@�V@�!@旍@�@㕁@�E�@��@�9X@�l�@���@�$�@��@���@��@��/@׍P@�ȴ@�5?@ԛ�@��;@�S�@�-@�K�@�E�@��@͑h@�&�@�ƨ@ʸR@�@��@�@��#@�(�@\@���@��@���@��@��R@��7@�r�@��@�t�@�^5@�@�7L@�&�@�V@���@���@�l�@��y@���@��9@�|�@�o@���@��\@��^@��@�1'@���@��@��@��@���@��@��!@�@�p�@��D@�  @���@�33@���@��\@�-@���@���@��7@��`@�ƨ@�l�@��@��y@��\@�-@��7@�?}@�?}@�?}@�?}@�&�@�V@��@���@��j@�j@��@�ƨ@�ƨ@�ƨ@�ƨ@�t�@�33@���@��R@�$�@���@��7@�V@��@�(�@�  @�  @��@�l�@�;d@��@�ȴ@��!@��\@�V@���@�hs@���@���@�r�@�Z@�Q�@�9X@��@��@�ƨ@��@�S�@��@��@���@�E�@�$�@��#@��-@�x�@�/@��/@��@��u@��P@�S�@�\)@�S�@�
=@�o@�@��y@���@�~�@�n�@�-@��^@�G�@�7L@�/@��@���@��D@�z�@��@��D@�r�@�9X@��@�S�@�+@�@��\@�n�@�ff@�V@�E�@�5?@�@��@�@�J@�{@���@�O�@�G�@�7L@���@��/@���@���@�r�@�b@�  @���@��@��m@���@�t�@��@�v�@��!@�M�@�^5@�ff@��@��T@��#@��-@�?}@���@�z�@��@�I�@�A�@�  @��F@�;d@��@���@��@���@�J@�$�@�-@�V@�^5@�V@�$�@��@���@��T@���@���@���@��@�`B@��@�V@�S&@~
�@u(�@jO@a�@Z5?@R��@K�@@E�@<Z@6R�@0�z@+�m@'4�@!�#@-@�S@�@�@T�@	O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A��A��A��A��A��
A��A��/A��#A��/A��;A��/A��mA��A��A��A���A�VAǋDA�K�Aȇ+Aȧ�AȮAȲ-Aȧ�A�`BA���A���AĲ-AA���A�5?A���A���A�;dA�l�A�VA�VA�|�A�=qA��PA�n�A�z�A�x�A�1'A�C�A��A�%A�z�A��FA�S�A�A��7A�"�A�%A���A���A�`BA��A��A�r�A��A�&�A�ZA�$�A���A�K�A��A�I�A�1'A��A�VA��A�C�A���A��A��uA�r�A�"�A���A��7A��7A��+A�?}A��-A�ffA�5?A���A��A�ZA���A���A�5?A�A��A�jA�(�A��A�A���A��A���A�5?A��^A��A��^A�1A��A�hsA���A��A���A�%A�M�A���A��A��-A�/A��A�A�(�A}��A{�FA{;dAw��At�!Ar�`Al�AidZAf��AdjAbbNA^�DAZ��AYoAW;dAT��AR�AP�AP1'AOS�AMl�AK`BAI��AG\)AC��AB��ABI�AAVA@-A>�`A>(�A=l�A=�A<�uA;�FA:��A9�A8jA7�wA7VA5A3��A2bA1/A0(�A/A-�A,ĜA,(�A++A*{A)K�A(-A'�A'��A'33A&�A%��A$��A#33A"9XA!p�A 1A��A�TA
=A��A�mA
=AhsA��A��Ap�Ar�AAȴAĜA5?A��A��A�At�A��A�+A�7AM�A�mA�hA
~�A��A`BA��A�DAt�A9XAG�A�`A��A&�A �A ^5A (�@��P@�V@�`B@��m@�M�@��@���@��@�x�@��m@�A�@��@���@�h@�z�@�+@��-@�7L@�9X@�{@�w@��@�n�@�V@�!@旍@�@㕁@�E�@��@�9X@�l�@���@�$�@��@���@��@��/@׍P@�ȴ@�5?@ԛ�@��;@�S�@�-@�K�@�E�@��@͑h@�&�@�ƨ@ʸR@�@��@�@��#@�(�@\@���@��@���@��@��R@��7@�r�@��@�t�@�^5@�@�7L@�&�@�V@���@���@�l�@��y@���@��9@�|�@�o@���@��\@��^@��@�1'@���@��@��@��@���@��@��!@�@�p�@��D@�  @���@�33@���@��\@�-@���@���@��7@��`@�ƨ@�l�@��@��y@��\@�-@��7@�?}@�?}@�?}@�?}@�&�@�V@��@���@��j@�j@��@�ƨ@�ƨ@�ƨ@�ƨ@�t�@�33@���@��R@�$�@���@��7@�V@��@�(�@�  @�  @��@�l�@�;d@��@�ȴ@��!@��\@�V@���@�hs@���@���@�r�@�Z@�Q�@�9X@��@��@�ƨ@��@�S�@��@��@���@�E�@�$�@��#@��-@�x�@�/@��/@��@��u@��P@�S�@�\)@�S�@�
=@�o@�@��y@���@�~�@�n�@�-@��^@�G�@�7L@�/@��@���@��D@�z�@��@��D@�r�@�9X@��@�S�@�+@�@��\@�n�@�ff@�V@�E�@�5?@�@��@�@�J@�{@���@�O�@�G�@�7L@���@��/@���@���@�r�@�b@�  @���@��@��m@���@�t�@��@�v�@��!@�M�@�^5@�ff@��@��T@��#@��-@�?}@���@�z�@��@�I�@�A�@�  @��F@�;d@��@���@��@���@�J@�$�@�-@�V@�^5@�V@�$�@��@���@��T@���@���@���@��@�`B@��G�O�@�S&@~
�@u(�@jO@a�@Z5?@R��@K�@@E�@<Z@6R�@0�z@+�m@'4�@!�#@-@�S@�@�@T�@	O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
#�B
$�B
$�B
$�B
$�B
&�B
49B
XB
�PB
��B
��B
�B
�LB
ŢB
��B
�;B
��B-B:^BE�Bq�Bz�B]/B=qB>wBG�BZBk�B\)Bs�B�B��B�wB�
B�fB�yB��B��B\B�B�B!�B(�B)�B0!B)�B�B �B#�B'�B33B>wBN�BW
B_;BdZBiyBhsBhsBiyBjBk�Bq�Bs�Bs�Bt�Bt�Bt�BiyB@�B0!B$�B�BB  B��B��B�B�mB�/B�B��B�wB��B�bB�JB�7B�%B�Bp�BffB_;BYBN�BI�B@�B1'B!�B�BDB
�B
�B
�RB
��B
��B
��B
�bB
}�B
ffB
L�B
�B
1B
B	�B	��B	�wB	��B	t�B	\)B	N�B	G�B	0!B	�B	PB	1B��B��B�B�mB�ZB�;B�B��B��BĜB�}B�jB�^B�FB�-B�!B�B��B��B��B��B��B��B��B��B��B�uB�\B�1B�B}�B~�Bz�Bz�Bu�Bu�Bx�Bx�Bu�Bu�Bu�Bt�Br�Bq�Bp�Bo�Bo�Bm�BiyBffBe`BffBffBe`BcTBffBp�Bn�Br�Bl�Br�B|�B|�Bu�Bn�Bp�Bq�Bp�Bo�Bk�BgmBhsBk�Be`BcTBdZBdZBe`BgmBffBdZBbNBbNB`BB_;B_;B^5B^5B^5B\)B[#BZB[#BYBYB[#B[#B`BB`BB`BB_;B]/B[#BYBW
BW
BW
BXBW
BW
BW
B^5BdZBgmBiyBjBk�BiyBiyBiyBiyBjBjBjBl�Bm�Bm�Bn�Bq�Bq�Bq�Bs�Bz�B|�B|�B~�B� B�B�B� B~�B�B~�B� Bz�Bz�By�B{�B� B�B�B� B�B�B�B�B�B�%B�7B�JB�bB�hB�hB�{B��B��B��B��B��B��B��B��B��B��B�B�9B�dB�qB��BĜBǮB��B��B��B��B�B�
B�B�B�)B�)B�HB�yB�B�B�B�B��B��B��B��B��B��B	  B	B	B	B	B	1B	JB	PB	PB	PB	PB	bB	oB	�B	�B	�B	#�B	&�B	,B	0!B	7LB	:^B	:^B	>wB	B�B	D�B	G�B	I�B	J�B	K�B	M�B	Q�B	XB	^5B	`BB	bNB	cTB	dZB	ffB	hsB	jB	k�B	o�B	q�B	s�B	t�B	u�B	z�B	{�B	|�B	}�B	� B	�B	�B	�%B	�+B	�JB	�PB	�VB	�\B	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�9B	�9B	�FB	�RB	�dB	�jB	�wB	�wB	�}B	�}B	B	ĜB	ǮB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�)B	�/B	�/B	�/B	�/B	�;B	�;B	�;B	�5B	�;B	�;B	�;B	�;B	�HB	�HB	�TB	�fB	�fB	�fB	�fB	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B
�B
MB
 �B
)�B
2�B
:^B
DMB
HKB
P�B
V9B
\B
`vB
dtB
jB
m�B
q�B
vzB
zDB
B
�U111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
$,B
G�B
}8B
��B
��B
��B
�/B
��B
��B
�B
��B�B*3B5vBaxBj�BMB-GB.NB7�BI�B[WBK�Bc�Bt�B��B�?B��B�*B�<B�~B�B�B@B	YB�B�B�B�B�B}B�B�B�B"�B.3B>�BF�BN�BTBY1BX+BX+BY1BZ7B[=BaaBcmBcmBdsBdsBdsBY2B0BB�B�B	_B��B��B�B�B�uB�9B��B��B��B�IB��B�;B|$ByBv Bs�B`�BVFBOBH�B>�B9�B0hB!B�BxB
�1B
�B
�B
�KB
��B
��B
��B
�`B
m�B
VjB
<�B
�B	�CB	�B	ܡB	��B	��B	��B	d�B	LTB	?B	7�B	 SB	
�B��B�iB�4B��B��B׫BԘB�zB�QB�:B�
B��B��B��B��B��B�uB�iB�WB�EB�@B�:B�:B�.B�
B��B��B��B��B�Bx�Br]BnFBoLBk4Bk4BfBfBi)Bi)BfBfBfBeBcBb B`�B_�B_�B]�BY�BV�BU�BV�BV�BU�BS�BV�B`�B^�Bc	B\�Bc	BmFBmFBfB^�B`�BbB`�B_�B[�BW�BX�B[�BU�BS�BT�BT�BU�BW�BV�BT�BR�BR�BP�BO�BO�BN�BN�BN�BL�BK�BJBK�BIzBIzBK�BK�BP�BP�BP�BO�BM�BK�BI{BGnBGnBGoBHuBGoBGoBGoBN�BT�BW�BY�BZ�B[�BY�BY�BY�BY�BZ�BZ�BZ�B\�B]�B]�B^�BbBbBbBdBkEBmRBmRBo]BpcBroBqjBpdBo^BqjBo^BpeBkFBkFBjABlMBpeBrqBrqBpfBrqBrqBt~Bu�BtBv�By�B|�B��B��B��B��B��B��B��B��B�B�B�!B�.B�FB�SB�pB��B��B��B��B��B�B�&B�8B�KB�VB�bB�hB�uB�{B̇B̇BѥB��B��B��B��B�B�$B�AB�NB�NB�NB�NB�ZB�fB�lB�sB�xB��B��B��B��B��B��B	 �B	�B	�B		�B	B	-B	?B	]B	 vB	'�B	*�B	*�B	.�B	2�B	4�B	8 B	:B	;B	<B	>%B	B=B	H`B	N�B	P�B	R�B	S�B	T�B	V�B	X�B	Z�B	[�B	_�B	a�B	dB	e	B	fB	k.B	l4B	m;B	n@B	pLB	teB	teB	vqB	wvB	|�B	}�B	~�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�8B	�DB	�JB	�[B	�hB	�nB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�#B	�5B	�;B	�;B	�;B	�;B	�AB	�AB	�AB	�GB	�ZB	�ZB	�lB	�qB	�qB	�qB	�qB	�}B	�}B	�}B	�xB	�}B	�}B	�}B	�}B	ъB	ъB	ӖB	֨B	֨B	֨B	֨B	ׯB	ٻB	ٺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	�B	�B
�B
6B
�B
"�B
*�B
4�B
8�B
@�B
FsB
LIB
P�B
T�B
Z�B
]�B
a�B
f�B
j|B
oKB
q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.23 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9996(+/-0.0001), vertically averaged dS =-0.016(+/-0.005) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144272022020411442720220204114427  AO  ARCAADJP                                                                    20200624140108    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200624140108  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200624140108  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114427  IP                  G�O�G�O�G�O�                