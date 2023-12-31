CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:14Z creation      
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
_FillValue                    �`Argo profile    3.1 1.2 19500101000000  20200619170914  20220204114419  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               WA   AO  7662                            2C  D   APEX                            8312                            080318                          846 @���h݋�1   @�����@6�hr� ��c�A�7K�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    WA   B   B   @���@�  @���A   A@  A^ffA�  A�  A�  A�  A�  A���A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bw��B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33C �C  C  C  C  C
  C  C  C  C  C  C  C�C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4�C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP�fDQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Day�Db  Db� Db��Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp�fDqfDq� Dr  Dr� Ds  Ds� Dt  Dt� Dy��D�)�D�Z�D��qD���D��D�_�D��RD���D��D�K3D��=D���D�&D�[�DڠRD���D�%D�^D�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��H@�{@��HA
=A=
=A[p�A}
=A��A��A��A��A�Q�AޅA�A��BB�BB�BB�BB�B'B�B/B�B7B�B?B�BGB�BOB�BWB�B_B�BgB�BoB�Bv�)BB�B��HB��HB�nB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HBáHBǡHBˡHBϡHBӡHBסHBۡHBߡHB�HB�HB�HB�HB�HB��HB��{B��|CФCФCФCФC	ФCФCФCФCФCФCФC�>CФCФCФCФC!ФC#ФC%ФC'ФC)ФC+ФC-ФC/ФC1ФC3�>C5ФC7ФC9ФC;ФC=ФC?ФCAФCCФCEФCGФCIФCKФCMФCOФCQФCSФCUФCWФCYФC[ФC]ФC_ФCaФCcФCeФCgФCiФCkФCmФCoФCqФCsФCuФCwФCyФC{ФC}ФCФC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�ۅC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD t)D �)Dt)D�)Dt)D�)Dt)D�)Dt)D�)Dt)D�)Dt)D�)Dt)D�)Dt)D�)D	t)D	�)D
t)D
�)Dt)D�)Dt)D�)Dt)D�)Dt)D�)Dt)D�)Dt)D��Dt)D�)Dt)D�)Dt)D�)Dt)D�)Dt)D�)Dt)D�)Dt)D�)Dt)D�)Dt)D�)Dt)D�)Dt)D�)Dt)D�)Dt)D�)Dt)D�)Dt)D�)D t)D �)D!t)D!�)D"t)D"�)D#t)D#�)D$t)D$�)D%t)D%�)D&t)D&�)D't)D'�)D(t)D(�)D)t)D)�)D*t)D*�)D+t)D+�)D,t)D,�)D-t)D-�)D.t)D.�)D/t)D/�)D0t)D0�)D1t)D1�)D2t)D2�)D3t)D3�)D4t)D4�)D5t)D5�)D6t)D6�)D7t)D7�)D8t)D8�)D9t)D9�)D:t)D:�)D;t)D;�)D<t)D<�)D=t)D=�)D>t)D>�)D?t)D?�)D@t)D@�)DAt)DA�)DBt)DB�)DCt)DC�)DDt)DD�)DEt)DE�)DFt)DF�)DGt)DG�)DHt)DH�)DIt)DI�)DJt)DJ�)DKt)DK�)DLt)DL�)DMt)DM�)DNt)DN�)DOt)DO�)DPz�DP�)DQt)DQ�)DRt)DR�)DSt)DS�)DTt)DT�)DUt)DU�)DVt)DV�)DWt)DW�)DXt)DX�)DYt)DY�)DZt)DZ�)D[t)D[�)D\t)D\�)D]t)D]�)D^t)D^�)D_t)D_�)D`t)D`�)Dam�Da�)Dbt)Db��Dct)Dc�)Ddt)Dd�)Det)De�)Dft)Df�)Dgt)Dg�)Dht)Dh�)Dit)Di�)Djt)Dj�)Dkt)Dk�)Dlt)Dl�)Dmt)Dm�)Dnt)Dn�)Dot)Do�)Dpz�Dp��Dqt)Dq�)Drt)Dr�)Dst)Ds�)Dtt)Dy��D�$ D�T�D���D���D��D�Y�D��fD���D��D�EGD��QD�� D� (D�U�DښfD���D�3D�X(D�D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aա�Aա�Aգ�Aգ�Aգ�Aա�Aգ�Aգ�Aե�Aե�AՕ�A�E�A�(�A�$�A�bA�%A���A��A��;A���AԲ-A�p�A�9XA�;dAҴ9A�+A��yA�jA�ƨAϟ�A̕�A��A�\)A��
A�A�A�JA�`BA���A��A��!A�ZA���A�7LA��yA�(�A��HA��/A�A�ffA��hA��^A�t�A�-A��A�r�A���A�v�A��FA�;dA�~�A�\)A�z�A��PA��A���A�r�A�/A��jA�~�A�VA��A���A�{A�;dA�I�A�33A��^A��+A��A���A�
=A��^A�l�A���A�  A���A��A��A�z�A�  A�^5A�oA��A�9XA��A�ffA�(�A���A�~�A�A�oA�I�A�oA�^5A���A�%A�A�A�VA��
A��
A���A�VA���A��A�O�A��7A�jA�t�A��\A��mA�`BA��hA�1A�VA��A��^A��HA��wA�C�A�bA��DA��TA���A�S�A}7LAx��Aw��Awx�AuK�As�PAp�An��AlM�Ai��Ad{Aa�A_��A^��A^�9A^=qA[/AXȴAVz�AS�wAQ��AP5?AO33AM��AL$�AJ��AI&�AG"�AE��ADI�AC/AA�A@1'A?��A?K�A=p�A;C�A8ĜA6�!A5A5�A5p�A4ĜA4E�A3p�A1��A17LA0v�A.�A,�DA+&�A)/A'�7A$ĜA#oA"^5A!��A VAx�A��AVA�wA`BA-A1AbA;dA�A��A�A��A��A��A33AĜA��AjAƨA�HA
�jA	K�Az�A5?A��A?}A�jAQ�A �A��A��A�7AhsAdZA�7AE�A��AVA��A��A7LA�9A��AhsAȴA�AQ�A�wA33A ��A ��A �A ��A ^5@�S�@�^5@�&�@�Z@�5?@��@�v�@��D@�+@�ƨ@�~�@���@�%@�ƨ@�@�@��@�$�@�7@�`B@��@�V@�x�@�/@��@�%@�(�@�S�@ݺ^@���@֟�@�`B@ӶF@�ff@с@���@��
@�V@�Ĝ@˕�@���@�p�@�z�@ǝ�@��H@Ə\@Ɨ�@Ƨ�@Ɨ�@�@ŉ7@�?}@ă@þw@���@�X@��+@�X@��h@�X@���@�bN@��F@���@��@�&�@�`B@�^5@��F@��u@��D@��@���@���@�ȴ@��\@�=q@��#@�@��@�G�@��/@�1@���@��@�1'@��@�t�@�33@��^@�@�?}@�9X@��w@��@�C�@��#@�G�@�7L@���@�r�@���@�+@���@�{@��-@�Ĝ@�(�@��F@��y@�$�@��h@�p�@��9@�z�@�z�@�bN@�1'@��@��@�|�@�\)@�+@�+@�S�@��@�|�@�\)@�dZ@�K�@�dZ@���@�ff@�-@�@��T@��T@���@���@�`B@�%@�Z@��@��w@�+@�v�@��@�@��-@�O�@��@���@�&�@��9@�bN@�9X@��@���@�|�@�K�@�"�@�@��@���@�n�@�$�@���@���@���@� �@��@�|�@�o@��@�@���@���@��@�+@��+@�5?@�@��-@�x�@�O�@���@��@���@��@�j@�1'@���@��w@���@�K�@�V@��@��h@��@�O�@��9@�r�@�r�@��@��
@��P@�|�@��@�o@�^5@��@�@���@���@�p�@��@���@��/@���@�bN@� �@��@���@���@�ƨ@��F@���@�|�@��@�E�@��@���@��^@���@��h@�p�@�`B@�O�@�7L@�/@��@�Ĝ@��@z�m@u(�@oU�@h"h@b��@We�@N��@G�@@1'@;Y@2��@.=q@&�H@"@w�@��@�w@��@
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   Aա�Aա�Aգ�Aգ�Aգ�Aա�Aգ�Aգ�Aե�Aե�AՕ�A�E�A�(�A�$�A�bA�%A���A��A��;A���AԲ-A�p�A�9XA�;dAҴ9A�+A��yA�jA�ƨAϟ�A̕�A��A�\)A��
A�A�A�JA�`BA���A��A��!A�ZA���A�7LA��yA�(�A��HA��/A�A�ffA��hA��^A�t�A�-A��A�r�A���A�v�A��FA�;dA�~�A�\)A�z�A��PA��A���A�r�A�/A��jA�~�A�VA��A���A�{A�;dA�I�A�33A��^A��+A��A���A�
=A��^A�l�A���A�  A���A��A��A�z�A�  A�^5A�oA��A�9XA��A�ffA�(�A���A�~�A�A�oA�I�A�oA�^5A���A�%A�A�A�VA��
A��
A���A�VA���A��A�O�A��7A�jA�t�A��\A��mA�`BA��hA�1A�VA��A��^A��HA��wA�C�A�bA��DA��TA���A�S�A}7LAx��Aw��Awx�AuK�As�PAp�An��AlM�Ai��Ad{Aa�A_��A^��A^�9A^=qA[/AXȴAVz�AS�wAQ��AP5?AO33AM��AL$�AJ��AI&�AG"�AE��ADI�AC/AA�A@1'A?��A?K�A=p�A;C�A8ĜA6�!A5A5�A5p�A4ĜA4E�A3p�A1��A17LA0v�A.�A,�DA+&�A)/A'�7A$ĜA#oA"^5A!��A VAx�A��AVA�wA`BA-A1AbA;dA�A��A�A��A��A��A33AĜA��AjAƨA�HA
�jA	K�Az�A5?A��A?}A�jAQ�A �A��A��A�7AhsAdZA�7AE�A��AVA��A��A7LA�9A��AhsAȴA�AQ�A�wA33A ��A ��A �A ��A ^5@�S�@�^5@�&�@�Z@�5?@��@�v�@��D@�+@�ƨ@�~�@���@�%@�ƨ@�@�@��@�$�@�7@�`B@��@�V@�x�@�/@��@�%@�(�@�S�@ݺ^@���@֟�@�`B@ӶF@�ff@с@���@��
@�V@�Ĝ@˕�@���@�p�@�z�@ǝ�@��H@Ə\@Ɨ�@Ƨ�@Ɨ�@�@ŉ7@�?}@ă@þw@���@�X@��+@�X@��h@�X@���@�bN@��F@���@��@�&�@�`B@�^5@��F@��u@��D@��@���@���@�ȴ@��\@�=q@��#@�@��@�G�@��/@�1@���@��@�1'@��@�t�@�33@��^@�@�?}@�9X@��w@��@�C�@��#@�G�@�7L@���@�r�@���@�+@���@�{@��-@�Ĝ@�(�@��F@��y@�$�@��h@�p�@��9@�z�@�z�@�bN@�1'@��@��@�|�@�\)@�+@�+@�S�@��@�|�@�\)@�dZ@�K�@�dZ@���@�ff@�-@�@��T@��T@���@���@�`B@�%@�Z@��@��w@�+@�v�@��@�@��-@�O�@��@���@�&�@��9@�bN@�9X@��@���@�|�@�K�@�"�@�@��@���@�n�@�$�@���@���@���@� �@��@�|�@�o@��@�@���@���@��@�+@��+@�5?@�@��-@�x�@�O�@���@��@���@��@�j@�1'@���@��w@���@�K�@�V@��@��h@��@�O�@��9@�r�@�r�@��@��
@��P@�|�@��@�o@�^5@��@�@���@���@�p�@��@���@��/@���@�bN@� �@��@���@���@�ƨ@��F@���@�|�@��@�E�@��@���@��^@���@��h@�p�@�`B@�O�@�7L@�/G�O�@�Ĝ@��@z�m@u(�@oU�@h"h@b��@We�@N��@G�@@1'@;Y@2��@.=q@&�H@"@w�@��@�w@��@
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�%B	�%B	�%B	�%B	�%B	�%B	�%B	�%B	�%B	�%B	�B	�B	�B	�B	�B	� B	~�B	}�B	}�B	}�B	|�B	}�B	�+B	��B	��B
hB
hB
1B	��B	�yB	ƨB	ÖB	�yB
bB
49B
T�B
S�B
S�B
YB
VB
XB
ffB
�+B
�B
��B
��B
��B
�B+B$�B/BH�BZBo�B{�B�%B�\B��B�B�HB�HB�sB�yB�yB��B%B\BuB�B�B�B �B#�B'�B'�B&�B5?BL�BcTBx�B�1B�PB�hB��B�PB�=B�Bz�B�B�hB��B��B�B�XB�^B�XBBǮBɺBƨB�9B��B�BVBA�B>wB>wB?}B?}B9XB#�B�BoB
=B��B�B�B�!B��B� B]/BL�BB�B49B�BB
�BB
��B
�-B
�B
��B
�1B
t�B
iyB
N�B
%�B
�B
�B
%B	�B	�BB	��B	�FB	��B	z�B	p�B	jB	dZB	aHB	\)B	Q�B	E�B	;dB	+B	�B	�B	\B	JB	B	B��B�B�B�`B�HB�B��BǮBB�jB�3B��B��B��B��B��B��B��B��B��B�{B�oB�JB�+B� B|�Bv�Bs�Bo�Bn�Bl�Bl�BhsBffBffBffBe`BdZBaHB`BB\)B^5B_;B`BBaHBdZBbNB`BB]/B\)BZBXBR�BJ�BE�BC�BA�BC�BA�B@�BA�BD�BD�BF�BI�BJ�BK�BO�B]/BiyBr�Bs�Bs�Bu�Bx�B� B�1B�PB�\B�hB�oB�bB�bB�oB�uB�uB�{B�oB�=B�%B�B|�Bt�BjB_;BW
BN�BL�BK�BM�BL�BI�BI�BK�BO�BS�BT�BW
B[#BdZBq�Bx�B~�B�B�B�Bx�Bl�BiyBe`BbNBbNBhsBo�BiyBgmBiyBm�Br�Bu�Bx�B{�B}�B~�B�B�%B�VB��B��B��B��B��B��B��B�uB��B��B��B��B��B�\B�{B��B��B�B�dBǮBǮB��B��B��B��B��B�
B�B�BB�TB�ZB�mB�B�B��B��B��B�B�B��B�B�yB�`B�ZB�TB�TB�sB�yB�B�B�B�B�B�B�B�B��B��B��B��B��B��B	  B	%B		7B	VB	\B	oB	uB	�B	�B	�B	�B	$�B	)�B	/B	5?B	9XB	>wB	B�B	E�B	R�B	_;B	bNB	aHB	bNB	dZB	e`B	e`B	gmB	hsB	iyB	hsB	gmB	gmB	gmB	hsB	jB	k�B	l�B	n�B	p�B	s�B	t�B	y�B	{�B	�B	�B	�B	�B	�%B	�+B	�1B	�=B	�DB	�JB	�PB	�PB	�PB	�VB	�PB	�bB	�oB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�B	�B	�!B	�-B	�3B	�3B	�?B	�?B	�FB	�LB	�XB	�dB	�dB	�qB	�wB	�}B	��B	B	ÖB	ÖB	ĜB	ĜB	ŢB	ŢB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�?B	�2B	��B
	�B
�B
�B
%�B
0B
33B
7B
=�B
B[B
I�B
Q B
U�B
[=B
`�B
e�B
i�B
p;B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B	~bB	~bB	~bB	~bB	~bB	~bB	~bB	~bB	~bB	~bB	}\B	{OB	yCB	zIB	yCB	x>B	w8B	v2B	v2B	v2B	u,B	v2B	hB	�B	�B
	�B
	�B
 gB	�%B	�B	��B	��B	�B
�B
,nB
M1B
L,B
L,B
QKB
N8B
PDB
^�B
\B
�7B
� B
�B
�B
�JB
�UBB'CB@�BRCBg�BtB~HB�B��B�#B�fB�gB��B�B�B��B�BByB�B�B�B�B�B�B B BB-[BD�B[mBp�B�HB�gB�~B��B�gB�TByBr�B{*B�B��B�B�$B�mB�sB�mB��B��B��B��B�OB��B}8BN B9�B6�B6�B7�B7�B1wB�B�B
�B_B�B�B�=B�IB��Bx,BU]BD�B:�B,jB�B
�@B
�yB
�B
�gB
�BB
�B
�nB
l�B
a�B
GB
'B
�B
�B	�kB	��B	؋B	�B	��B	� B	s1B	h�B	b�B	\�B	Y�B	T|B	J@B	=�B	3�B	#YB	B	�B	�B	�B�yB�`B�6B�B��BݽB٥B�{B�&B�B��B��B��B�RB�EB�B�B�	B��B��B��B��B��B��B��B�BxfBuTBo0BlBhBg Bd�Bd�B`�B^�B^�B^�B]�B\�BY�BX�BT�BV�BW�BX�BY�B\�BZ�BX�BU�BT�BR�BP|BK^BC.B>B<B9�B<B9�B8�B9�B=
B=
B?BB(BC/BD5BHLBU�Ba�BkBl!Bl!Bn.Bq@BxkB��B��B��B��B��B��B��B��B��B��B��B��B��B~�B|�BuZBm)Bb�BW�BOyBGIBE=BD7BFCBE=BB*BB*BD7BHOBLhBMnBOzBS�B\�BjBqCBwgBzyB{BzzBqCBd�Ba�B]�BZ�BZ�B`�BhBa�B_�Ba�BfBk Bn3BqEBtWBvcBwiBz{B~�B��B��B�B�&B�&B� B�B��B��B��B��B��B��B��B��B��B�B�9B��B��B�B�B�3B�?B�EB�JB�cB�uB�{BحB۾B��B��B��B�B�-B�-B�&B� B� B�&B��B��B��B��BۿBۿB��B��B��B��B�B�B�B�B�!B�B�.B�:B�FB�RB�YB�eB�kB��B	�B	�B	�B	
�B	�B	�B		B	B	!B	FB	"dB	'�B	-�B	1�B	6�B	:�B	>	B	KXB	W�B	Z�B	Y�B	Z�B	\�B	]�B	]�B	_�B	`�B	a�B	`�B	_�B	_�B	_�B	`�B	b�B	c�B	d�B	f�B	iB	lB	m B	r?B	tKB	yiB	{uB	||B	}�B	~�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�'B	�-B	�-B	�3B	�?B	�EB	�KB	�RB	�KB	�KB	�KB	�RB	�^B	�^B	�KB	�RB	�XB	�dB	�dB	�^B	�dB	�pB	�vB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�'B	�-B	�-B	�-B	�3B	�3B	�3B	�3G�O�B	ϟB	ޑB	�B
B
B
YB
ZB
(cB
+�B
/sB
5�B
:�B
BKB
I[B
NB
S�B
YB
^$B
b"B
h�B
nQ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.185 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.008(+/-0.005) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144192022020411441920220204114419  AO  ARCAADJP                                                                    20200619170914    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170914  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170914  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114419  IP                  G�O�G�O�G�O�                