CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:00Z creation      
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
_FillValue                    �`Argo profile    3.1 1.2 19500101000000  20200619170900  20220204114413  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL                A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @؏��q�1   @؏}'�@8�r� Ĝ�cָQ�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                     A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`  Bh  Bp  Bx  B�  B�  B�  B�ffB���B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� DfD� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/fD/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy��D�=D�]D���D�� D�#�D�UqD��qD��D�  D�\{D��D��D��D�R�DڐRD�޸D�
D�ND�~11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@�Q�@�Q�A(�A>(�A^(�A~(�A�{A�{A�{A�{A�{A�{A�{A�{B�=B�=B�=B�=B'�=B/�=B7�=B?�=BG�=BO�=BW�B_�=Bg�=Bo�=Bw�=B�=B��B��B�+�B���B��B���B��B��B��B��B��B��B��B��B��B��B��B��Bˑ�B��B��B��B��B��B��B��B��B��B��B��B��B��C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��HC��HC��HC��HC��HC��HC��HC��{C��HC��HC��HC��HC��HC��{C��HC��{C��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HD x�D ��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D	x�D	��D
x�D
��Dx�D��Dx�D��Dx�D�
Dx�D��Dx�D��Dx�D��Dr>D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D x�D ��D!x�D!��D"x�D"��D#x�D#��D$x�D$��D%x�D%��D&x�D&��D'x�D'��D(x�D(��D)x�D)��D*x�D*��D+x�D+��D,x�D,��D-x�D-��D.x�D.�
D/x�D/��D0x�D0��D1x�D1��D2x�D2��D3x�D3��D4x�D4��D5x�D5��D6x�D6��D7x�D7��D8x�D8��D9x�D9��D:x�D:��D;x�D;��D<x�D<��D=x�D=��D>x�D>��D?x�D?��D@x�D@��DAx�DA��DBx�DB��DCx�DC��DDx�DD��DEx�DE��DFx�DF��DGx�DG��DHx�DH��DIx�DI��DJx�DJ��DKx�DK��DLx�DL��DMx�DM��DNx�DN��DOx�DO��DPx�DP��DQx�DQ��DRx�DR��DSx�DS��DTx�DT��DUx�DU��DVx�DV��DWx�DW��DXx�DX��DYx�DY��DZx�DZ��D[x�D[��D\x�D\��D]x�D]��D^x�D^��D_x�D_��D`x�D`��Dax�Da��Dbx�Db��Dcx�Dc��Ddx�Dd��Dex�De��Dfx�Df��Dgx�Dg��Dhx�Dh��Dix�Di��Djx�Dj��Dkx�Dk��Dlx�Dl��Dmx�Dm��Dnx�Dn��Dox�Do��Dpx�Dp��Dqx�Dq��Drx�Dr��Dsx�Ds��Dtx�Dt��Dy�{D��D�YqD���D��RD��D�Q�D���D���D�RD�X�D��qD��qD��D�N�Dڌ�D��
D�\D�JfD�zf11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A�%A�%A�A���A�Aҙ�Aҏ\Aҏ\A�|�A�t�A�r�A�l�A�dZA�hsA�bNA�hsA�p�A�hsA�r�A�z�A҅AҋDAҏ\Aҝ�AҼjA�%AҶFA�O�A�t�A�\)A�dZA���A�oA�t�A���A��A��wA�A��A��A�|�A�jA���A��
A�l�A�/A��TA�dZA�ƨA��A�l�A�bNA�ƨA�t�A���A�O�A��A��A��hA�oA�A�A��jA�(�A��9A�  A�t�A�jA�A�hsA��A��A�dZA���A�`BA�/A���A���A�O�A��A��A�G�A��A�
=A��yA�ZA���A�Q�A�1A��FA�p�A���A��TA��hA�9XA�ƨA�hsA��A�n�A��HA�;dA��uA�
=A��A�ĜA��A�VA�XA��TA��A��A��A�C�A~bNAyƨAu33Ao?}Am"�Ai33Af�Ae�;Ae�Ae%Ad�DAb��Aa"�A`�A_�A_hsA^z�A\�HA[�PA[/A[�AZ�DAZ5?AY�;AYt�AY;dAX�yAX�AWG�AU�wAUXAT��AT1AS��AS��AR�AQ��AQ��AQ33AQ7LAQ;dAP��AP1'AP1'AP$�AO�;AOK�AM�;AK��AKO�AJ�/AH��AFJAD�AD1AB�AB-A?��A="�A;��A;"�A9\)A6(�A4�jA3�#A3&�A2�`A2I�A1?}A0��A0^5A/A.5?A-l�A+/A(v�A'�
A&�`A&�!A%��A$ �A!�#A   A�DA1A/AVA(�AhsA�A��A(�A�+AdZAĜA5?A��A&�A�A��A�AjA�A�jAA7LA�DAZA �A
��A
VA	�PA	AJA�RA$�A&�AJAAp�A�uA E�@��@��#@���@��7@�/@�9X@�~�@��9@�b@�M�@���@�%@�Q�@�1@��@�{@��@��@���@��@�l�@�7L@�P@�+@���@�@��@���@��@�  @���@��
@ް!@�x�@ܴ9@ڇ+@���@�Z@���@�
=@�ff@�`B@�(�@Ӯ@��T@У�@�A�@�1@�|�@�33@�ȴ@�=q@��T@ͺ^@�hs@̴9@ˍP@�x�@��m@���@Å@�E�@�V@�"�@�v�@�`B@�Ĝ@���@�dZ@���@���@�A�@�dZ@���@���@��j@��;@���@�v�@��T@�%@���@��u@�z�@��;@��F@���@�|�@�\)@�dZ@�t�@�ƨ@���@���@�\)@�o@���@�{@��#@�hs@��`@���@���@�bN@�9X@� �@�1'@���@��@���@�x�@���@���@��`@��@�G�@��`@��F@��@��@�A�@��P@���@�  @��@�G�@�$�@�$�@�@�J@��@��h@�X@�G�@��@��@�S�@��R@�E�@�@���@�?}@�r�@�1@��@��
@�C�@�@��!@��\@�=q@�{@��@���@�X@�O�@�7L@��@��j@��@�  @�  @��
@�+@�ff@��@��@�&�@���@�r�@�b@�"�@�x�@�V@�&�@���@�Z@�1'@��
@�S�@�S�@�;d@�33@�"�@�@��@���@���@�~�@�J@��#@���@��@�/@�/@��9@��j@���@��@��@��@�M�@��@��@�1'@�b@� �@���@��9@��P@�l�@�K�@�;d@���@���@���@��\@�v�@�v�@�V@���@�7L@�&�@�%@���@�Q�@�9X@�9X@�I�@�9X@�9X@��@��
@���@�S�@�o@��H@���@�M�@�-@��@�{@�@��@�{@�-@�@��@��#@���@�{@���@���@�
=@� @v�<@k]�@`��@Y^�@S�@J�@D<�@<�@6�"@1�X@+��@'��@#U�@z@L�@B[@�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   A���A�%A�%A�A���A�Aҙ�Aҏ\Aҏ\A�|�A�t�A�r�A�l�A�dZA�hsA�bNA�hsA�p�A�hsA�r�A�z�A҅AҋDAҏ\Aҝ�AҼjA�%AҶFA�O�A�t�A�\)A�dZA���A�oA�t�A���A��A��wA�A��A��A�|�A�jA���A��
A�l�A�/A��TA�dZA�ƨA��A�l�A�bNA�ƨA�t�A���A�O�A��A��A��hA�oA�A�A��jA�(�A��9A�  A�t�A�jA�A�hsA��A��A�dZA���A�`BA�/A���A���A�O�A��A��A�G�A��A�
=A��yA�ZA���A�Q�A�1A��FA�p�A���A��TA��hA�9XA�ƨA�hsA��A�n�A��HA�;dA��uA�
=A��A�ĜA��A�VA�XA��TA��A��A��A�C�A~bNAyƨAu33Ao?}Am"�Ai33Af�Ae�;Ae�Ae%Ad�DAb��Aa"�A`�A_�A_hsA^z�A\�HA[�PA[/A[�AZ�DAZ5?AY�;AYt�AY;dAX�yAX�AWG�AU�wAUXAT��AT1AS��AS��AR�AQ��AQ��AQ33AQ7LAQ;dAP��AP1'AP1'AP$�AO�;AOK�AM�;AK��AKO�AJ�/AH��AFJAD�AD1AB�AB-A?��A="�A;��A;"�A9\)A6(�A4�jA3�#A3&�A2�`A2I�A1?}A0��A0^5A/A.5?A-l�A+/A(v�A'�
A&�`A&�!A%��A$ �A!�#A   A�DA1A/AVA(�AhsA�A��A(�A�+AdZAĜA5?A��A&�A�A��A�AjA�A�jAA7LA�DAZA �A
��A
VA	�PA	AJA�RA$�A&�AJAAp�A�uA E�@��@��#@���@��7@�/@�9X@�~�@��9@�b@�M�@���@�%@�Q�@�1@��@�{@��@��@���@��@�l�@�7L@�P@�+@���@�@��@���@��@�  @���@��
@ް!@�x�@ܴ9@ڇ+@���@�Z@���@�
=@�ff@�`B@�(�@Ӯ@��T@У�@�A�@�1@�|�@�33@�ȴ@�=q@��T@ͺ^@�hs@̴9@ˍP@�x�@��m@���@Å@�E�@�V@�"�@�v�@�`B@�Ĝ@���@�dZ@���@���@�A�@�dZ@���@���@��j@��;@���@�v�@��T@�%@���@��u@�z�@��;@��F@���@�|�@�\)@�dZ@�t�@�ƨ@���@���@�\)@�o@���@�{@��#@�hs@��`@���@���@�bN@�9X@� �@�1'@���@��@���@�x�@���@���@��`@��@�G�@��`@��F@��@��@�A�@��P@���@�  @��@�G�@�$�@�$�@�@�J@��@��h@�X@�G�@��@��@�S�@��R@�E�@�@���@�?}@�r�@�1@��@��
@�C�@�@��!@��\@�=q@�{@��@���@�X@�O�@�7L@��@��j@��@�  @�  @��
@�+@�ff@��@��@�&�@���@�r�@�b@�"�@�x�@�V@�&�@���@�Z@�1'@��
@�S�@�S�@�;d@�33@�"�@�@��@���@���@�~�@�J@��#@���@��@�/@�/@��9@��j@���@��@��@��@�M�@��@��@�1'@�b@� �@���@��9@��P@�l�@�K�@�;d@���@���@���@��\@�v�@�v�@�V@���@�7L@�&�@�%@���@�Q�@�9X@�9X@�I�@�9X@�9X@��@��
@���@�S�@�o@��H@���@�M�@�-@��@�{@�@��@�{@�-@�@��@��#@���@�{@���G�O�@�
=@� @v�<@k]�@`��@Y^�@S�@J�@D<�@<�@6�"@1�X@+��@'��@#U�@z@L�@B[@�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�9B�wB��B�BB\B%�BD�BN�B^5BcTB]/BaHBXBP�BQ�BN�BL�BQ�BVBW
B]/BffB}�B}�By�B{�B|�B|�B� B�B�B�B�B�B�1B�+B�B�%B�B�B�B~�Bx�Bn�BffB_;BXBVBP�BK�BG�B@�B=qB9XB1'B,B"�B�B�B�B�B�BbB��B�`B�;B�BɺB�3B��B��B�bB�7B�B|�Bu�B[#BXBI�B(�B%�B"�B�BuB  B
��B
�/B
ŢB
�!B
�DB
ffB
D�B
\B	�B	�5B	��B	�B	��B	��B	��B	�\B	�B	q�B	gmB	e`B	e`B	l�B	t�B	p�B	o�B	q�B	t�B	r�B	p�B	m�B	l�B	k�B	iyB	e`B	_;B	\)B	[#B	VB	S�B	R�B	Q�B	J�B	I�B	F�B	G�B	J�B	I�B	D�B	F�B	G�B	H�B	B�B	5?B	 �B	�B	�B	!�B	\B	1B	B	  B��B�B�HB�B�B��B��B�RB�3B�!B�B�B��B��B��B��B��B�VB�=B�B�B�B�B�B{�Bt�Bq�BiyBe`BdZBZBVBP�BN�BM�BM�BK�BE�BD�BB�BA�BA�B@�B?}B?}B>wB<jB<jB8RB:^B6FB5?B6FB49B49B33B2-B2-B1'B/B.B.B,B+B+B+B'�B&�B'�B&�B&�B&�B)�B+B-B.B.B/B/B/B0!B/B/B49B7LB7LB7LB8RB7LB7LB7LB8RB;dB;dB<jB;dB;dB:^B>wBA�BC�BG�BH�BG�BG�BG�BF�BF�BE�BE�BI�BK�BK�BK�BM�BL�BM�BN�BN�BO�BO�BP�BS�BW
BYBaHBcTBgmBjBp�Br�Bv�Bx�Bz�B|�B~�B�B�+B�7B�DB�PB�VB�oB��B��B��B��B��B��B��B��B��B��B��B�B�!B�'B�9B�dB�jB�qB�}B��BƨBǮBɺB��B��B��B��B��B��B�B�;B�;B�5B�5B�5B�;B�HB�TB�ZB�`B�mB�B�B�B�B	B	B	%B	bB	�B	�B	�B	�B	 �B	!�B	"�B	#�B	.B	33B	5?B	9XB	=qB	>wB	=qB	?}B	@�B	A�B	C�B	D�B	E�B	G�B	H�B	H�B	J�B	K�B	L�B	N�B	O�B	P�B	P�B	Q�B	S�B	T�B	T�B	T�B	S�B	T�B	W
B	YB	ZB	^5B	]/B	]/B	_;B	bNB	bNB	_;B	^5B	_;B	^5B	^5B	`BB	bNB	cTB	ffB	ffB	gmB	iyB	o�B	q�B	s�B	u�B	x�B	{�B	}�B	� B	� B	�B	�7B	�DB	�PB	�\B	�VB	�PB	�VB	�VB	�VB	�VB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�?B	�LB	�RB	�XB	�XB	�RB	�XB	�jB	�jB	�qB	�wB	�wB	�wB	�wB	�wB	�wB	�qB	�wB	B	ÖB	ÖB	ĜB	ŢB	ƨB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�	B
3B
BB

B
 BB
'mB
0UB
8�B
B[B
GB
MjB
SuB
WYB
[#B
`B
e,B
iyB
nIB
oi11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   B�1B�1B�+B�%B�%B�B�B�B�B�B�B�B�B�B�B�B�B�1B�+B�=B�OB�gB�zB��B��B�*B��B�eB�B:B;�BF.BU�BZ�BT�BX�BOfBH<BIDBF1BD%BIDBM\BNbBT�B]�BuJBuJBq1Bs=BtEBtEBwWBx]Bx]BycBziB|vB�B~�B|vB}|B|wB{qB{qBvRBp.Be�B]�BV�BOlBM`BHABC$B?B7�B4�B0�B(�B#gB1BBBB�B�B�B�DB��B֠BуB�!B��B�MB�B��B��B{�Bt[Bm1BR�BO�BA+B jBWBEB3B
�B
�wB
�4B
ԩB
�B
��B
��B
]�B
<"B
�B	�B	��B	�B	��B	�JB	�9B	�B	��B	x�B	i@B	_B	\�B	\�B	d!B	lRB	h;B	g5B	iAB	lRB	jGB	h;B	e(B	d"B	cB	aB	\�B	V�B	S�B	R�B	M�B	K�B	J�B	I�B	B[B	ATB	>BB	?HB	B[B	ATB	<7B	>BB	?HB	@NB	:*B	,�B	bB	>B	1B	iB	�B��B��B��B�B�3B��B��BͨB�xB�/B��B��B��B��B��B��B�zB�gB�OB�1B� B��Bz�Bx�By�Bz�Bx�Bs�BliBiXBa'B]B\	BQ�BM�BH�BF�BE�BE�BCxB=TB<NB:AB9;B9;B86B70B70B6*B4B4B0B2B-�B,�B-�B+�B+�B*�B)�B)�B(�B&�B%�B%�B#�B"�B"�B"�B�B�B�B�B�B�B!�B"�B$�B%�B%�B&�B&�B&�B'�B&�B&�B+�B/B/B/B0	B/B/B/B0	B3B3B4!B3B3B2B6.B9@B;MB?eB@kB?eB?eB?eB>_B>_B=ZB=ZBArBC~BC~BCBE�BD�BE�BF�BF�BG�BG�BH�BK�BN�BP�BX�B[B_$Bb6BhZBjfBnBp�Br�Bt�Bv�Bz�B~�B��B��B�B�B�$B�<B�HB�HB�aB�rB�yB��B��B��B��B��B��B��B��B��B�B�B�$B�/B�;B�ZB�`B�lB�sB�sB�yB�BŅBɝB��B��B��B��B��B��B��B��B�B�B�B�B�/B�HB�TB�`B��B��B��B	B	SB	SB	_B	kB	rB	xB	~B	�B	%�B	*�B	,�B	1B	5B	6"B	5B	7(B	8.B	94B	;AB	<GB	=MB	?YB	@_B	@_B	BlB	CrB	DxB	F�B	G�B	H�B	H�B	I�B	K�B	L�B	L�B	L�B	K�B	L�B	N�B	P�B	Q�B	U�B	T�B	T�B	V�B	Y�B	Y�B	V�B	U�B	V�B	U�B	U�B	W�B	Y�B	Z�B	^B	^B	_B	a#B	gGB	iSB	k_B	mlB	p~B	s�B	u�B	w�B	w�B	|�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�
B	�"B	�:B	�SB	�_B	�lB	�rB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�4B	�;B	�;B	�AB	�GB	�MB	�eB	�}B	�}B	ǃB	ȉB	˜B	кB	��G�O�B	�$B	�B	��B
�B
�B
�B
B
'�B
0&B
9�B
>�B
E	B
KB
N�B
R�B
W�B
\�B
aB
e�B
g11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.115 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.008(+/-0.002) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144132022020411441320220204114413  AO  ARCAADJP                                                                    20200619170900    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170900  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170900  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114413  IP                  G�O�G�O�G�O�                