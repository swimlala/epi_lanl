CDF   	   
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  -   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2022-09-05T11:10:03Z creation; 2023-02-10T23:09:45Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.2   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    8   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  8   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  8�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  9   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        9x   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    9�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    9�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  9�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    9�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    9�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  9�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  :   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  :T   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    :�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   axis      T      
resolution        >�E�r�_K   
_FillValue        A.�~            :�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    :�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        ?F�k"kmj   
_FillValue        A.�~            :�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   	valid_min         �V�        	valid_max         @V�        axis      Y      
_FillValue        @�i�            :�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    	valid_min         �f�        	valid_max         @f�        axis      X      
_FillValue        @�i�            :�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    :�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    :�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    :�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    ;    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        =    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     h  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 \  Vp   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     h  \�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 \  v4   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     h  |�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     h  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 \  �`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     h  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 \  �$   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     h  Հ   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     h  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 \ P   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     h �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 \ (   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     h .p   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` G�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   H8   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   N8   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   T8   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T Z8   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   Z�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   Z�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   Z�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   Z�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � Z�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   [,   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   [H   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    [P   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        [p   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        [x   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       [�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    [�Argo profile    3.1 1.2 19500101000000  20220905111003  20230210230945  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_227                 6810_008521_227                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @��X5�Q/@��X5�Q/11  @��Xg8~@��Xg8~@2���6�@2���6��d��L�Pr�d��L�Pr11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  @   @B�\@�  @��R@�  @�  A   A��A ��A+�A@��A`��A���A���A�Q�A���A�Q�A�  A��A���B   B  B  B�B�B'�B/�B8(�B@  BH  BO�
BX  B`Q�Bh(�Bp(�Bx(�B�{B�{B�{B��B�  B�  B�  B�  B�  B��B�  B�  B��B��B�  B�  B��B�{B�(�B�  B�  B�  B��B��
B��B�{B�{B��B��B�  B�{B�{C 
=C{C��C��C{C
  C��C  C
=C{C�C�C{C  C��C  C   C"  C$
=C&  C(  C)��C+�C-��C0  C2  C3��C6  C8  C9��C;��C>
=C@  CB  CC��CE��CH
=CI��CL
=CN
=CP  CR  CT  CU��CW��CZ  C[��C]��C_��Ca��Cd
=Cf  Ch  Ci�Ck�Cn
=Cp
=Cq��Ct  Cv  Cx
=Cz  C{��C~  C�C�C���C�  C���C���C�  C�  C���C�C�  C���C�  C���C�  C�C�C���C�  C�
=C�  C�  C�  C���C���C�  C�  C�C�  C���C�C�  C�C�  C���C���C�  C�  C�C�C�  C���C�  C�  C�C�C�
=C�
=C�  C�C�
=C�
=C���C�  C�  C�  C�C���C���C���C�  C�
=C�C�  C�  C�C�
=C�
=C�C�
=C�C�  C�  C�  C���C���C���C�  C���C���C���C���C���C�  C�C�  C�C�C���C���C���C���C�  C�C�C�  C�  C�  C���C���C���C���C���C�C�  C���C�C�  C�  C�  C���C�  C�  C�  C�  C�  C�  C�C���C���C�C�C���C�  C�  C���C���C�  C���D }qD �qD� D�qDz�D�qD��D  D� D�D� D�qD� D  D}qD�qD� D	  D	� D
  D
}qD
�qD}qD�qD� D�qD� DD��D  D� D  D� D  D}qD  D��DD��D�D��D  D��D�qD}qD  D��D�D��D  D��D  D}qD��D� DD��D�qDz�D  D�DD��D �D }qD �qD!��D"�D"� D"�qD#}qD$�D$}qD%  D%�D&�D&��D'�D'� D(�D(��D)  D)}qD*�D*��D*�qD+� D,�D,� D,�qD-� D-�qD.}qD/�D/��D0  D0� D1�D1� D2  D2� D2�qD3� D4  D4� D5  D5� D6�D6��D7  D7��D7�qD8� D9�D9��D:D:� D:�qD;� D<  D<� D=  D=� D>  D>}qD?�D?� D@  D@��DA  DA}qDB  DB��DC  DC��DD  DD}qDE  DE��DE��DF}qDG  DG}qDH�DH�DI�DI��DJ�DJ��DJ�qDK� DLDL��DM  DM}qDM�qDN}qDO  DO}qDO��DPz�DP��DQ� DQ�qDR� DR�qDS}qDT  DT� DU�DU��DU��DVz�DW  DW� DW�qDX� DYDY��DY��DZz�DZ�qD[� D\�D\��D]  D]� D^�D^��D_  D_}qD`  D`��Da�Da��Db�Db}qDb��Dc}qDd  Dd� Dd�qDe� Df  Df� Df�qDg� Dh�Dh� Dh��Diz�Di��Djz�Dk  Dk��Dk�qDl��Dm�Dm� Dn  Dn� Do  Do}qDo�qDp��DqDq��Dr  Dr}qDs  Ds��Dt  Dt� DuDu��Dv�Dv� Dw  Dw�DxDx�Dx�qDyz�Dz�Dz�Dz�qD{� D|�D|}qD}�D}� D}��D~z�D~�qD� D��D�@ D��HD�� D�  D�AHD���D��HD�HD�@ D��HD��HD�  D�=qD�~�D��HD�  D�>�D��HD�� D�HD�B�D���D�� D���D�@ D�� D���D�  D�@ D�}qD��HD�HD�=qD��HD���D��D�AHD�� D��HD�  D�@ D��HD�� D�  D�AHD��HD��HD�  D�@ D�� D�� D�  D�>�D�~�D�� D���D�@ D��HD�D��D�AHD�� D���D���D�@ D�� D���D�  D�@ D�� D��HD�HD�@ D�� D�� D�HD�@ D�� D��HD�HD�AHD�� D�� D�  D�@ D�~�D�� D�  D�@ D�� D�� D�  D�AHD�~�D���D�  D�@ D�~�D���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�G�?\)?u?���?\?�@�@��@.{@@  @W
=@fff@}p�@��@�33@��H@�ff@�33@���@Ǯ@�\)@�(�@��
@��@��HA�A�A
�HAG�AA�HA\)A#�
A(��A,(�A2�\A5A:�HA@  AC33AH��AMp�AP��AW�AZ=qA_\)Ac33AhQ�An{Ap��AvffAy��A�  A���A��
A��RA�  A�33A�z�A�
=A���A�33A�ffA��A�=qA���A��RA�G�A��HA�A��A��\A�(�A�
=A�G�A�33A�ffA�  A��HA��A�\)A\A�(�A�\)A���A�(�A�A���Aҏ\A�A׮A��A��A޸RA��A�33A�ffA�  A�33A���A�  A��A�(�A�\)A���A�(�A�{B Q�BB�\B(�B�BffB�
B��B
ffB33B��BB33BQ�BG�B
=B  BG�B�RB�BG�B�B�Bz�B{B
=B z�B!p�B"�HB$(�B%�B&�\B(  B(��B*=qB+�B,z�B-�B/\)B0(�B1B2�\B4(�B5G�B6=qB8  B8��B:=qB;�B<z�B>{B?
=B@(�BABB�RBD(�BEG�BF�\BG�
BH��BJ=qBK33BL(�BMBNffBP(�BP��BR=qBS\)BTQ�BUp�BW
=BW�
BY�BZ�\B[�B\��B^{B_
=B`Q�Ba��BbffBc�Bd��Be�Bg�Bhz�Bip�Bk
=Bk�Bm�Bn�\Bo\)Bp��Br{Br�HBtz�Bup�Bv�\Bx(�Bx��BzffB{\)B|��B~{B~�HB�(�B��RB��B��B�ffB���B��B�(�B��RB��B��B���B�33B��B�z�B���B�p�B�=qB��HB�G�B��
B���B�
=B��B�Q�B��RB�p�B�{B�ffB�33B�B�(�B��HB�p�B��B��RB��B��B�z�B��RB�p�B�  B�ffB��B�B�  B��RB�G�B���B�Q�B��HB�33B��B�ffB���B���B�  B�z�B�33B���B�(�B���B�33B��
B�ffB���B��B�{B�z�B�33B�B�(�B��HB�\)B��
B��\B��B��B�=qB��RB��B��
B�ffB��RB�p�B�  B�ffB��B��B�(�B��HB�G�B��B�z�B��HB�G�B�{B�z�B���B��B�{B���B�G�B��B�=qB��HB�G�B�B�z�B���B�G�B�{B�z�B��HB���B�(�B�z�B�33B��B�{B£�B�G�BîB�(�B���B�\)B�B�ffB�
=B�p�B�  Bȣ�B�
=Bə�B�Q�B���B�G�B��B�z�B���B�p�B�(�BΣ�B�
=B��
B�=qBиRB�p�B�  B�z�B���B�B�=qBԣ�B�G�B�  B�z�B��HBׅB�(�B؏\B�
=BٮB�Q�BڸRB�33B��
B�ffB���B�p�B�{B�ffB�
=B߮B�  B��B�G�BᙚB�(�B��HB�G�B�B�ffB���B�p�B��B�RB��B�B�=qB��HB�33B��
B�z�B��HB�\)B�{B��B�
=B��B�ffB��HB�\)B��B�RB�33B�B�Q�B�
=B�B�  B��RB�p�B��
B�z�B�33B��B�(�B���B�p�B��B��RB�33B���B�ffB�
=B�p�B�  B���B�G�B�C G�C ��C ��C�C�CC
=CffC�RC��CG�C�C�C(�Cz�C�
C(�C\)C�C�C\)C��C  C\)C�\C�HCG�C�\C�
C	�C	z�C	��C

=C
\)C
�RC  C=qC��C��C=qC�\C��C33C�C�C33Cz�C��C33C�CC
=Cz�C��C
=Cp�CC  CffCC  CQ�C�C  C33C��C�HC�C�CC
=C\)C��C�
C�Cp�C��C��C{C\)C�\C�C�C�C\)C�C��C�
C
=C{CQ�C�C��C��C
=C�C=qC�C��CC  C33C=qCp�C�C��C�C(�C\)Cz�C�\C��C  C�C=qCz�C�C��C�HC�C=qC\)C��CC�
C {C =qC Q�C z�C �C �HC ��C!�C!\)C!p�C!�\C!��C!�HC"  C"=qC"\)C"p�C"�RC"�
C"��C#(�C#\)C#ffC#��C#��C#�HC$  C$=qC$ffC$�\C$��C$C$��C%(�C%Q�C%ffC%�C%��C%��C&  C&(�C&=qC&\)C&�\C&�RC&��C&��C'�C'G�C'\)C'p�C'�C'�
C'�C(  C((�C(Q�C(z�C(�C(��C(�HC)
=C){C)33C)\)C)�\C)�RC)C)�C*(�C*=qC*\)C*z�C*��C*��C*��C+
=C+(�C+Q�C+�C+�C+��C+�C,{C,G�C,p�C,�C,�C,�HC-{C-33C-G�C-p�C-�C-�
C.  C.{C.=qC.p�C.��C.�C.�
C/  C/33C/\)C/z�C/��C/�
C0
=C0�C0=qC0p�C0��C0�
C0�C1
=C1G�C1z�C1�\C1�C1�
C2{C233C2Q�C2p�C2��C2�HC3
=C3(�C3=qC3ffC3��C3��C3�C4
=C4(�C4ffC4��C4C4�HC5
=C533C5p�C5��C5�RC5�HC6�C6Q�C6�C6��C6��C7
=C7=qC7\)C7z�C7C7��C8�C8=qC8ffC8�\C8�
C8��C9
=C9=qC9p�C9��C9�RC9�HC:�C:Q�C:ffC:�C:�RC:��C;�C;=qC;\)C;�C;C;��C<�C<=qC<ffC<�\C<�
C=  C=(�C=Q�C=�\C=��C>  C>{C>G�C>�\C>C>�C?
=C?33C?z�C?�C?�HC@
=C@(�C@ffC@��C@�
CA  CA(�CA\)CA��CA�
CB  CB�CBQ�CB�\CB�
CC  CC�CCQ�CC�CCCC��CD�CDG�CDp�CD��CD�CE�CEG�CEz�CE��CECE��CF33CFp�CF��CFCF�CG33CGffCG��CGCG�H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                    111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ?�  @   @B�\@�  @��R@�  @�  A   A��A ��A+�A@��A`��A���A���A�Q�A���A�Q�A�  A��A���B   B  B  B�B�B'�B/�B8(�B@  BH  BO�
BX  B`Q�Bh(�Bp(�Bx(�B�{B�{B�{B��B�  B�  B�  B�  B�  B��B�  B�  B��B��B�  B�  B��B�{B�(�B�  B�  B�  B��B��
B��B�{B�{B��B��B�  B�{B�{C 
=C{C��C��C{C
  C��C  C
=C{C�C�C{C  C��C  C   C"  C$
=C&  C(  C)��C+�C-��C0  C2  C3��C6  C8  C9��C;��C>
=C@  CB  CC��CE��CH
=CI��CL
=CN
=CP  CR  CT  CU��CW��CZ  C[��C]��C_��Ca��Cd
=Cf  Ch  Ci�Ck�Cn
=Cp
=Cq��Ct  Cv  Cx
=Cz  C{��C~  C�C�C���C�  C���C���C�  C�  C���C�C�  C���C�  C���C�  C�C�C���C�  C�
=C�  C�  C�  C���C���C�  C�  C�C�  C���C�C�  C�C�  C���C���C�  C�  C�C�C�  C���C�  C�  C�C�C�
=C�
=C�  C�C�
=C�
=C���C�  C�  C�  C�C���C���C���C�  C�
=C�C�  C�  C�C�
=C�
=C�C�
=C�C�  C�  C�  C���C���C���C�  C���C���C���C���C���C�  C�C�  C�C�C���C���C���C���C�  C�C�C�  C�  C�  C���C���C���C���C���C�C�  C���C�C�  C�  C�  C���C�  C�  C�  C�  C�  C�  C�C���C���C�C�C���C�  C�  C���C���C�  C���D }qD �qD� D�qDz�D�qD��D  D� D�D� D�qD� D  D}qD�qD� D	  D	� D
  D
}qD
�qD}qD�qD� D�qD� DD��D  D� D  D� D  D}qD  D��DD��D�D��D  D��D�qD}qD  D��D�D��D  D��D  D}qD��D� DD��D�qDz�D  D�DD��D �D }qD �qD!��D"�D"� D"�qD#}qD$�D$}qD%  D%�D&�D&��D'�D'� D(�D(��D)  D)}qD*�D*��D*�qD+� D,�D,� D,�qD-� D-�qD.}qD/�D/��D0  D0� D1�D1� D2  D2� D2�qD3� D4  D4� D5  D5� D6�D6��D7  D7��D7�qD8� D9�D9��D:D:� D:�qD;� D<  D<� D=  D=� D>  D>}qD?�D?� D@  D@��DA  DA}qDB  DB��DC  DC��DD  DD}qDE  DE��DE��DF}qDG  DG}qDH�DH�DI�DI��DJ�DJ��DJ�qDK� DLDL��DM  DM}qDM�qDN}qDO  DO}qDO��DPz�DP��DQ� DQ�qDR� DR�qDS}qDT  DT� DU�DU��DU��DVz�DW  DW� DW�qDX� DYDY��DY��DZz�DZ�qD[� D\�D\��D]  D]� D^�D^��D_  D_}qD`  D`��Da�Da��Db�Db}qDb��Dc}qDd  Dd� Dd�qDe� Df  Df� Df�qDg� Dh�Dh� Dh��Diz�Di��Djz�Dk  Dk��Dk�qDl��Dm�Dm� Dn  Dn� Do  Do}qDo�qDp��DqDq��Dr  Dr}qDs  Ds��Dt  Dt� DuDu��Dv�Dv� Dw  Dw�DxDx�Dx�qDyz�Dz�Dz�Dz�qD{� D|�D|}qD}�D}� D}��D~z�D~�qD� D��D�@ D��HD�� D�  D�AHD���D��HD�HD�@ D��HD��HD�  D�=qD�~�D��HD�  D�>�D��HD�� D�HD�B�D���D�� D���D�@ D�� D���D�  D�@ D�}qD��HD�HD�=qD��HD���D��D�AHD�� D��HD�  D�@ D��HD�� D�  D�AHD��HD��HD�  D�@ D�� D�� D�  D�>�D�~�D�� D���D�@ D��HD�D��D�AHD�� D���D���D�@ D�� D���D�  D�@ D�� D��HD�HD�@ D�� D�� D�HD�@ D�� D��HD�HD�AHD�� D�� D�  D�@ D�~�D�� D�  D�@ D�� D�� D�  D�AHD�~�D���D�  D�@ D�~�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�G�?\)?u?���?\?�@�@��@.{@@  @W
=@fff@}p�@��@�33@��H@�ff@�33@���@Ǯ@�\)@�(�@��
@��@��HA�A�A
�HAG�AA�HA\)A#�
A(��A,(�A2�\A5A:�HA@  AC33AH��AMp�AP��AW�AZ=qA_\)Ac33AhQ�An{Ap��AvffAy��A�  A���A��
A��RA�  A�33A�z�A�
=A���A�33A�ffA��A�=qA���A��RA�G�A��HA�A��A��\A�(�A�
=A�G�A�33A�ffA�  A��HA��A�\)A\A�(�A�\)A���A�(�A�A���Aҏ\A�A׮A��A��A޸RA��A�33A�ffA�  A�33A���A�  A��A�(�A�\)A���A�(�A�{B Q�BB�\B(�B�BffB�
B��B
ffB33B��BB33BQ�BG�B
=B  BG�B�RB�BG�B�B�Bz�B{B
=B z�B!p�B"�HB$(�B%�B&�\B(  B(��B*=qB+�B,z�B-�B/\)B0(�B1B2�\B4(�B5G�B6=qB8  B8��B:=qB;�B<z�B>{B?
=B@(�BABB�RBD(�BEG�BF�\BG�
BH��BJ=qBK33BL(�BMBNffBP(�BP��BR=qBS\)BTQ�BUp�BW
=BW�
BY�BZ�\B[�B\��B^{B_
=B`Q�Ba��BbffBc�Bd��Be�Bg�Bhz�Bip�Bk
=Bk�Bm�Bn�\Bo\)Bp��Br{Br�HBtz�Bup�Bv�\Bx(�Bx��BzffB{\)B|��B~{B~�HB�(�B��RB��B��B�ffB���B��B�(�B��RB��B��B���B�33B��B�z�B���B�p�B�=qB��HB�G�B��
B���B�
=B��B�Q�B��RB�p�B�{B�ffB�33B�B�(�B��HB�p�B��B��RB��B��B�z�B��RB�p�B�  B�ffB��B�B�  B��RB�G�B���B�Q�B��HB�33B��B�ffB���B���B�  B�z�B�33B���B�(�B���B�33B��
B�ffB���B��B�{B�z�B�33B�B�(�B��HB�\)B��
B��\B��B��B�=qB��RB��B��
B�ffB��RB�p�B�  B�ffB��B��B�(�B��HB�G�B��B�z�B��HB�G�B�{B�z�B���B��B�{B���B�G�B��B�=qB��HB�G�B�B�z�B���B�G�B�{B�z�B��HB���B�(�B�z�B�33B��B�{B£�B�G�BîB�(�B���B�\)B�B�ffB�
=B�p�B�  Bȣ�B�
=Bə�B�Q�B���B�G�B��B�z�B���B�p�B�(�BΣ�B�
=B��
B�=qBиRB�p�B�  B�z�B���B�B�=qBԣ�B�G�B�  B�z�B��HBׅB�(�B؏\B�
=BٮB�Q�BڸRB�33B��
B�ffB���B�p�B�{B�ffB�
=B߮B�  B��B�G�BᙚB�(�B��HB�G�B�B�ffB���B�p�B��B�RB��B�B�=qB��HB�33B��
B�z�B��HB�\)B�{B��B�
=B��B�ffB��HB�\)B��B�RB�33B�B�Q�B�
=B�B�  B��RB�p�B��
B�z�B�33B��B�(�B���B�p�B��B��RB�33B���B�ffB�
=B�p�B�  B���B�G�B�C G�C ��C ��C�C�CC
=CffC�RC��CG�C�C�C(�Cz�C�
C(�C\)C�C�C\)C��C  C\)C�\C�HCG�C�\C�
C	�C	z�C	��C

=C
\)C
�RC  C=qC��C��C=qC�\C��C33C�C�C33Cz�C��C33C�CC
=Cz�C��C
=Cp�CC  CffCC  CQ�C�C  C33C��C�HC�C�CC
=C\)C��C�
C�Cp�C��C��C{C\)C�\C�C�C�C\)C�C��C�
C
=C{CQ�C�C��C��C
=C�C=qC�C��CC  C33C=qCp�C�C��C�C(�C\)Cz�C�\C��C  C�C=qCz�C�C��C�HC�C=qC\)C��CC�
C {C =qC Q�C z�C �C �HC ��C!�C!\)C!p�C!�\C!��C!�HC"  C"=qC"\)C"p�C"�RC"�
C"��C#(�C#\)C#ffC#��C#��C#�HC$  C$=qC$ffC$�\C$��C$C$��C%(�C%Q�C%ffC%�C%��C%��C&  C&(�C&=qC&\)C&�\C&�RC&��C&��C'�C'G�C'\)C'p�C'�C'�
C'�C(  C((�C(Q�C(z�C(�C(��C(�HC)
=C){C)33C)\)C)�\C)�RC)C)�C*(�C*=qC*\)C*z�C*��C*��C*��C+
=C+(�C+Q�C+�C+�C+��C+�C,{C,G�C,p�C,�C,�C,�HC-{C-33C-G�C-p�C-�C-�
C.  C.{C.=qC.p�C.��C.�C.�
C/  C/33C/\)C/z�C/��C/�
C0
=C0�C0=qC0p�C0��C0�
C0�C1
=C1G�C1z�C1�\C1�C1�
C2{C233C2Q�C2p�C2��C2�HC3
=C3(�C3=qC3ffC3��C3��C3�C4
=C4(�C4ffC4��C4C4�HC5
=C533C5p�C5��C5�RC5�HC6�C6Q�C6�C6��C6��C7
=C7=qC7\)C7z�C7C7��C8�C8=qC8ffC8�\C8�
C8��C9
=C9=qC9p�C9��C9�RC9�HC:�C:Q�C:ffC:�C:�RC:��C;�C;=qC;\)C;�C;C;��C<�C<=qC<ffC<�\C<�
C=  C=(�C=Q�C=�\C=��C>  C>{C>G�C>�\C>C>�C?
=C?33C?z�C?�C?�HC@
=C@(�C@ffC@��C@�
CA  CA(�CA\)CA��CA�
CB  CB�CBQ�CB�\CB�
CC  CC�CCQ�CC�CCCC��CD�CDG�CDp�CD��CD�CE�CEG�CEz�CE��CECE��CF33CFp�CF��CFCF�CG33CGffCG��CGCG�H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                    111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�A�Q�A�ffA�jA�jA�jA�hsA�jA�jA�hsA�hsA�hsA�jA�bNA�^5A�dZA�^5A�E�A�7LA�1'A�(�A�VA��A���A���A�ƨA�ĜAܾwAܼjAܸRAܰ!AܬAܝ�A�t�A�$�A���A�"�Aڰ!AڑhA�M�A�9XA�"�A�
=A��A٥�A�hsA�%A؁A���A�A�A��TA�|�A� �A�x�A�ƨA�ZA���A�ȴA�dZA��A�`BA�1Aˉ7A�"�A�x�A���A�?}A�Q�AƶFA�33Aũ�A��/A�(�Aå�A´9A���A�A��uA��TA�|�A�-A��A��PA��A�G�A�1'A�oA�Q�A���A��!A�ĜA���A�|�A��;A�x�A�  A�$�A���A�A��-A�|�A�ƨA�\)A�I�A���A�~�A�33A��jA���A�p�A�p�A���A���A���A��DA�A�x�A�ȴA�-A��A��A���A���A��\A���A��PA���A�?}A�bA}��Ay��Asl�AnQ�AmAi�FAg��Afv�Ab9XA`z�A_�A^VA[�TAYdZAT�RAQC�AN�9AMO�AL��AK;dAF^5AB�/AA�A@ZA?�hA?&�A>$�A=��A=�A<ZA<�A<1A;�A:�9A9oA5ƨA5\)A4�RA3p�A1�A/�PA-+A,�!A,�A+;dA)�A'�A%�-A#��A"Q�A!�A!�^A ��A E�A�uA�;A�-AS�A5?Ax�A��A�A��AdZA�+A(�AƨA��A�9An�A��A�Av�AG�A��AȴAZAE�A�A/A	��A��AZA��A�FA�-A��AA(�A�-AS�A��A��A��A�RA�A�A v�@�~�@��@�p�@�Q�@�+@�p�@�X@�@�r�@�@�E�@�$�@�J@�R@�@��@��T@��@��T@陚@�"�@�Z@�7L@�bN@�9X@�=q@�&�@��#@�9X@�b@�ƨ@�@��#@��/@ӕ�@���@���@҇+@��T@���@�  @�C�@�v�@�{@ͩ�@�/@�b@�V@�-@���@��/@�@�M�@���@���@���@�33@��H@�$�@��T@�?}@�bN@��@ǍP@�dZ@�K�@���@ƸR@�5?@��@ũ�@�X@���@�9X@�1@�1@�S�@�ȴ@�{@���@�p�@�%@��@��/@��/@��`@���@�Q�@��@�t�@�
=@��y@���@�V@��@��-@�&�@�j@�ƨ@�+@���@��R@��\@�^5@��@���@�&�@��@���@�I�@���@��;@�33@���@���@�&�@���@�Ĝ@�Ĝ@���@���@���@��j@��j@��@���@��D@���@��H@���@�=q@�@�G�@�V@��u@�Z@��@�\)@���@��H@���@�v�@�M�@�E�@�=q@�$�@��7@�hs@�`B@�G�@�%@���@�A�@�  @���@��w@���@�o@��@��H@���@�ff@��T@��-@�`B@��`@���@��D@�Q�@��@��@��@�K�@��H@�@���@��h@�`B@�?}@��@��/@�Ĝ@��9@���@�Q�@�(�@��w@�33@��y@��+@�=q@�J@���@�O�@��@�Ĝ@�1'@�1@��;@��@���@�t�@�\)@��@���@���@�-@��-@�/@��@��`@�Q�@� �@���@�|�@�o@���@�
=@�
=@�
=@��H@��!@�^5@���@��7@�`B@�?}@���@� �@��m@��@�\)@�33@��@��\@�~�@�n�@�V@�-@��#@��h@�p�@�O�@�%@���@��9@�r�@��;@���@��P@��@�t�@�l�@�"�@��R@�~�@�V@�=q@��@���@��@�7L@�&�@�V@���@���@���@�Z@�(�@�1@���@��@�|�@�K�@��@��@���@�~�@�n�@�M�@�-@��T@���@�7L@���@�z�@� �@���@��
@��P@�S�@�+@�@��@���@���@�~�@�V@�5?@�$�@�{@��@��h@�O�@�%@��@�Ĝ@�z�@�Q�@�Q�@�Q�@�I�@�1'@�ƨ@�|�@��@���@���@��+@�n�@�@���@���@�x�@�&�@���@��@�A�@�1'@��
@�"�@�ȴ@��!@���@�v�@�-@��T@�@���@�hs@�7L@��@���@��`@�Ĝ@�r�@�Z@�A�@�(�@�1@�@~$�@}�@|��@|�D@|9X@{�m@{�@{S�@{o@z��@z~�@z�@y�^@yG�@x��@xA�@w�@w|�@wl�@w;d@w�@v�@vff@u�@uO�@t�@tj@s�m@s�@s33@r�H@r�!@r-@q�@q��@pr�@o�@o;d@n�y@n�+G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�VA�O�A�S�A�K�A�`BA�jA�bNA�dZA�ffA�jA�l�A�hsA�n�A�ffA�n�A�jA�jA�l�A�ffA�jA�ffA�n�A�hsA�n�A�jA�jA�jA�ffA�l�A�ffA�jA�hsA�hsA�jA�ffA�n�A�jA�l�A�l�A�ffA�l�A�hsA�dZA�jA�bNA�ffA�`BA�^5A�`BA�\)A�bNA�ZA�^5A�ZA�`BA�bNA�bNA�ffA�`BA�dZA�dZA�bNA�hsA�bNA�dZA�ZA�VA�bNA�^5A�bNA�ZA�^5A�O�A�=qA�?}A�;dA�;dA�7LA�=qA�=qA�;dA�;dA�1'A�1'A�1'A�7LA�/A�33A�/A�33A�-A�-A�1'A�-A�-A�$�A�$�A� �A��A�bA�bA�
=A�1A�%A�A�A���A��mA��#A���A��/A��A��#A���A�ƨA���A�ƨA���A�ƨA���A���A�ȴA���A�ƨA�ȴA�ȴA�ĜA���A�ĜA���A�ĜA�ȴA�A�ĜA���A���A�AܼjA���A�AܼjA���AܾwAܺ^AܾwA���Aܺ^A���Aܺ^A���AܸRAܶFAܺ^Aܴ9AܶFAܴ9AܮAܴ9Aܰ!AܮAܲ-AܮAܰ!AܮAܰ!AܮAܩ�AܬAܧ�Aܥ�Aܧ�Aܟ�Aܟ�Aܕ�Aܙ�Aܕ�A܉7A܁A�x�A�p�A�n�A�n�A�dZA�VA�;dA�"�A��A��A�bA�JA�  A��`A��A���AۼjAۮAۛ�AۍPA�\)A��A��A��yA��/A��AڶFAڮAڮAڥ�AڬAڧ�Aک�Aڣ�AړuAړuAڏ\A�|�A�r�A�dZA�ZA�K�A�C�A�A�A�C�A�=qA�A�A�=qA�7LA�9XA�33A�1'A�33A�/A�&�A�$�A� �A�{A�bA�oA�VA�{A�{A�A�A���A��A��mA��HA��
A��
A���A�ĜA���AٮA٬A٧�Aٟ�Aٝ�AّhAٓuAى7A�n�A�jA�hsA�hsA�M�A�;dA�7LA�/A�&�A�oA�A��A�A؝�AؑhA؉7A؋DA؁A�l�A�hsA�ffA�^5A�=qA���A��`A׾wAן�A׋DA�|�A�hsA�7LA�1'A�(�A� �A��A�bA�A���A��A��;A���A֝�A�ZA���A՛�A�v�A�?}A�/A��A��A���A�z�A�7LA��AӴ9AӶFAӬAӧ�Aӣ�Aӗ�AӍPA�z�A�O�A�-A��A���A��
A�AҸRAҧ�Aҧ�Aҙ�Aҏ\A҃A�|�A�x�A�hsA�XA�=qA�A�v�A�E�A�1A���Aа!A�~�A�=qA���A��HA���A���A���Aϴ9Aϩ�AϬAϛ�AσA�n�A�jA�M�A� �A�JA��yA�v�A�1'A�bA��A�v�A�bNA�bNA�bNA�ZA�`BA�`BA�`BA�dZA�hsA�^5A�S�A�E�A��HȂhA�p�A�\)A�oA�ƨA˛�A�dZA�A�A�"�A�%A��Aʰ!A�|�A�-A��AɼjAɝ�Aɉ7Aɉ7AɃA�|�A�~�A�x�A�jA�\)A�33A�
=A���A���A��mA���AȬAȕ�A�jA�O�A�5?A�(�A�bAǸRAǇ+A�jA�VA�G�A�;dA�1'A��A�A��/A�Aƴ9Aƙ�A�z�A�l�A�bNA�S�A�;dA�(�A��A�A��yA��;A���AŰ!Aŉ7AŁA�v�A�O�A�+A�A��#AľwAě�A�VA�G�A�9XA�"�A��A�{A�bA�A��A��AîA�v�A�A�A��A��A��;A®A�ADA�p�A�ZA�I�A�;dA�  A���A�r�A�?}A�&�A�bA���A��A��TA��/A���A��jA��9A���A��\A�VA�%A���A���A��A���A�ȴA���A���A��7A�~�A�|�A�dZA�5?A�JA��A��jA�(�A��A���A�p�A�ZA�33A�$�A�A���A���A���A�A��FA��PA��\A��A�~�A�M�A�5?A�"�A� �A��A�VA�A���A��#A�ƨA�ĜA��-A��-A��FA���A���A���A�n�A�r�A�l�A�9XA�$�A�VA��A��jA���A��+A�n�A�\)A�K�A�C�A�;dA�-A��A�JA���A��A���A��9A���A�x�A�5?A�bA���A��A��HA���A�ȴA�ĜA��^A��9A��!A��-A���A���A���A���A��uA��PA�G�A���A���A��uA�jA�=qA���A���A��9A��uA��PA��7A�ffA�XA�S�A�O�A�A�A�9XA�+A��A�VA�A��A��TA��;A�ȴA��^A���A��A�A���A��-A��7A�p�A�O�A�-A�1A��A��#A���A��A�r�A�\)A�5?A�+A�bA�VA�A��yA���A���A�ƨA��-A���A���A�x�A�v�A�t�A�v�A�l�A�\)A�9XA�1'A��A�1A��A��DA��-A�33A���A�p�A��A�oA�bA�oA���A���A���A���A���A��A��TA�ƨA���A��RA��A���A���A���A��A���A���A��uA��PA��+A�v�A�C�A�7LA�5?A�1'A��A��A��A��A�oA��A���A���A��A��TA��HA��mA��HA��A���A��wA��A���A�z�A�%A��A��A�|�A�l�A�1'A��yA�v�A�33A���A��/A��jA��hA�~�A�hsA�\)A�O�A�?}A�&�A��A�{A�1A���A���A��A��A��`A��TA��;A��A���A���A�ȴA��^A��A���A���A���A���A��PA��A�O�A�A��A�G�A���A���A�7LA�v�A�9XA�VA�bA�JA�
=A��`A��A�7LA��A�bNA�-A�VA��A��jA��A���A���A��hA��+A�p�A�I�A�$�A�VA��mA��9A��A�hsA�O�A�I�A�5?A�-A�+A�+A�(�A��A��A�bA�VA�
=A���A��A��mA��
A���A���A��wA��RA��!A��uA��A�z�A�p�A�ffA�`BA�A�A� �A��`A��hA�ffA��
A��u11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                    111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�Q�A�ffA�jA�jA�jA�hsA�jA�jA�hsA�hsA�hsA�jA�bNA�^5A�dZA�^5A�E�A�7LA�1'A�(�A�VA��A���A���A�ƨA�ĜAܾwAܼjAܸRAܰ!AܬAܝ�A�t�A�$�A���A�"�Aڰ!AڑhA�M�A�9XA�"�A�
=A��A٥�A�hsA�%A؁A���A�A�A��TA�|�A� �A�x�A�ƨA�ZA���A�ȴA�dZA��A�`BA�1Aˉ7A�"�A�x�A���A�?}A�Q�AƶFA�33Aũ�A��/A�(�Aå�A´9A���A�A��uA��TA�|�A�-A��A��PA��A�G�A�1'A�oA�Q�A���A��!A�ĜA���A�|�A��;A�x�A�  A�$�A���A�A��-A�|�A�ƨA�\)A�I�A���A�~�A�33A��jA���A�p�A�p�A���A���A���A��DA�A�x�A�ȴA�-A��A��A���A���A��\A���A��PA���A�?}A�bA}��Ay��Asl�AnQ�AmAi�FAg��Afv�Ab9XA`z�A_�A^VA[�TAYdZAT�RAQC�AN�9AMO�AL��AK;dAF^5AB�/AA�A@ZA?�hA?&�A>$�A=��A=�A<ZA<�A<1A;�A:�9A9oA5ƨA5\)A4�RA3p�A1�A/�PA-+A,�!A,�A+;dA)�A'�A%�-A#��A"Q�A!�A!�^A ��A E�A�uA�;A�-AS�A5?Ax�A��A�A��AdZA�+A(�AƨA��A�9An�A��A�Av�AG�A��AȴAZAE�A�A/A	��A��AZA��A�FA�-A��AA(�A�-AS�A��A��A��A�RA�A�A v�@�~�@��@�p�@�Q�@�+@�p�@�X@�@�r�@�@�E�@�$�@�J@�R@�@��@��T@��@��T@陚@�"�@�Z@�7L@�bN@�9X@�=q@�&�@��#@�9X@�b@�ƨ@�@��#@��/@ӕ�@���@���@҇+@��T@���@�  @�C�@�v�@�{@ͩ�@�/@�b@�V@�-@���@��/@�@�M�@���@���@���@�33@��H@�$�@��T@�?}@�bN@��@ǍP@�dZ@�K�@���@ƸR@�5?@��@ũ�@�X@���@�9X@�1@�1@�S�@�ȴ@�{@���@�p�@�%@��@��/@��/@��`@���@�Q�@��@�t�@�
=@��y@���@�V@��@��-@�&�@�j@�ƨ@�+@���@��R@��\@�^5@��@���@�&�@��@���@�I�@���@��;@�33@���@���@�&�@���@�Ĝ@�Ĝ@���@���@���@��j@��j@��@���@��D@���@��H@���@�=q@�@�G�@�V@��u@�Z@��@�\)@���@��H@���@�v�@�M�@�E�@�=q@�$�@��7@�hs@�`B@�G�@�%@���@�A�@�  @���@��w@���@�o@��@��H@���@�ff@��T@��-@�`B@��`@���@��D@�Q�@��@��@��@�K�@��H@�@���@��h@�`B@�?}@��@��/@�Ĝ@��9@���@�Q�@�(�@��w@�33@��y@��+@�=q@�J@���@�O�@��@�Ĝ@�1'@�1@��;@��@���@�t�@�\)@��@���@���@�-@��-@�/@��@��`@�Q�@� �@���@�|�@�o@���@�
=@�
=@�
=@��H@��!@�^5@���@��7@�`B@�?}@���@� �@��m@��@�\)@�33@��@��\@�~�@�n�@�V@�-@��#@��h@�p�@�O�@�%@���@��9@�r�@��;@���@��P@��@�t�@�l�@�"�@��R@�~�@�V@�=q@��@���@��@�7L@�&�@�V@���@���@���@�Z@�(�@�1@���@��@�|�@�K�@��@��@���@�~�@�n�@�M�@�-@��T@���@�7L@���@�z�@� �@���@��
@��P@�S�@�+@�@��@���@���@�~�@�V@�5?@�$�@�{@��@��h@�O�@�%@��@�Ĝ@�z�@�Q�@�Q�@�Q�@�I�@�1'@�ƨ@�|�@��@���@���@��+@�n�@�@���@���@�x�@�&�@���@��@�A�@�1'@��
@�"�@�ȴ@��!@���@�v�@�-@��T@�@���@�hs@�7L@��@���@��`@�Ĝ@�r�@�Z@�A�@�(�@�1@�@~$�@}�@|��@|�D@|9X@{�m@{�@{S�@{o@z��@z~�@z�@y�^@yG�@x��@xA�@w�@w|�@wl�@w;d@w�@v�@vff@u�@uO�@t�@tj@s�m@s�@s33@r�H@r�!@r-@q�@q��@pr�@o�@o;d@n�yG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�VA�O�A�S�A�K�A�`BA�jA�bNA�dZA�ffA�jA�l�A�hsA�n�A�ffA�n�A�jA�jA�l�A�ffA�jA�ffA�n�A�hsA�n�A�jA�jA�jA�ffA�l�A�ffA�jA�hsA�hsA�jA�ffA�n�A�jA�l�A�l�A�ffA�l�A�hsA�dZA�jA�bNA�ffA�`BA�^5A�`BA�\)A�bNA�ZA�^5A�ZA�`BA�bNA�bNA�ffA�`BA�dZA�dZA�bNA�hsA�bNA�dZA�ZA�VA�bNA�^5A�bNA�ZA�^5A�O�A�=qA�?}A�;dA�;dA�7LA�=qA�=qA�;dA�;dA�1'A�1'A�1'A�7LA�/A�33A�/A�33A�-A�-A�1'A�-A�-A�$�A�$�A� �A��A�bA�bA�
=A�1A�%A�A�A���A��mA��#A���A��/A��A��#A���A�ƨA���A�ƨA���A�ƨA���A���A�ȴA���A�ƨA�ȴA�ȴA�ĜA���A�ĜA���A�ĜA�ȴA�A�ĜA���A���A�AܼjA���A�AܼjA���AܾwAܺ^AܾwA���Aܺ^A���Aܺ^A���AܸRAܶFAܺ^Aܴ9AܶFAܴ9AܮAܴ9Aܰ!AܮAܲ-AܮAܰ!AܮAܰ!AܮAܩ�AܬAܧ�Aܥ�Aܧ�Aܟ�Aܟ�Aܕ�Aܙ�Aܕ�A܉7A܁A�x�A�p�A�n�A�n�A�dZA�VA�;dA�"�A��A��A�bA�JA�  A��`A��A���AۼjAۮAۛ�AۍPA�\)A��A��A��yA��/A��AڶFAڮAڮAڥ�AڬAڧ�Aک�Aڣ�AړuAړuAڏ\A�|�A�r�A�dZA�ZA�K�A�C�A�A�A�C�A�=qA�A�A�=qA�7LA�9XA�33A�1'A�33A�/A�&�A�$�A� �A�{A�bA�oA�VA�{A�{A�A�A���A��A��mA��HA��
A��
A���A�ĜA���AٮA٬A٧�Aٟ�Aٝ�AّhAٓuAى7A�n�A�jA�hsA�hsA�M�A�;dA�7LA�/A�&�A�oA�A��A�A؝�AؑhA؉7A؋DA؁A�l�A�hsA�ffA�^5A�=qA���A��`A׾wAן�A׋DA�|�A�hsA�7LA�1'A�(�A� �A��A�bA�A���A��A��;A���A֝�A�ZA���A՛�A�v�A�?}A�/A��A��A���A�z�A�7LA��AӴ9AӶFAӬAӧ�Aӣ�Aӗ�AӍPA�z�A�O�A�-A��A���A��
A�AҸRAҧ�Aҧ�Aҙ�Aҏ\A҃A�|�A�x�A�hsA�XA�=qA�A�v�A�E�A�1A���Aа!A�~�A�=qA���A��HA���A���A���Aϴ9Aϩ�AϬAϛ�AσA�n�A�jA�M�A� �A�JA��yA�v�A�1'A�bA��A�v�A�bNA�bNA�bNA�ZA�`BA�`BA�`BA�dZA�hsA�^5A�S�A�E�A��HȂhA�p�A�\)A�oA�ƨA˛�A�dZA�A�A�"�A�%A��Aʰ!A�|�A�-A��AɼjAɝ�Aɉ7Aɉ7AɃA�|�A�~�A�x�A�jA�\)A�33A�
=A���A���A��mA���AȬAȕ�A�jA�O�A�5?A�(�A�bAǸRAǇ+A�jA�VA�G�A�;dA�1'A��A�A��/A�Aƴ9Aƙ�A�z�A�l�A�bNA�S�A�;dA�(�A��A�A��yA��;A���AŰ!Aŉ7AŁA�v�A�O�A�+A�A��#AľwAě�A�VA�G�A�9XA�"�A��A�{A�bA�A��A��AîA�v�A�A�A��A��A��;A®A�ADA�p�A�ZA�I�A�;dA�  A���A�r�A�?}A�&�A�bA���A��A��TA��/A���A��jA��9A���A��\A�VA�%A���A���A��A���A�ȴA���A���A��7A�~�A�|�A�dZA�5?A�JA��A��jA�(�A��A���A�p�A�ZA�33A�$�A�A���A���A���A�A��FA��PA��\A��A�~�A�M�A�5?A�"�A� �A��A�VA�A���A��#A�ƨA�ĜA��-A��-A��FA���A���A���A�n�A�r�A�l�A�9XA�$�A�VA��A��jA���A��+A�n�A�\)A�K�A�C�A�;dA�-A��A�JA���A��A���A��9A���A�x�A�5?A�bA���A��A��HA���A�ȴA�ĜA��^A��9A��!A��-A���A���A���A���A��uA��PA�G�A���A���A��uA�jA�=qA���A���A��9A��uA��PA��7A�ffA�XA�S�A�O�A�A�A�9XA�+A��A�VA�A��A��TA��;A�ȴA��^A���A��A�A���A��-A��7A�p�A�O�A�-A�1A��A��#A���A��A�r�A�\)A�5?A�+A�bA�VA�A��yA���A���A�ƨA��-A���A���A�x�A�v�A�t�A�v�A�l�A�\)A�9XA�1'A��A�1A��A��DA��-A�33A���A�p�A��A�oA�bA�oA���A���A���A���A���A��A��TA�ƨA���A��RA��A���A���A���A��A���A���A��uA��PA��+A�v�A�C�A�7LA�5?A�1'A��A��A��A��A�oA��A���A���A��A��TA��HA��mA��HA��A���A��wA��A���A�z�A�%A��A��A�|�A�l�A�1'A��yA�v�A�33A���A��/A��jA��hA�~�A�hsA�\)A�O�A�?}A�&�A��A�{A�1A���A���A��A��A��`A��TA��;A��A���A���A�ȴA��^A��A���A���A���A���A��PA��A�O�A�A��A�G�A���A���A�7LA�v�A�9XA�VA�bA�JA�
=A��`A��A�7LA��A�bNA�-A�VA��A��jA��A���A���A��hA��+A�p�A�I�A�$�A�VA��mA��9A��A�hsA�O�A�I�A�5?A�-A�+A�+A�(�A��A��A�bA�VA�
=A���A��A��mA��
A���A���A��wA��RA��!A��uA��A�z�A�p�A�ffA�`BA�A�A� �A��`A��hA�ffA��
A��u11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                    111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��B
�B
��B
�>B
��B
��B
�rB
�>B
�rB
��B
�>B
��B
�rB
�	B
�lB
��B
��B
��B
��B
��B
�B
�B
��B
�B
�AB
�vB
�vB
�B
��B
�B
�B
�iB
�cB
�"B
�>B
� B
یB
֡B
�sB
�B
ںB
�B
�B
�AB
�.B�BB�B \B0UB1[B!bB�B�B#nB/OB3�B9�B@�BN�B`Bh>Bu%B�+B��B�xB�RB�FB��B�XB��B�KB�%B��B�B�B#:B,�B-�B-�B>BK)BM�BN�BR BU�BP}BQ�BQNBIBK�BF�BC-B,qB)�B&�B#B1B(B	�B��B��B��B҉B̘B�?B�LB�B�(B�ABg�BT�BFB:�B,qB�B7BB
�B
ҽB
��B
��B
�_B
w�B
jB
P�B
@�B
7�B
0�B
%�B

�B	�B	ȴB	�BB	�UB	�B	��B	�xB	zB	s�B	n�B	]�B	S�B	B[B	.�B	$�B	�B	�B	B	�B�B�B�B�B��B�B�,B�&B�B�B�B�;B��B��B�EB��B��B� B�gB�BیBޞB�BںB֡B��B҉B�WB�QB��B��B�B�B�B�TB�MB�TB�B�QB�BߤB�B�[B�vB�oB��B�JB�PB�AB�B��B�%B	�B	�B	uB	_B	�B	�B	�B	�B	FB	{B	�B	4B	hB	B	�B	�B	$tB	 �B	!B	�B	4B	�B	 4B��B�B�QB� B�9B� B��B�TBҽB��BیB��B�BB� B��B�B�	B��B		7B	�B	B	VB	GB��B�B�B��B�iB�B�
B�B�B��B�B�B�)B�oB�B��B�B�TB�2B��B�B�xB�VB	;B	�B	�B	YB	�B	�B	�B	(�B	,�B	1[B	5?B	7�B	?}B	C�B	C�B	GzB	I�B	QB	T�B	Y�B	[�B	]�B	bNB	d�B	iyB	ncB	qB	tB	wfB	zxB	{�B	~�B	�B	cB	�B	��B	�1B	�	B	�rB	�xB	��B	�xB	��B	��B	�B	�YB	�1B	��B	�B	�B	�-B	��B	�B	��B	��B	��B	��B	��B	�aB	��B	��B	��B	�XB	�XB	�B	�UB	�-B	ŢB	�B	��B	�EB	�B	�XB	��B	̘B	͟B	�vB	�}B	ѷB	ѷB	уB	уB	�B	��B	ӏB	�aB	��B	רB	ٴB	��B	�dB	ݘB	�;B	��B	�HB	�B	��B	�2B	�8B	�8B	�8B	�B	��B	�WB	��B	�B	� B	�cB	�/B	�/B	�/B	�B	�MB	�ZB	�%B	�ZB	�ZB	�8B	��B	�DB	�B	�"B	��B	�VB	�(B	��B	��B	�.B	��B
 �B
�B
uB
�B
�B
_B
1B
�B
�B
	B
�B

rB

	B
B
�B
"B
�B
�B
bB
hB
�B
B
uB
B
FB
{B
�B
B
B
MB
�B
�B
�B
�B
$B
�B
�B
MB
B
SB
�B
+B
�B
�B
7B
�B
�B
�B
�B
OB
�B
�B
�B
�B
!-B
"hB
"hB
"�B
"�B
#B
$@B
$B
$tB
$tB
%B
%FB
&�B
&�B
'RB
'�B
($B
(XB
(�B
(�B
*0B
*eB
*0B
*eB
*eB
*0B
+kB
+�B
,=B
,=B
,qB
,�B
,�B
-�B
-�B
-�B
-�B
-�B
-�B
.IB
.IB
.�B
.�B
/B
0!B
0!B
0UB
0�B
0�B
1�B
1�B
1�B
1�B
1�B
2-B
2-B
2�B
3hB
3�B
49B
49B
4nB
4�B
5B
5?B
5?B
5tB
5�B
5�B
6B
6FB
6zB
6zB
6zB
6�B
7LB
7�B
7�B
7�B
7�B
8�B
8RB
8B
8B
8B
7�B
9$B
9$B
9�B
:*B
9�B
9�B
:*B
:�B
;0B
;dB
;0B
<�B
<jB
=qB
=qB
=B
=�B
>B
=�B
>B
=�B
>BB
>wB
?B
?B
?}B
?�B
?�B
@B
@B
?�B
@B
@OB
?�B
@B
@OB
@B
@�B
B[B
B�B
B�B
C-B
C�B
C�B
C�B
D3B
D3B
D�B
D�B
D�B
EB
E9B
E�B
E�B
F?B
F�B
F�B
F�B
F�B
GB
GB
GzB
G�B
G�B
G�B
HB
HB
G�B
HB
G�B
HKB
HKB
I�B
K�B
MjB
M�B
NB
N<G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��B
��B
��B
��B
��B
�	B
�B
�B
�>B
��B
�	B
��B
��B
�B
�lB
��B
�B
�B
�B
�lB
�xB
��B
�JB
�	B
�JB
�fB
��B
��B
��B
�xB
�>B
��B
��B
�>B
�DB
�lB
��B
��B
�>B
��B
�	B
�B
��B
�8B
��B
��B
�B
�lB
��B
�B
�2B
��B
��B
��B
�>B
�8B
�>B
��B
��B
��B
��B
��B
�fB
�B
�rB
�fB
�fB
�TB
�lB
��B
�B
�`B
��B
�8B
��B
�2B
�ZB
��B
�TB
��B
��B
��B
��B
�B
��B
��B
�`B
�B
��B
�B
�`B
��B
�MB
��B
�MB
��B
�B
�%B
�B
�+B
�B
�MB
�B
�|B
��B
�vB
�B
��B
��B
�B
�B
��B
��B
��B
�GB
�B
�GB
�;B
�B
�oB
�B
��B
��B
�B
�AB
��B
�B
�oB
�|B
�oB
�|B
�oB
�B
�B
�GB
�B
�B
�B
�B
�B
�B
�B
�B
��B
�oB
�;B
�B
�iB
�B
�B
��B
�AB
�B
�B
�oB
�B
��B
� B
�B
�AB
��B
�;B
� B
��B
�B
� B
�;B
�B
�5B
��B
��B
��B
�/B
� B
�)B
��B
�B
�/B
�B
�B
�B
�B
�B
�QB
��B
�B
�8B
�,B
�B
�B
�fB
�`B
�B
��B
�NB
��B
ߤB
�B
�`B
ܒB
��B
��B
�
B
��B
�B
�?B
՛B
�sB
�2B
�9B
՛B
�mB
خB
�9B
�
B
�#B
�
B
�yB
��B
�B
ٴB
ٴB
�B
�QB
خB
ٴB
چB
�KB
�WB
�5B
ޞB
�vB
�B
� B
�B
�B
��B
�8B
�B
�>B
�B
�]B
�B
��B
�B
�B
�5B
�AB
�oB
��B
�B
�B
��B
��B
�.B�B 4B�B
��B�B	B�B�B�B�BxB�B�BJB"BxB B(B�B�B	lB�B�B
rBYBBBkB&�B&�B*0B,�B-wB,=B/B49B0!B0UB1[B0!B/�B2aB0�B1[B1�B0�B1�B7�B6zB�B \B�B�BBB�B�B�B \BB�B�BYB�B�B�B+B�BYB�B�B�B!bB#�B'�B,qB/�B1�B0UB,�B,qB.}B.�B.B5B<6B-�B)*B*�B3hB4�B@�B7LB8RB7B9�B<B9�B=�B;�B?B?�B<6B@�BCaBC�BE9BJ�BY�B7�B>BQ�Bb�B\]B\�B`vBa|B^5B`vBb�BaHBaBc�Bc Bd�BtBs�BjKBj�BxBu�ByrB|�BtBs�Br�BsMB|B�oB��B�=B��B��B�\B��B��B�bB��B� B�uB��B�IB��B��B��B��B�B��B�\B��B��B��B��B�kB�<B��B�3B�B��B��B�FB��B��B��B�-B�tBȀBƨB��B�?B��B��B˒B��B��B�NB�<B҉B�BخB֡B�yB�5B��B�`B�B��B�B��B�B�B��B��B��B�B��B��B�B��BB	�B  B 4BB	lBYB�BBB�B�B�B�B#:B 'B!�B#�B"�B$tB&�B%zB'�B)*B)*B*�B-wB>�B,�B-CB-CB5?B-B+�B33B,qB-�B,�B*�B,qB0!B.IB-B0�BK)BN�BMjBX�BF?BL0BI�BPBF?BT,BJXBJ�BH�BN<BJ�BLdBJXB[�BQ�BOBL0BL�BNBI�BO�BP�BO�BN�BO�BO�BMBP}BM�BN�BQNBL�BM�BS�BS[BXyBO�BX�BY�BV�BX�BVBYBU�BTaBV9BV9BVBT,BT,BT�BR�BP�BYBR�BQ�BPBM�BM�BO�BN�BN<BN<BN�BN�BL�BQ�BOBM�BN<BNpBL�B\�BZ�BR�BYBW?BZQBV�BS�B[�BO�BOBPHBR�BNpBJ�BK�BMBJXBK�BJ�BH�BI�BH�BHBEBFtBEBF?BGEBQ�BG�BK)BH�BG�BGEBK�BN�BK�BK�BOBBJ�BK)BI�BR BH�BIBC�BIRBMBJ�BCaBH�BH�BA�BF�BE9BE�BFBB[BG�BB'BH�BB'BA�B?�B>�BP}BRTB@�BQB^B/�B.IB,�B+B0!B-wB,�B+�B+B+B0UB+kB+�B-CB-�B+�B+B(�B(�B+B)�B+�B'�B(�B.�B4�B'RB&�B&�B)*B%�B%FB&LB(�B+�B$�B%�B&LB'�B%FB �B �B"4B%zB#�B �B!�B \B6B&�BB�B�B"�B!bB%�B�B+B$B$BSBB�B�BB�BoB�B�BbBVB�B�B\B"B~B~BB�BJB	�B�B�B�B+B1B�B%BMB�BDB
	B
=B	B��B iB��B�(B�B�B�B��B��B�B�>B�B�B�
B�,B��B��BߤB�;B��B�#BیB�/B�B�]BخBیB��B�yB�QB�TB�HB� B�B��B�vB�<B�B�jB�vB�jB�BΥB˒B��BϫB�#BȴBǮB�EBȴBɺBƨB�B�9B��B��B�RB�B��B�UB�BٴBҽ44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                    444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                    444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202302102309422023021023094220230210230942202302102309422023021023094220230210230942SI  SI  ARFMARFM                                                                                                                                                2022090511100320220905111003IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022091507032720220915070327QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1B83E           383E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022091507032720220915070327QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8800            800             SI  SI  ARFMARFM                                                                                                                                                2023021013194720230210131947IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI      ARSQ    SIQC    V3.1                                                                                                                                    20230210220014              CF      PRES                            D�~�G�O�D���G�O�@@  G�O�Valu passes DMQC                SI      ARSQ    SIQC    V3.1                                                                                                                                    20230210220014              CF      TEMP                            D�AHG�O�D���G�O�@�  G�O�Valu passes DMQC                SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                